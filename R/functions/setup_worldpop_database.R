library(RPostgres)
library(DBI)
library(terra)
library(sf)

# Load the constraint fix function
source("R/functions/fix_raster_constraints.R")

# Database setup and raster loading functions for WorldPop data

setup_postgres_database <- function(con) {
  
  cat("Setting up PostgreSQL database extensions for WorldPop data...\n")

  # Enable PostGIS extension
  tryCatch({
    dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgis")
    dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgis_raster") 
    dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgis_topology")
    cat("PostGIS extensions enabled successfully\n")
  }, error = function(e) {
    cat("Error enabling PostGIS extensions:", e$message, "\n")
    stop("PostGIS must be installed on your PostgreSQL server")
  })
  
  # Create metadata table for tracking country uploads
  tryCatch({
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS worldpop_countries (
        country_iso3 VARCHAR(3) PRIMARY KEY,
        filename VARCHAR(255),
        file_size_bytes BIGINT,
        tile_count INTEGER,
        upload_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        upload_status VARCHAR(20) DEFAULT 'completed'
      )
    ")
    cat("Metadata table worldpop_countries created/verified\n")
  }, error = function(e) {
    cat("Error creating metadata table:", e$message, "\n")
  })
  
  return(TRUE)
}

load_worldpop_to_postgres <- function(
  raster_file_path,
  con,
  table_name,
  tile_size,
  db_host,
  db_port,
  db_user,
  db_name,
  overview_factor = 2     # Create overviews for faster queries
) {
  
  cat("Loading WorldPop raster into PostgreSQL...\n")
  cat("This may take a while for global datasets (30+ minutes)\n")
  
  # Validate raster file exists
  if (!file.exists(raster_file_path)) {
    stop("Raster file not found: ", raster_file_path)
  }

  # Use raster2pgsql command line tool for efficient loading
  # This is much faster than loading through R for large rasters

  # Check if table exists and determine mode
  table_exists <- dbExistsTable(con, table_name)
  mode_flag <- if (table_exists) "-a" else "-c"  # append if exists, create if not
  
  cat("Table", table_name, if (table_exists) "exists - appending data" else "does not exist - creating new table", "\n")
  
  # Always remove potentially blocking constraints before upload
  if (dbExistsTable(con, table_name)) {
    cat("Removing any existing constraints that could block upload...\n")
    tryCatch({
      # Suppress PostgreSQL notices for cleaner output
      dbExecute(con, "SET client_min_messages TO WARNING")
      
      # Remove all PostGIS raster constraints that might block new country data
      constraint_names <- c(
        "enforce_max_extent_rast", "enforce_width_rast", "enforce_height_rast", 
        "enforce_same_alignment_rast", "enforce_scalex_rast", "enforce_scaley_rast",
        "enforce_srid_rast", "enforce_num_bands_rast", "enforce_pixel_types_rast", 
        "enforce_nodata_values_rast", "enforce_out_db_rast"
      )
      
      for (constraint_name in constraint_names) {
        dbExecute(con, sprintf("ALTER TABLE %s DROP CONSTRAINT IF EXISTS %s", table_name, constraint_name))
      }
      
      # Restore normal message level
      dbExecute(con, "SET client_min_messages TO NOTICE")
      
      cat("✓ All potentially blocking constraints removed\n")
    }, error = function(e) {
      cat("Warning: Could not remove constraints:", e$message, "\n")
    })
  }
  
  # Build raster2pgsql command using provided parameters
  # Don't use constraints (-C -M) since we're loading multiple countries with different extents
  cmd <- sprintf(
    'raster2pgsql -s 4326 -I %s -t %s %s %s | psql -h %s -p %s -U %s -d %s',
    mode_flag,           # Mode: -c (create) or -a (append)
    tile_size,           # Tile size
    shQuote(raster_file_path),  # Input raster (quoted for spaces)
    table_name,          # Table name
    db_host,
    db_port,
    db_user,
    db_name
  )
  
  cat("Running command:\n", cmd, "\n")

  
  # Run the command
  result <- system(cmd, intern = TRUE)
  
  if (length(result) == 0 || any(grepl("ERROR", result))) {
    stop("raster2pgsql failed. Check that PostgreSQL command line tools are installed.")
  }
  
  cat("Raster loaded successfully into table:", table_name, "\n")
  
  # Note: Constraints are not added automatically during uploads
  # Use fix_worldpop_constraints() manually or run after all uploads complete
  
  # Add country_iso3 column if it doesn't exist
  tryCatch({
    dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN IF NOT EXISTS country_iso3 VARCHAR(3)", table_name))
  }, error = function(e) {
    # Column might already exist, continue
  })
  
  # Extract country code from filename and update the uploaded rows
  filename <- basename(raster_file_path)
  country_iso3 <- toupper(substring(filename, 1, 3))  # First 3 characters, uppercase
  
  # Update the newly uploaded rows with country code
  # Get the range of RIDs that were just uploaded
  rows_updated <- dbExecute(con, sprintf(
    "UPDATE %s SET country_iso3 = '%s' WHERE country_iso3 IS NULL OR country_iso3 = ''",
    table_name, country_iso3
  ))
  
  cat("Updated", rows_updated, "rows with country code:", country_iso3, "\n")
  
  # Create additional indexes for performance
  cat("Creating spatial indexes...\n")
  
  # Spatial index on raster column
  dbExecute(con, sprintf(
    "CREATE INDEX IF NOT EXISTS %s_rast_gist_idx ON %s USING GIST (ST_ConvexHull(rast))",
    table_name, table_name
  ))
  
  # Update table statistics
  dbExecute(con, sprintf("ANALYZE %s", table_name))
  
  # Mark country as completely uploaded in metadata table
  file_size <- file.size(raster_file_path)
  filename <- basename(raster_file_path)
  
  # Insert or update metadata record
  dbExecute(con, sprintf("
    INSERT INTO worldpop_countries (country_iso3, filename, file_size_bytes, tile_count)
    VALUES ('%s', '%s', %d, %d)
    ON CONFLICT (country_iso3) DO UPDATE SET
      filename = EXCLUDED.filename,
      file_size_bytes = EXCLUDED.file_size_bytes,
      tile_count = EXCLUDED.tile_count,
      upload_date = CURRENT_TIMESTAMP,
      upload_status = 'completed'
  ", country_iso3, filename, file_size, rows_updated))
  
  cat("Country", country_iso3, "marked as completely uploaded\n")
  cat("Database setup complete!\n")
  
  return(TRUE)
}

# Check if a country is completely uploaded (uses metadata table)
country_exists_in_db <- function(con, country_iso3) {
  result <- dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as count FROM worldpop_countries 
     WHERE country_iso3 = '%s' AND upload_status = 'completed'",
    toupper(country_iso3)
  ))
  return(result$count > 0)
}

# Get list of countries already completely uploaded
get_existing_countries <- function(con) {
  result <- dbGetQuery(con, 
    "SELECT country_iso3 FROM worldpop_countries 
     WHERE upload_status = 'completed' 
     ORDER BY country_iso3"
  )
  return(result$country_iso3)
}

# Get upload statistics
get_upload_stats <- function(con) {
  result <- dbGetQuery(con,
    "SELECT 
       COUNT(*) as total_countries,
       SUM(file_size_bytes) as total_size_bytes,
       SUM(tile_count) as total_tiles,
       MIN(upload_date) as first_upload,
       MAX(upload_date) as latest_upload
     FROM worldpop_countries 
     WHERE upload_status = 'completed'"
  )
  return(result)
}

# Process a single country: check DB, check file, download if needed, upload
process_worldpop_country <- function(
  country_iso3,
  con,
  table_name,
  tile_size,
  db_host,
  db_port, 
  db_user,
  db_name,
  year = 2025,
  output_dir = "data/worldpop/",
  resolution = 100,
  version = "R2025A",
  dataset_version = "v1"
) {
  
  cat(sprintf("Processing country: %s\n", country_iso3))
  
  # 1. Clean up any partial/incomplete data for this country first
  if (dbExistsTable(con, table_name)) {
    # Check if country is marked as completed in metadata
    is_completed <- country_exists_in_db(con, country_iso3)
    
    if (!is_completed) {
      # Country is not marked as completed - clean up any partial data
      cat("  → Cleaning up any incomplete data for", country_iso3, "...\n")
      tryCatch({
        rows_deleted <- dbExecute(con, sprintf(
          "DELETE FROM %s WHERE country_iso3 = '%s' OR country_iso3 IS NULL", 
          table_name, toupper(country_iso3)
        ))
        if (rows_deleted > 0) {
          cat("  ✓ Removed", rows_deleted, "incomplete rows\n")
        }
      }, error = function(e) {
        cat("  Warning: Could not clean incomplete data:", e$message, "\n")
      })
    } else {
      # Country is already completed - skip entirely
      cat("  ✓ Already completed in database - skipping\n")
      return("skipped_db")
    }
  }
  
  # 2. Build local file path
  country_lower <- tolower(country_iso3)
  filename <- paste0(country_lower, "_pop_", year, "_CN_", resolution, "m_", version, "_", dataset_version, ".tif")
  local_file <- file.path(output_dir, filename)
  
  # 3. Check if TIF file exists locally
  if (file.exists(local_file) && file.size(local_file) > 1000) {
    cat("  ✓ TIF file exists locally - uploading to database\n")
    
    # Upload existing file
    tryCatch({
      load_worldpop_to_postgres(
        raster_file_path = local_file,
        con = con,
        table_name = table_name,
        tile_size = tile_size,
        db_host = db_host,
        db_port = db_port,
        db_user = db_user,
        db_name = db_name
      )
      cat("  ✓ Successfully uploaded", country_iso3, "to database\n")
      return("uploaded_existing")
    }, error = function(e) {
      cat("  ✗ Failed to upload", country_iso3, ":", e$message, "\n")
      return("failed_upload")
    })
    
  } else {
    cat("  → TIF file not found locally - downloading and uploading\n")
    
    # 4. Download the file
    # Build FTP URL
    ftp_base <- "ftp://ftp.worldpop.org/GIS/Population/Global_2015_2030"
    ftp_url <- paste0(
      ftp_base, "/", 
      version, "/", 
      year, "/", 
      country_iso3, "/", 
      dataset_version, "/", 
      resolution, "m/constrained/", 
      filename
    )
    
    # Create output directory
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    cat("  → Downloading from:", ftp_url, "\n")
    
    # Use simplified download (from existing function)
    download_success <- tryCatch({
      exit_code <- system2("curl", args = c(
        "--ftp-pasv",
        "--retry", "2",
        "--retry-delay", "3", 
        "--max-time", "300",
        "--connect-timeout", "30",
        "-L",
        "--create-dirs",
        "-o", shQuote(local_file),
        shQuote(ftp_url)
      ), stdout = FALSE, stderr = FALSE)
      
      exit_code == 0 && file.exists(local_file) && file.size(local_file) > 1000
    }, error = function(e) {
      FALSE
    })
    
    if (!download_success) {
      cat("  ✗ Failed to download", country_iso3, "\n")
      return("failed_download")
    }
    
    file_size_mb <- round(file.size(local_file) / 1024 / 1024, 1)
    cat("  ✓ Downloaded", country_iso3, ":", file_size_mb, "MB\n")
    
    # 5. Upload to database
    tryCatch({
      load_worldpop_to_postgres(
        raster_file_path = local_file,
        con = con,
        table_name = table_name,
        tile_size = tile_size,
        db_host = db_host,
        db_port = db_port,
        db_user = db_user,
        db_name = db_name
      )
      cat("  ✓ Successfully downloaded and uploaded", country_iso3, "\n")
      return("downloaded_uploaded")
    }, error = function(e) {
      cat("  ✗ Download succeeded but upload failed for", country_iso3, ":", e$message, "\n")
      return("failed_upload")
    })
  }
}

