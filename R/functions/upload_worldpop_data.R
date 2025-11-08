library(RPostgres)
library(DBI)
library(glue)

# Load required functions
source("R/functions/create_worldpop_database.R")

# Upload functions for WorldPop and global raster data

load_global_raster_to_postgres <- function(
  raster_file_path,
  con,
  table_name,
  tile_size,
  db_host,
  db_port,
  db_user,
  db_name,
  dataset_type = "population",
  country_iso3 = NULL,
  year = NULL,
  version = NULL,
  resolution = NULL,
  overview_factor = 2
) {
  
  cat("Loading", dataset_type, "raster into PostgreSQL table:", table_name, "...\n")
  cat("This may take a while for global datasets (30+ minutes)\n")
  
  # Validate raster file exists
  if (!file.exists(raster_file_path)) {
    stop("Raster file not found: ", raster_file_path)
  }

  # Extract country code from filename if not provided
  if (is.null(country_iso3)) {
    filename <- basename(raster_file_path)
    country_iso3 <- toupper(substring(filename, 1, 3))
  }
  country_iso3 <- toupper(country_iso3)
  
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
  
  # Add country_iso3 column if it doesn't exist
  tryCatch({
    dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN IF NOT EXISTS country_iso3 VARCHAR(3)", table_name))
  }, error = function(e) {
    # Column might already exist, continue
  })
  
  # Update the newly uploaded rows with country code
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
  
  # Mark dataset as completely uploaded in enhanced metadata table
  file_size <- file.size(raster_file_path)
  filename <- basename(raster_file_path)
  
  # Insert or update metadata record in dataset_countries table
  if (!is.null(year) && !is.null(version) && !is.null(resolution)) {
    dbExecute(con, sprintf("
      INSERT INTO dataset_countries 
        (country_iso3, dataset_type, year, version, resolution, filename, table_name, file_size_bytes, tile_count)
      VALUES ('%s', '%s', %d, '%s', %d, '%s', '%s', %d, %d)
      ON CONFLICT (country_iso3, dataset_type, year, version, resolution) 
      DO UPDATE SET
        filename = EXCLUDED.filename,
        table_name = EXCLUDED.table_name,
        file_size_bytes = EXCLUDED.file_size_bytes,
        tile_count = EXCLUDED.tile_count,
        upload_date = CURRENT_TIMESTAMP,
        upload_status = 'completed'
    ", country_iso3, dataset_type, year, version, resolution, filename, table_name, file_size, rows_updated))
    
    cat("Country", country_iso3, "marked as completely uploaded for", dataset_type, "dataset\n")
  }
  
  # Also update legacy worldpop_countries table for population data (backward compatibility)
  if (dataset_type == "population") {
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
  }
  
  cat("Database upload complete!\n")
  return(TRUE)
}

# Process a single country for any dataset type
process_global_dataset_country <- function(
  country_iso3,
  con,
  table_name,
  tile_size,
  db_host,
  db_port, 
  db_user,
  db_name,
  dataset_type = "population",
  year = 2025,
  version = "R2025A",
  resolution = 100,
  output_dir = "data/"
) {
  
  cat(sprintf("Processing country: %s for %s dataset\n", country_iso3, dataset_type))
  
  # Load download function
  source("R/functions/download_dataset_file.R")
  
  # 1. Clean up any partial/incomplete data for this country first
  if (dbExistsTable(con, table_name)) {
    # Check if country is marked as completed in metadata
    is_completed <- dataset_exists_in_db(con, country_iso3, dataset_type, year, version, resolution)
    
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
  
  # 2. Download/get the file (handles all dataset-specific logic)
  tryCatch({
    local_file <- download_dataset_file(
      country_iso3 = country_iso3,
      dataset_type = dataset_type,
      year = year,
      version = version,
      resolution = resolution,
      output_dir = output_dir
    )
  }, error = function(e) {
    cat("  ✗ Failed to obtain", country_iso3, "data:", e$message, "\n")
    return("failed_download")
  })
  
  # 3. Upload to database
  tryCatch({
    load_global_raster_to_postgres(
      raster_file_path = local_file,
      con = con,
      table_name = table_name,
      tile_size = tile_size,
      db_host = db_host,
      db_port = db_port,
      db_user = db_user,
      db_name = db_name,
      dataset_type = dataset_type,
      country_iso3 = country_iso3,
      year = year,
      version = version,
      resolution = resolution
    )
    
    cat("  ✓ Successfully uploaded", country_iso3, "to database\n")
    
    # Return appropriate status
    return(if (file.exists(local_file)) "downloaded_uploaded" else "uploaded_existing")
    
  }, error = function(e) {
    cat("  ✗ Upload failed for", country_iso3, ":", e$message, "\n")
    return("failed_upload")
  })
}