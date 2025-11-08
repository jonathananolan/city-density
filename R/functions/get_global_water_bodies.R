

library(RPostgres)
library(DBI)
library(terra)
library(sf)
library(httr)

# Download and upload Global Surface Water rasters to PostGIS
# Based on the Global Surface Water dataset from Google/JRC
get_global_water_bodies <- function(
  con,
  destination_folder = "data/water_bodies/",
  dataset_name = "occurrence",  # or "change", "seasonality", "recurrence", etc.
  table_name = "global_water_bodies",
  tile_size = "100x100",
  recreate_table = FALSE,
  max_tiles = NULL,  # Limit for testing (NULL = download all)
  verbose = TRUE
) {
  
  if (verbose) cat("=== Downloading Global Surface Water Dataset ===\n")
  if (verbose) cat("Dataset:", dataset_name, "\n")
  if (verbose) cat("Destination:", destination_folder, "\n\n")
  
  # Create destination folder
  if (!dir.exists(destination_folder)) {
    if (verbose) cat("Creating folder:", destination_folder, "\n")
    dir.create(destination_folder, recursive = TRUE)
  }
  
  # Generate tile coordinates (matching the Python script logic)
  longs <- c(
    paste0(seq(180, 10, -10), "W"),  # 180W to 10W
    paste0(seq(0, 170, 10), "E")     # 0E to 170E
  )
  
  lats <- c(
    paste0(seq(50, 10, -10), "S"),   # 50S to 10S  
    paste0(seq(0, 80, 10), "N")      # 0N to 80N
  )
  
  # Calculate total tiles
  total_tiles <- length(longs) * length(lats)
  if (!is.null(max_tiles)) {
    total_tiles <- min(total_tiles, max_tiles)
  }
  
  if (verbose) cat("Total tiles to process:", total_tiles, "\n")
  
  # Create PostGIS table for water bodies
  if (recreate_table || !dbExistsTable(con, table_name)) {
    if (verbose) cat("Creating water bodies table in PostGIS...\n")
    
    if (dbExistsTable(con, table_name)) {
      dbExecute(con, sprintf("DROP TABLE %s CASCADE", table_name))
    }
    
    # Setup PostGIS extensions
    tryCatch({
      dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgis")
      dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgis_raster")
    }, error = function(e) {
      if (verbose) cat("PostGIS extensions already exist or error:", e$message, "\n")
    })
    
    if (verbose) cat("✓ PostGIS table prepared\n")
  }
  
  # Track successful downloads and uploads
  downloaded_files <- character(0)
  counter <- 1
  
  # Download and upload tiles
  for (lng in longs) {
    for (lat in lats) {
      
      # Check if we've hit the tile limit
      if (!is.null(max_tiles) && counter > max_tiles) {
        if (verbose) cat("Reached tile limit of", max_tiles, "- stopping\n")
        break
      }
      
      # Generate filename
      filename <- paste0(dataset_name, "_", lng, "_", lat, "v1_4_2021.tif")
      local_path <- file.path(destination_folder, filename)
      
      # Check if file already exists locally
      if (file.exists(local_path) && file.size(local_path) > 1000) {
        if (verbose) cat(sprintf("(%d/%d) %s already exists - uploading to PostGIS\n", 
                                counter, total_tiles, filename))
        
        # Upload existing file to PostGIS
        upload_success <- upload_water_tile_to_postgis(
          raster_path = local_path,
          con = con,
          table_name = table_name,
          tile_size = tile_size,
          verbose = verbose
        )
        
        if (upload_success) {
          downloaded_files <- c(downloaded_files, local_path)
        }
        
      } else {
        # Download the tile
        url <- paste0("http://storage.googleapis.com/global-surface-water/downloads2021/", 
                     dataset_name, "/", filename)
        
        if (verbose) cat(sprintf("(%d/%d) Downloading %s\n", counter, total_tiles, filename))
        
        # Check if URL exists and download
        tryCatch({
          response <- HEAD(url)
          if (status_code(response) == 200) {
            
            # Download file
            download_result <- tryCatch({
              download.file(url, local_path, mode = "wb", quiet = !verbose)
              TRUE
            }, error = function(e) {
              if (verbose) cat("  ✗ Download failed:", e$message, "\n")
              FALSE
            })
            
            if (download_result && file.exists(local_path) && file.size(local_path) > 1000) {
              if (verbose) cat("  ✓ Downloaded successfully\n")
              
              # Upload to PostGIS immediately
              upload_success <- upload_water_tile_to_postgis(
                raster_path = local_path,
                con = con,
                table_name = table_name,
                tile_size = tile_size,
                verbose = verbose
              )
              
              if (upload_success) {
                downloaded_files <- c(downloaded_files, local_path)
              }
              
            } else {
              if (verbose) cat("  ✗ Download failed or file too small\n")
              # Clean up failed download
              if (file.exists(local_path)) file.remove(local_path)
            }
            
          } else {
            if (verbose) cat("  ✗ File not found (HTTP", status_code(response), ")\n")
          }
          
        }, error = function(e) {
          if (verbose) cat("  ✗ Error checking URL:", e$message, "\n")
        })
      }
      
      counter <- counter + 1
      
      # Small delay to be nice to the server
      Sys.sleep(0.5)
    }
    
    # Break outer loop if we hit tile limit
    if (!is.null(max_tiles) && counter > max_tiles) break
  }
  
  # Summary
  if (verbose) {
    cat("\n=== Download and Upload Complete ===\n")
    cat("Files processed:", length(downloaded_files), "\n")
    cat("Destination folder:", destination_folder, "\n")
    
    if (length(downloaded_files) > 0) {
      total_size_mb <- sum(file.size(downloaded_files)) / 1024 / 1024
      cat("Total data size:", round(total_size_mb, 1), "MB\n")
      
      # PostGIS table stats
      tryCatch({
        stats <- dbGetQuery(con, sprintf("
          SELECT 
            COUNT(*) as tile_count,
            pg_size_pretty(pg_total_relation_size('%s')) as table_size
          FROM %s
        ", table_name, table_name))
        
        cat("PostGIS table stats:\n")
        cat("- Tiles in database:", stats$tile_count, "\n")
        cat("- Table size:", stats$table_size, "\n")
      }, error = function(e) {
        cat("Could not get PostGIS stats:", e$message, "\n")
      })
    }
  }
  
  return(downloaded_files)
}

# Helper function to upload a water raster tile to PostGIS
upload_water_tile_to_postgis <- function(
  raster_path,
  con, 
  table_name,
  tile_size = "100x100",
  verbose = TRUE
) {
  
  tryCatch({
    # Get database connection details from the connection
    con_info <- dbGetInfo(con)
    
    # Build raster2pgsql command for water bodies
    # Use -a (append) mode since table should already exist
    cmd <- sprintf(
      'raster2pgsql -s 4326 -I -a -t %s %s %s | psql -h %s -p %s -U %s -d %s > /dev/null 2>&1',
      tile_size,
      shQuote(raster_path),
      table_name,
      con_info$host,
      con_info$port, 
      con_info$username,
      con_info$dbname
    )
    
    # Execute command
    result <- system(cmd)
    
    if (result == 0) {
      if (verbose) cat("  ✓ Uploaded to PostGIS\n")
      return(TRUE)
    } else {
      if (verbose) cat("  ✗ PostGIS upload failed\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    if (verbose) cat("  ✗ Upload error:", e$message, "\n")
    return(FALSE)
  })
}

# Convenience wrapper function
download_water_bodies <- function(
  dataset_name = "occurrence",  # "occurrence", "change", "seasonality", "recurrence"  
  destination_folder = "data/water_bodies/",
  max_tiles = 10,  # For testing - set to NULL for full download
  db_name = "worldpop_db",
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  recreate_table = FALSE
) {
  
  # Connect to database
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = db_name,
    host = host,
    port = port,
    user = user,
    password = password
  )
  
  tryCatch({
    result <- get_global_water_bodies(
      con = con,
      destination_folder = destination_folder,
      dataset_name = dataset_name,
      table_name = "global_water_bodies",
      recreate_table = recreate_table,
      max_tiles = max_tiles,
      verbose = TRUE
    )
    
    return(result)
    
  }, finally = {
    dbDisconnect(con)
  })
}

# Legacy function for backwards compatibility
get_water_bodies <- function(){
#Water bodies are from an esri source - it was slightly quicker to convert to mollweide using qgis, but you could easily recreate the data yourself by going here. 
  
  #https://hub.arcgis.com/content/esri::world-water-bodies/about
  #or here...
  #https://global-surface-water.appspot.com/
  #https://collections.sentinel-hub.com/water-bodies/
  
  folder <- "input_data/water_bodies"
  filename <- "water_mol_simplified.shp"
  full_path <- paste0(folder,"/",filename)
  zip_file <- paste0(folder,"/water_bodies.zip")
  if (!dir.exists(folder)){  dir.create(folder,recursive = T) }
  
  if(!file.exists(full_path)){
    print("Downloading water bodies from AWS")
    download.file("https://city-density.s3.amazonaws.com/water_bodies/water_bodies.zip",zip_file)
    unzip(zipfile = zip_file,exdir = folder)
    read_sf(full_path)
  } else{
    read_sf(full_path)
  }
  
}
