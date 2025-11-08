library(RPostgres)
library(DBI)

# Database setup functions for WorldPop and global raster data

create_worldpop_database <- function(con, dataset_type = "population") {
  
  cat("Setting up PostgreSQL database for", dataset_type, "data...\n")

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
  
  # Create enhanced metadata table for tracking multiple dataset uploads
  tryCatch({
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS dataset_countries (
        country_iso3 VARCHAR(3),
        dataset_type VARCHAR(50),
        year INTEGER,
        version VARCHAR(20),
        resolution INTEGER,
        filename VARCHAR(255),
        table_name VARCHAR(255),
        file_size_bytes BIGINT,
        tile_count INTEGER,
        upload_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        upload_status VARCHAR(20) DEFAULT 'completed',
        PRIMARY KEY (country_iso3, dataset_type, year, version, resolution)
      )
    ")
    cat("Enhanced metadata table 'dataset_countries' created/verified\n")
  }, error = function(e) {
    cat("Error creating metadata table:", e$message, "\n")
  })
  
  # Create legacy worldpop_countries table for backward compatibility if it doesn't exist
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
    cat("Legacy metadata table 'worldpop_countries' created/verified for backward compatibility\n")
  }, error = function(e) {
    cat("Error creating legacy metadata table:", e$message, "\n")
  })
  
  return(TRUE)
}

# Check if a specific dataset for a country is completely uploaded
dataset_exists_in_db <- function(con, country_iso3, dataset_type, year, version = NULL, resolution = NULL) {
  
  # Build WHERE clause based on provided parameters
  where_clause <- sprintf(
    "country_iso3 = '%s' AND dataset_type = '%s' AND year = %d AND upload_status = 'completed'",
    toupper(country_iso3), dataset_type, year
  )
  
  if (!is.null(version)) {
    where_clause <- paste(where_clause, sprintf("AND version = '%s'", version))
  }
  
  if (!is.null(resolution)) {
    where_clause <- paste(where_clause, sprintf("AND resolution = %d", resolution))
  }
  
  result <- dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as count FROM dataset_countries WHERE %s",
    where_clause
  ))
  
  return(result$count > 0)
}

# Get list of countries with completed uploads for a specific dataset
get_existing_countries_for_dataset <- function(con, dataset_type, year, version = NULL, resolution = NULL) {
  
  # Build WHERE clause
  where_clause <- sprintf(
    "dataset_type = '%s' AND year = %d AND upload_status = 'completed'",
    dataset_type, year
  )
  
  if (!is.null(version)) {
    where_clause <- paste(where_clause, sprintf("AND version = '%s'", version))
  }
  
  if (!is.null(resolution)) {
    where_clause <- paste(where_clause, sprintf("AND resolution = %d", resolution))
  }
  
  result <- dbGetQuery(con, sprintf(
    "SELECT country_iso3 FROM dataset_countries 
     WHERE %s
     ORDER BY country_iso3",
    where_clause
  ))
  
  return(result$country_iso3)
}

# Legacy function for backward compatibility with existing worldpop_countries table
country_exists_in_db <- function(con, country_iso3) {
  result <- dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as count FROM worldpop_countries 
     WHERE country_iso3 = '%s' AND upload_status = 'completed'",
    toupper(country_iso3)
  ))
  return(result$count > 0)
}

# Legacy function for backward compatibility
get_existing_countries <- function(con) {
  result <- dbGetQuery(con, 
    "SELECT country_iso3 FROM worldpop_countries 
     WHERE upload_status = 'completed' 
     ORDER BY country_iso3"
  )
  return(result$country_iso3)
}

# Get upload statistics for all datasets
get_dataset_upload_stats <- function(con, dataset_type = NULL) {
  
  where_clause <- "upload_status = 'completed'"
  
  if (!is.null(dataset_type)) {
    where_clause <- paste(where_clause, sprintf("AND dataset_type = '%s'", dataset_type))
  }
  
  result <- dbGetQuery(con, sprintf(
    "SELECT 
       dataset_type,
       year,
       version,
       resolution,
       COUNT(*) as total_countries,
       SUM(file_size_bytes) as total_size_bytes,
       SUM(tile_count) as total_tiles,
       MIN(upload_date) as first_upload,
       MAX(upload_date) as latest_upload
     FROM dataset_countries 
     WHERE %s
     GROUP BY dataset_type, year, version, resolution
     ORDER BY dataset_type, year, version, resolution",
    where_clause
  ))
  
  return(result)
}

# Legacy function for backward compatibility
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