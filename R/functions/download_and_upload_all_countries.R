library(RPostgres)
library(DBI)

# Load required functions
source("R/functions/get_worldpop_global_data_ftp.R")
source("R/functions/create_worldpop_database.R")
source("R/functions/upload_worldpop_data.R")

# High-level function to download and upload all countries for a dataset
download_and_upload_all_countries <- function(
  con,
  dataset_type,
  year,
  version,
  resolution,
  table_name,
  tile_size,
  db_host,
  db_port,
  db_user,
  db_name,
  output_dir = "data/"
) {
  
  cat("=== Download and Upload All Countries ===\n")
  cat("Dataset:", dataset_type, "| Year:", year, "| Version:", version, "| Resolution:", resolution, "m\n")
  cat("Target table:", table_name, "\n\n")
  
  # Step 1: Get list of existing countries in database
  cat("Step 1: Checking existing countries in database...\n")
  existing_countries <- get_existing_countries_for_dataset(
    con, 
    dataset_type = dataset_type, 
    year = year, 
    version = version, 
    resolution = resolution
  )
  cat("Found", length(existing_countries), "countries already in database for", dataset_type, "dataset\n")
  if (length(existing_countries) > 0) {
    cat("Existing countries:", paste(head(existing_countries, 10), collapse = ", "))
    if (length(existing_countries) > 10) cat("...")
    cat("\n")
  }
  cat("\n")
  
  # Step 2: Get list of all available countries  
  cat("Step 2: Getting list of all available countries...\n")
  tryCatch({
    all_countries <- get_worldpop_global_data_ftp(
      year = year, 
      version = version, 
      resolution = resolution,
      dataset_type = dataset_type
    )
    # Remove Antarctica and any problematic countries
    all_countries <- all_countries[!all_countries %in% c("ATA", "ATF")]
    cat("✓ Found", length(all_countries), "countries available for", dataset_type, "dataset\n\n")
  }, error = function(e) {
    cat("✗ Error getting country list:", e$message, "\n")
    stop("Failed to get country list")
  })
  
  # Step 3: Process each country (check DB → check file → download → upload)
  cat("Step 3: Processing countries (check DB → check file → download → upload)...\n")
  cat("This may take 1-2 hours for new countries...\n\n")
  
  # Determine which countries need processing
  countries_to_process <- setdiff(all_countries, existing_countries)
  cat("Countries to process:", length(countries_to_process), "out of", length(all_countries), "total\n")
  
  if (length(countries_to_process) == 0) {
    cat("✓ All countries already in database - nothing to do!\n")
    return(list(
      total_countries = length(all_countries),
      skipped_db = length(existing_countries),
      uploaded_existing = 0,
      downloaded_uploaded = 0,
      failed_download = 0,
      failed_upload = 0
    ))
  } else {
    cat("Processing countries:", paste(head(countries_to_process, 10), collapse = ", "))
    if (length(countries_to_process) > 10) cat("...")
    cat("\n\n")
  }
  
  # Process each country
  results <- list(
    skipped_db = length(existing_countries),
    uploaded_existing = 0, 
    downloaded_uploaded = 0,
    failed_download = 0,
    failed_upload = 0
  )
  
  for (i in seq_along(countries_to_process)) {
    country <- countries_to_process[i]
    
    cat(sprintf("Processing %d/%d: %s\n", i, length(countries_to_process), country))
    
    result <- process_global_dataset_country(
      country_iso3 = country,
      con = con,
      table_name = table_name,
      tile_size = tile_size,
      db_host = db_host,
      db_port = db_port,
      db_user = db_user,
      db_name = db_name,
      dataset_type = dataset_type,
      year = year,
      version = version,
      resolution = resolution,
      output_dir = output_dir
    )
    
    # Track results
    results[[result]] <- results[[result]] + 1
    
    # Brief pause between countries
    if (i %% 5 == 0) {
      cat("  → Progress:", i, "/", length(countries_to_process), "- pausing 3 seconds...\n")
      Sys.sleep(3)
    }
    cat("\n")
  }
  
  # Step 4: Show final summary
  cat("=== Processing Summary ===\n")
  cat("Already in database:", results$skipped_db, "countries\n")
  cat("Uploaded existing files:", results$uploaded_existing, "countries\n")
  cat("Downloaded and uploaded:", results$downloaded_uploaded, "countries\n") 
  cat("Failed downloads:", results$failed_download, "countries\n")
  cat("Failed uploads:", results$failed_upload, "countries\n")
  
  total_success <- results$uploaded_existing + results$downloaded_uploaded
  cat("Total successful:", total_success, "countries\n")
  
  if (total_success == 0 && length(countries_to_process) > 0) {
    stop("No countries were successfully processed")
  }
  
  cat("✓ Download and upload process complete!\n\n")
  return(results)
}