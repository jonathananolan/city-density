# Parallel City Annuli Processing - Optimized for M2 Mac
# This script processes multiple cities in parallel using R's parallel package

# Load required libraries
library(RPostgres)
library(DBI)
library(tidyverse)
library(parallel)

# Database configuration
DB_NAME <- "worldpop_db"
DB_HOST <- "localhost"
DB_PORT <- 5432
DB_USER <- Sys.getenv("POSTGRES_USER", "abrey")
DB_PASSWORD <- Sys.getenv("POSTGRES_PASSWORD")

# M2 Mac optimization - detect cores and set optimal thread count
detect_optimal_cores <- function() {
  total_cores <- parallel::detectCores()
  cat(sprintf("Detected %d cores\n", total_cores))

  # For M2 Mac: use 75% of cores, leave some for PostgreSQL and system
  optimal_cores <- max(1, floor(total_cores * 0.75))
  optimal_cores <- min(optimal_cores, 6) # Cap at 6 for memory efficiency

  cat(sprintf("Using %d parallel threads\n", optimal_cores))
  return(optimal_cores)
}

# Function to create database connection (used by each worker)
create_connection <- function() {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = DB_NAME,
    host = DB_HOST,
    port = DB_PORT,
    user = DB_USER,
    password = DB_PASSWORD
  )
  return(con)
}

# Import all functions from the original script (without main execution)
source_functions <- function() {
  # Read the original script and extract functions
  script_content <- readLines("R/02_join_pop_with_cities.R")

  # Find function definitions (exclude main execution part)
  function_start <- which(grepl("^create_tables <- function", script_content))
  main_execution <- which(grepl("^# Main execution", script_content))

  if (length(function_start) > 0 && length(main_execution) > 0) {
    function_lines <- script_content[function_start:(main_execution - 1)]
    eval(parse(text = paste(function_lines, collapse = "\n")), envir = .GlobalEnv)
  }
}

# Load functions from original script
source_functions()

# Worker function to process a single city
process_city_worker <- function(city_info, max_radius_km = 150) {
  # Create connection for this worker
  con <- create_connection()

  tryCatch({
    # Process the city
    pixels_processed <- process_city(
      con,
      city_info$geoname_id,
      city_info$city_name,
      max_radius_km
    )

    # Return result
    result <- list(
      geoname_id = city_info$geoname_id,
      city_name = city_info$city_name,
      pixels_processed = pixels_processed,
      status = "success",
      worker_id = Sys.getpid()
    )

    return(result)

  }, error = function(e) {
    return(list(
      geoname_id = city_info$geoname_id,
      city_name = city_info$city_name,
      pixels_processed = 0,
      status = "error",
      error_message = as.character(e),
      worker_id = Sys.getpid()
    ))
  }, finally = {
    # Always close connection
    if (exists("con") && !is.null(con)) {
      dbDisconnect(con)
    }
  })
}

# Function to get cities that need processing
get_cities_to_process <- function(con, max_radius_km, limit_cities = NULL) {
  # Get all cities
  all_cities_query <- "
    SELECT DISTINCT geoname_id, city_name
    FROM city_annulus
    ORDER BY geoname_id
  "

  if (!is.null(limit_cities)) {
    all_cities_query <- paste(all_cities_query, "LIMIT", limit_cities)
  }

  all_cities <- dbGetQuery(con, all_cities_query)

  # Check which cities are already complete
  completed_cities_query <- "
    SELECT DISTINCT pc.geoname_id
    FROM processing_completion pc
    WHERE pc.max_radius_km = $1
      AND pc.ring_number = $1
  "

  completed_cities <- dbGetQuery(con, completed_cities_query, params = list(max_radius_km))

  # Filter out completed cities
  remaining_cities <- all_cities[!all_cities$geoname_id %in% completed_cities$geoname_id, ]

  cat(sprintf("Found %d cities total, %d already complete, %d remaining\n",
              nrow(all_cities), nrow(completed_cities), nrow(remaining_cities)))

  return(remaining_cities)
}

# Main parallel processing function
process_cities_parallel <- function(max_radius_km = 150, limit_cities = NULL,
                                  batch_size = NULL) {

  cat("=== Parallel City Processing ===\n\n")

  # Detect optimal number of cores
  num_cores <- detect_optimal_cores()

  # Create main connection for coordination
  main_con <- create_connection()

  # Ensure tables exist
  create_tables(main_con)

  # Get cities that need processing
  cities_to_process <- get_cities_to_process(main_con, max_radius_km, limit_cities)

  if (nrow(cities_to_process) == 0) {
    cat("✓ All cities already processed!\n")
    dbDisconnect(main_con)
    return(0)
  }

  # Set batch size if not specified (optimal for memory management)
  if (is.null(batch_size)) {
    batch_size <- num_cores * 2  # Process 2 batches per core
  }

  total_processed <- 0
  start_time <- Sys.time()

  # Process cities in batches to manage memory
  for (batch_start in seq(1, nrow(cities_to_process), batch_size)) {
    batch_end <- min(batch_start + batch_size - 1, nrow(cities_to_process))
    batch_cities <- cities_to_process[batch_start:batch_end, ]

    cat(sprintf("\nProcessing batch %d-%d of %d cities...\n",
                batch_start, batch_end, nrow(cities_to_process)))

    batch_start_time <- Sys.time()

    # Create cluster for parallel processing
    cl <- makeCluster(num_cores, type = "PSOCK")

    tryCatch({
      # Export necessary variables to workers
      clusterExport(cl, c("DB_NAME", "DB_HOST", "DB_PORT", "DB_USER", "DB_PASSWORD",
                         "create_connection", "process_city", "get_resumption_point",
                         "cleanup_partial_city", "process_city_rings_batch",
                         "max_radius_km"))

      # Load required packages on each worker
      clusterEvalQ(cl, {
        library(RPostgres)
        library(DBI)
        library(tidyverse)
      })

      # Process cities in parallel
      results <- parLapply(cl, split(batch_cities, seq_len(nrow(batch_cities))),
                          function(city_row) {
                            process_city_worker(city_row, max_radius_km)
                          })

    }, finally = {
      # Always stop cluster
      stopCluster(cl)
    })

    # Process results
    batch_pixels <- 0
    successes <- 0
    errors <- 0

    for (result in results) {
      if (result$status == "success") {
        batch_pixels <- batch_pixels + result$pixels_processed
        successes <- successes + 1
      } else {
        errors <- errors + 1
        cat(sprintf("ERROR processing %s (ID: %s): %s\n",
                   result$city_name, result$geoname_id, result$error_message))
      }
    }

    total_processed <- total_processed + batch_pixels
    batch_duration <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))
    total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    cat(sprintf("Batch completed: %d pixels, %d successes, %d errors in %.1fs\n",
                batch_pixels, successes, errors, batch_duration))
    cat(sprintf("Total progress: %d pixels in %.1fs (%.0f pixels/sec)\n",
                total_processed, total_duration,
                if(total_duration > 0) total_processed/total_duration else 0))
  }

  # Final summary
  final_duration <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  cat(sprintf("\n✓ Parallel processing complete!\n"))
  cat(sprintf("Total: %d pixels in %.1f minutes\n", total_processed, final_duration))
  cat(sprintf("Average: %.0f pixels/minute\n",
              if(final_duration > 0) total_processed/final_duration else 0))

  dbDisconnect(main_con)
  return(total_processed)
}

# Example usage and testing
if (interactive()) {
  cat("Parallel processing script loaded.\n")
  cat("To run:\n")
  cat("  # Test with 5 cities: process_cities_parallel(max_radius_km = 10, limit_cities = 5)\n")
  cat("  # Full production: process_cities_parallel(max_radius_km = 150)\n")
} else {
  # If run as script, do a small test
  process_cities_parallel(max_radius_km = 10, limit_cities = 3)
}