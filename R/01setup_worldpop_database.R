# WorldPop Database Setup Script
# This script downloads WorldPop global population data and loads it into PostgreSQL

# Load required libraries
library(RPostgres)
library(DBI)

# Ensure PATH includes PostgreSQL tools
Sys.setenv(PATH = paste("/opt/homebrew/bin:/opt/homebrew/opt/postgresql@17/bin", Sys.getenv("PATH"), sep = ":"))

# Load required functions
source("R/functions/get_worldpop_files.R")
source("R/functions/get_worldpop_countries_ftp.R")
source("R/functions/setup_worldpop_database.R")

# Configuration - All settings in one place
YEAR <- 2025
DB_NAME <- "worldpop_db"
TABLE_NAME <- paste0("worldpop_", YEAR)
DB_HOST <- "localhost"
DB_PORT <- 5432
DB_USER <- Sys.getenv("POSTGRES_USER", "abrey")
DB_PASSWORD <- Sys.getenv("POSTGRES_PASSWORD")
TILE_SIZE <- "100x100"


  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = DB_NAME,
    host = DB_HOST,
    port = DB_PORT,
    user = DB_USER,
    password = DB_PASSWORD
  )

# Step 1: Setup PostgreSQL database extensions
cat("Step 1: Setting up PostgreSQL database extensions...\n")
tryCatch({
  setup_postgres_database(con)
  cat("✓ Database setup complete\n\n")
}, error = function(e) {
  cat("✗ Error setting up database:", e$message, "\n")
  cat("Please ensure:\n")
  cat("1. PostgreSQL is installed and running\n")
  cat("2. PostGIS extension is available\n") 
  cat("3. Database credentials are correct\n")
  cat("4. raster2pgsql command line tool is installed\n\n")
  stop("Database setup failed")
})

# Step 2: Get list of existing countries in database
cat("Step 2: Checking existing countries in database...\n")
existing_countries <- get_existing_countries(con)
cat("Found", length(existing_countries), "countries already in database\n")
if (length(existing_countries) > 0) {
  cat("Existing countries:", paste(head(existing_countries, 10), collapse = ", "))
  if (length(existing_countries) > 10) cat("...")
  cat("\n")
}
cat("\n")

# Step 3: Get list of all available countries  
cat("Step 3: Getting list of all available countries...\n")
tryCatch({
  all_countries <- get_worldpop_countries_ftp(year = YEAR, version = "R2025A", resolution = 100)
  # Remove Antarctica and any problematic countries
  all_countries <- all_countries[!all_countries %in% c("ATA", "ATF")]
  cat("✓ Found", length(all_countries), "countries available from WorldPop\n\n")
}, error = function(e) {
  cat("✗ Error getting country list:", e$message, "\n")
  stop("Failed to get country list")
})

# Step 4: Process each country (check DB → check file → download → upload)
cat("Step 4: Processing countries (check DB → check file → download → upload)...\n")
cat("This may take 1-2 hours for new countries...\n\n")

# Determine which countries need processing
countries_to_process <- setdiff(all_countries, existing_countries)
cat("Countries to process:", length(countries_to_process), "out of", length(all_countries), "total\n")

if (length(countries_to_process) == 0) {
  cat("✓ All countries already in database - nothing to do!\n")
} else {
  cat("Processing countries:", paste(head(countries_to_process, 10), collapse = ", "))
  if (length(countries_to_process) > 10) cat("...")
  cat("\n\n")
}

# Process each country
results <- list(
  skipped_db = 0,
  uploaded_existing = 0, 
  downloaded_uploaded = 0,
  failed_download = 0,
  failed_upload = 0
)

for (i in seq_along(countries_to_process)) {
  country <- countries_to_process[i]
  
  cat(sprintf("Processing %d/%d: %s\n", i, length(countries_to_process), country))
  
  result <- process_worldpop_country(
    country_iso3 = country,
    con = con,
    table_name = TABLE_NAME,
    tile_size = TILE_SIZE,
    db_host = DB_HOST,
    db_port = DB_PORT,
    db_user = DB_USER,
    db_name = DB_NAME,
    year = YEAR
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

# Step 5: Show final summary
cat("=== Processing Summary ===\n")
cat("Uploaded existing files:", results$uploaded_existing, "countries\n")
cat("Downloaded and uploaded:", results$downloaded_uploaded, "countries\n") 
cat("Failed downloads:", results$failed_download, "countries\n")
cat("Failed uploads:", results$failed_upload, "countries\n")

total_success <- results$uploaded_existing + results$downloaded_uploaded
cat("Total successful:", total_success, "countries\n")

if (total_success == 0 && length(countries_to_process) > 0) {
  stop("No countries were successfully processed")
}

# Step 6: Test the setup
cat("\n=== Testing Database Setup ===\n")
test_result <- test_worldpop_database(con, TABLE_NAME)

if (test_result) {
  cat("✓ All tests passed!\n\n")
  
  cat("=== Setup Complete ===\n")
  cat("Your WorldPop database is ready for use.\n")
  cat("Database:", DB_NAME, "\n")
  cat("Table:", TABLE_NAME, "\n")
  cat("Connection object: con\n\n")
  
  cat("Example query for 10km buffer around a city:\n")
  cat("dbGetQuery(con, \"SELECT SUM(population) as total_pop \n")
  cat("                FROM", TABLE_NAME, "\n") 
  cat("                WHERE ST_DWithin(geom, \n")
  cat("                  ST_SetSRID(ST_MakePoint(lon, lat), 4326)::geography, 10000)\")\n\n")
  
} else {
  cat("✗ Some tests failed - check database setup\n")
}


    source("R/functions/fix_raster_constraints.R")
    fix_raster_constraints(con, TABLE_NAME, verbose = TRUE)
  