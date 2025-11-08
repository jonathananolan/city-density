# WorldPop Database Setup Script
# This script downloads WorldPop global population data and loads it into PostgreSQL

# Load required libraries
library(RPostgres)
library(DBI)
library(tidyverse)
# Load required functions
source("R/functions/create_worldpop_database.R")
source("R/functions/download_and_upload_all_countries.R")
source("R/functions/fix_raster_constraints.R")

# Configuration - All settings in one place
YEAR <- 2025
VERSION <- "R2025A"
RESOLUTION <- 100
DB_NAME <- "worldpop_db"
DB_HOST <- "localhost"
DB_PORT <- 5432
DB_USER <- Sys.getenv("POSTGRES_USER", "abrey")
DB_PASSWORD <- Sys.getenv("POSTGRES_PASSWORD")
TILE_SIZE <- "100x100"

# Dataset configurations
DATASETS <- list(
  water = list(
    type = "water", 
    year = YEAR,
    version = VERSION,
    table_name = "water_pct"
  ),
  population = list(
    type = "population",
    year = YEAR,
    version = VERSION,
    table_name = paste0("worldpop_", YEAR)
  )

)

# Establish database connection
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = DB_NAME,
  host = DB_HOST,
  port = DB_PORT,
  user = DB_USER,
  password = DB_PASSWORD
)


  create_worldpop_database(con, dataset_type = "multi")

for (dataset_name in names(DATASETS)) {
  dataset <- DATASETS[[dataset_name]]
  
    results <- download_and_upload_all_countries(
      con = con,
      dataset_type = dataset$type,
      year = dataset$year,
      version = dataset$version,
      resolution = RESOLUTION,
      table_name = dataset$table_name,
      tile_size = TILE_SIZE,
      db_host = DB_HOST,
      db_port = DB_PORT,
      db_user = DB_USER,
      db_name = DB_NAME,
      output_dir = "data/"
    )

    fix_raster_constraints(con, dataset$table_name, verbose = TRUE)

}

# Check for duplicates in worldwide tables
cat("Checking for spatial overlaps and duplicate countries...\n")

for (dataset_name in names(DATASETS)) {
  dataset <- DATASETS[[dataset_name]]
  table_name <- dataset$table_name

  cat(sprintf("Checking table: %s\n", table_name))

  # Check 1: Find spatial overlaps between tiles
  cat("  Checking for spatial overlaps...\n")
  overlap_query <- sprintf("
    WITH overlaps AS (
      SELECT
        t1.rid as rid1,
        t2.rid as rid2,
        t1.country_iso3 as country1,
        t2.country_iso3 as country2,
        ST_Area(ST_Intersection(t1.rast::geometry, t2.rast::geometry)) as overlap_area
      FROM %s t1
      JOIN %s t2 ON t1.rid < t2.rid
      WHERE ST_Intersects(t1.rast, t2.rast)
    )
    SELECT
      country1, country2,
      COUNT(*) as overlapping_pairs,
      SUM(overlap_area) as total_overlap_area
    FROM overlaps
    GROUP BY country1, country2
    ORDER BY total_overlap_area DESC
  ", table_name, table_name)

  overlaps <- tryCatch({
    dbGetQuery(con, overlap_query)
  }, error = function(e) {
    cat("    Could not check spatial overlaps:", e$message, "\n")
    data.frame()
  })

  if (nrow(overlaps) > 0) {
    cat(sprintf("    WARNING: Found spatial overlaps between countries in %s:\n", table_name))
    print(overlaps)
  } else {
    cat(sprintf("    ✓ No spatial overlaps found in %s\n", table_name))
  }

  # Check 2: Find duplicate countries and clean them up
  cat("  Checking for duplicate countries...\n")
  country_query <- sprintf("
    SELECT
      country_iso3,
      COUNT(*) as tile_count,
      MIN(rid) as first_rid,
      MAX(rid) as last_rid
    FROM %s
    WHERE country_iso3 IS NOT NULL
    GROUP BY country_iso3
    ORDER BY country_iso3
  ", table_name)

  countries <- tryCatch({
    dbGetQuery(con, country_query)
  }, error = function(e) {
    cat("    Could not check countries:", e$message, "\n")
    data.frame()
  })

  if (nrow(countries) > 0) {
    # Check metadata to see which countries should be complete
    metadata_query <- sprintf("
      SELECT country_iso3, upload_status, tile_count as expected_tiles
      FROM dataset_countries
      WHERE dataset_type = '%s' AND year = %d AND version = '%s'
      ORDER BY country_iso3
    ", dataset$type, dataset$year, dataset$version)

    expected <- tryCatch({
      dbGetQuery(con, metadata_query)
    }, error = function(e) {
      data.frame()
    })

    if (nrow(expected) > 0) {
      # Compare actual vs expected tile counts
      comparison <- merge(countries, expected, by = "country_iso3", all.x = TRUE, suffixes = c("_actual", "_expected"))

      issues <- comparison[
        is.na(comparison$upload_status) |
        comparison$upload_status != 'completed' |
        (!is.na(comparison$expected_tiles) & comparison$tile_count != comparison$expected_tiles),
      ]

      if (nrow(issues) > 0) {
        cat("    WARNING: Found countries with incomplete/duplicate data:\n")
        print(issues[, c("country_iso3", "tile_count_actual", "expected_tiles", "upload_status")])

        # Offer to clean up problematic countries
        cat("\n    To clean up these countries, you can run:\n")
        for (i in 1:nrow(issues)) {
          country <- issues$country_iso3[i]
          cat(sprintf("    DELETE FROM %s WHERE country_iso3 = '%s';\n", table_name, country))
        }
        cat("    Then re-run the upload process for these countries.\n")
      } else {
        cat(sprintf("    ✓ All %d countries appear complete in %s\n", nrow(countries), table_name))
      }
    } else {
      cat(sprintf("    Found %d countries in %s (no metadata to compare against)\n", nrow(countries), table_name))
    }
  }

  cat("\n")
}
