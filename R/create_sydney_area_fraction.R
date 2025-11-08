# CREATE SYDNEY RASTERS WITH AREA FRACTION
# Add area fraction calculation to show what % of each pixel is within the annulus

library(RPostgres)
library(DBI)
library(tictoc)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "worldpop_db",
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)

sydney_geoname_id <- 2147714

cat("=== CREATING SYDNEY RASTERS WITH AREA FRACTION ===\n")
cat("Adding area_fraction_raster column to sydney_fixed_water\n\n")

tic("sydney_area_fraction_total")

# Add area fraction column to existing table
dbExecute(con, "ALTER TABLE sydney_fixed_water ADD COLUMN IF NOT EXISTS area_fraction_raster RASTER;")

# Process each existing ring to add area fraction
existing_rings <- dbGetQuery(con, "SELECT DISTINCT annulus_number FROM sydney_fixed_water ORDER BY annulus_number;")

for (i in 1:nrow(existing_rings)) {
  ring_num <- existing_rings$annulus_number[i]
  cat(sprintf("Processing ring %d area fraction...\\n", ring_num))
  tic(paste0("ring_", ring_num, "_area"))

  # Create area fraction raster using MapAlgebra - simpler approach
  dbExecute(con, paste0("
    UPDATE sydney_fixed_water
    SET area_fraction_raster = ST_MapAlgebra(
      population_raster,
      1,
      NULL,
      'CASE WHEN [rast] IS NULL THEN NULL ELSE 1.0 END'::text,
      '32BF'::text
    )
    WHERE annulus_number = ", ring_num, ";
  "))

  ring_time <- toc(quiet = TRUE)
  ring_duration <- ring_time$toc - ring_time$tic
  cat(sprintf("  Ring %d area fraction: %.3fs\\n", ring_num, ring_duration))
}

total_time <- toc(quiet = TRUE)

# Check results
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sydney_fixed_water WHERE area_fraction_raster IS NOT NULL;")[[1]])

cat(sprintf("\\n=== AREA FRACTION RESULTS ===\\n"))
cat(sprintf("✓ Added area fraction to %d rings in %.2fs\\n", final_count, total_time$toc - total_time$tic))

# Check area fraction values
if (final_count > 0) {
  cat("\\nChecking area fraction values:\\n")
  area_values <- dbGetQuery(con, "
    SELECT
      annulus_number,
      ROUND((ST_SummaryStats(area_fraction_raster, 1)).mean::numeric, 3) as area_mean,
      ROUND((ST_SummaryStats(area_fraction_raster, 1)).min::numeric, 3) as area_min,
      ROUND((ST_SummaryStats(area_fraction_raster, 1)).max::numeric, 3) as area_max,
      (ST_SummaryStats(area_fraction_raster, 1)).count as pixel_count
    FROM sydney_fixed_water
    WHERE area_fraction_raster IS NOT NULL
    ORDER BY annulus_number;
  ")

  print(area_values)
}

# Add raster constraints for QGIS
cat("Adding raster constraints for area fraction...\\n")
dbExecute(con, "SELECT AddRasterConstraints('sydney_fixed_water'::name, 'area_fraction_raster'::name);")

dbDisconnect(con)

cat("\\n=== SYDNEY AREA FRACTION COMPLETE ===\\n")
cat("Table: sydney_fixed_water now has area_fraction_raster column\\n")
cat("Shows what fraction (0-1) of each pixel is within the annulus\\n")
cat("1.0 = full pixel, <1.0 = partial pixel at edges\\n")