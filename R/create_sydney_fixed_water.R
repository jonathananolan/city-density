# CREATE SYDNEY RASTERS WITH FIXED WATER PIXEL TYPE
# Force water raster to use same pixel type as population (32BF)

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

cat("=== CREATING SYDNEY RASTERS WITH FIXED WATER TYPE ===\n")
cat("Forcing both rasters to use 32BF pixel type\n\n")

tic("sydney_fixed_total")

# Create table
dbExecute(con, "DROP TABLE IF EXISTS sydney_fixed_water;")
dbExecute(con, "
  CREATE TABLE sydney_fixed_water (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    annulus_number INTEGER,
    population_raster RASTER,
    water_raster RASTER,
    pixel_count INTEGER,
    creation_time_seconds DOUBLE PRECISION
  );
")

# Test with just first 3 rings
for (ring_num in 1:150) {
  cat(sprintf("Processing ring %d...\n", ring_num))
  tic(paste0("ring_", ring_num))

  dbExecute(con, "
    WITH ring_geom AS (
      SELECT annulus_geom
      FROM city_annulus
      WHERE geoname_id = $1 AND ring_number = $2
    ),

    -- Population raster (32BF)
    pop_raster AS (
      SELECT ST_Union(ST_Clip(p.rast, r.annulus_geom, true)) as pop_band
      FROM worldpop_2025 p, ring_geom r
      WHERE ST_Intersects(r.annulus_geom, p.rast)
        AND ST_Clip(p.rast, r.annulus_geom, true) IS NOT NULL
    ),

    -- Water raster - FORCE to 32BF pixel type
    water_raster AS (
      SELECT
        CASE
          WHEN COUNT(*) > 0 THEN
            ST_Reclass(
              ST_Union(ST_Clip(w.rast, r.annulus_geom, true)),
              1,
              '0-255:0-255',
              '32BF'::text,
              0
            )
          ELSE NULL
        END as water_band
      FROM water_pct w, ring_geom r
      WHERE ST_Intersects(r.annulus_geom, w.rast)
        AND ST_Clip(w.rast, r.annulus_geom, true) IS NOT NULL
    )

    INSERT INTO sydney_fixed_water (
      geoname_id, annulus_number, population_raster, water_raster, pixel_count
    )
    SELECT
      $1::BIGINT,
      $2,
      pr.pop_band,
      wr.water_band,
      (ST_SummaryStats(pr.pop_band)).count::integer
    FROM pop_raster pr, water_raster wr
    WHERE pr.pop_band IS NOT NULL;
  ", params = list(sydney_geoname_id, ring_num))

  ring_time <- toc(quiet = TRUE)
  ring_duration <- ring_time$toc - ring_time$tic

  # Update timing
  dbExecute(con, "
    UPDATE sydney_fixed_water
    SET creation_time_seconds = $1
    WHERE annulus_number = $2;
  ", params = list(ring_duration, ring_num))

  cat(sprintf("  Ring %d: %.3fs\n", ring_num, ring_duration))
}

total_time <- toc(quiet = TRUE)

# Check results
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sydney_fixed_water;")[[1]])

cat(sprintf("\n=== RESULTS ===\n"))
cat(sprintf("✓ Created %d rings in %.2fs\n", final_count, total_time$toc - total_time$tic))

# Check pixel types
if (final_count > 0) {
  cat("\nChecking raster constraints (pixel types):\n")
  constraints <- dbGetQuery(con, "
    SELECT
      r_raster_column,
      pixel_types,
      nodata_values
    FROM raster_columns
    WHERE r_table_name = 'sydney_fixed_water';
  ")
  print(constraints)

  # Check values
  values_check <- dbGetQuery(con, "
    SELECT
      annulus_number,
      ROUND((ST_SummaryStats(population_raster, 1)).mean::numeric, 2) as pop_mean,
      ROUND((ST_SummaryStats(water_raster, 1)).mean::numeric, 2) as water_mean,
      ROUND((ST_SummaryStats(water_raster, 1)).max::numeric, 2) as water_max
    FROM sydney_fixed_water
    ORDER BY annulus_number;
  ")

  cat("\nValue check:\n")
  print(values_check)
}

# Add raster constraints for QGIS
cat("Adding raster constraints for QGIS...\n")
dbExecute(con, "SELECT AddRasterConstraints('sydney_fixed_water'::name, 'population_raster'::name);")
dbExecute(con, "SELECT AddRasterConstraints('sydney_fixed_water'::name, 'water_raster'::name);")

dbDisconnect(con)

cat("\n=== SYDNEY FIXED WATER COMPLETE ===\n")
cat("Table: sydney_fixed_water ready for QGIS testing!\n")
cat("Both rasters should now use same pixel type (32BF)\n")