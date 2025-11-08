# CREATE SYDNEY 3 RASTER COLUMNS - Full 150 rings
# population_raster, water_raster, area_fraction_raster as separate columns

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

sydney_geoname_id <- 2147714  # Sydney
max_rings <- 150

cat("=== CREATING SYDNEY 3 RASTER COLUMNS TABLE ===\n")
cat("Columns: population_raster, water_raster, area_fraction_raster\n")
cat(sprintf("Processing all %d rings for Sydney\n\n", max_rings))

tic("sydney_3_columns_total")

# Create the table
dbExecute(con, "DROP TABLE IF EXISTS sydney_3_raster_columns;")
dbExecute(con, "
  CREATE TABLE sydney_3_raster_columns (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    annulus_number INTEGER,
    population_raster RASTER,
    water_raster RASTER,
    area_fraction_raster RASTER,
    pixel_count INTEGER,
    creation_time_seconds DOUBLE PRECISION,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")

cat("Processing all 150 Sydney rings...\n")

total_ring_time <- 0
rings_processed <- 0

# Process each ring
for (ring_num in 1:max_rings) {
  tic(paste0("sydney_ring_", ring_num))

  # Check if this ring exists for Sydney
  ring_exists <- dbGetQuery(con, "
    SELECT COUNT(*) as count
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number = $2;
  ", params = list(sydney_geoname_id, ring_num))

  if (ring_exists$count > 0) {
    # Create all 3 rasters for this ring
    dbExecute(con, "
      WITH ring_geom AS (
        SELECT annulus_geom
        FROM city_annulus
        WHERE geoname_id = $1 AND ring_number = $2
      ),

      -- Population raster (same as current fast approach)
      pop_raster AS (
        SELECT ST_Union(ST_Clip(p.rast, r.annulus_geom, true)) as pop_band
        FROM worldpop_2025 p, ring_geom r
        WHERE ST_Intersects(r.annulus_geom, p.rast)
          AND ST_Clip(p.rast, r.annulus_geom, true) IS NOT NULL
      ),

      -- Water raster (clipped to same area)
      water_raster AS (
        SELECT
          CASE
            WHEN COUNT(*) > 0 THEN ST_Union(ST_Clip(w.rast, r.annulus_geom, true))
            ELSE NULL
          END as water_band
        FROM water_pct w, ring_geom r
        WHERE ST_Intersects(r.annulus_geom, w.rast)
          AND ST_Clip(w.rast, r.annulus_geom, true) IS NOT NULL
      ),

      -- Area fraction raster (simplified to 1.0 for now - can be enhanced later)
      area_fraction_raster AS (
        SELECT
          -- Create raster with same extent as population, filled with 1.0
          ST_MapAlgebra(
            pr.pop_band,
            1,
            NULL,
            'CASE WHEN [rast] IS NULL THEN NULL ELSE 1.0 END'::text,
            '32BF'::text
          ) as area_band
        FROM pop_raster pr
      )

      INSERT INTO sydney_3_raster_columns (
        geoname_id, annulus_number, population_raster, water_raster,
        area_fraction_raster, pixel_count
      )
      SELECT
        $1::BIGINT,
        $2,
        pr.pop_band,
        wr.water_band,
        afr.area_band,
        (ST_SummaryStats(pr.pop_band)).count::integer
      FROM pop_raster pr, water_raster wr, area_fraction_raster afr
      WHERE pr.pop_band IS NOT NULL;
    ", params = list(sydney_geoname_id, ring_num))

    rings_processed <- rings_processed + 1
  }

  ring_time <- toc(quiet = TRUE)
  ring_duration <- ring_time$toc - ring_time$tic
  total_ring_time <- total_ring_time + ring_duration

  # Update timing if ring was processed
  if (ring_exists$count > 0) {
    dbExecute(con, "
      UPDATE sydney_3_raster_columns
      SET creation_time_seconds = $1
      WHERE annulus_number = $2;
    ", params = list(ring_duration, ring_num))
  }

  # Progress reporting
  if (ring_num %% 20 == 0) {
    avg_time <- total_ring_time / ring_num
    remaining_rings <- max_rings - ring_num
    estimated_remaining <- remaining_rings * avg_time
    cat(sprintf("✓ Processed %d/%d rings (%d created). Avg: %.3fs/ring. Est remaining: %.1fm\n",
                ring_num, max_rings, rings_processed, avg_time, estimated_remaining/60))
  }
}

sydney_total_time <- toc(quiet = TRUE)

# Get final results
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sydney_3_raster_columns;")[[1]])
table_size <- dbGetQuery(con, "SELECT pg_size_pretty(pg_total_relation_size('sydney_3_raster_columns')) as size;")[[1]]

# Summary statistics
avg_time_per_ring <- total_ring_time / max_rings
avg_time_per_created_ring <- if(rings_processed > 0) total_ring_time / rings_processed else 0

cat("\n=== SYDNEY 3 RASTER COLUMNS RESULTS ===\n")
cat(sprintf("✓ Total time: %.2f minutes (%.1fs)\n", (sydney_total_time$toc - sydney_total_time$tic)/60, sydney_total_time$toc - sydney_total_time$tic))
cat(sprintf("✓ Rings processed: %d/%d\n", rings_processed, max_rings))
cat(sprintf("✓ Rings created: %d\n", final_count))
cat(sprintf("✓ Avg time per ring checked: %.3fs\n", avg_time_per_ring))
cat(sprintf("✓ Avg time per ring created: %.3fs\n", avg_time_per_created_ring))
cat(sprintf("✓ Storage: %s\n", table_size))

# Sample data check
if (final_count > 0) {
  sample_info <- dbGetQuery(con, "
    SELECT
      annulus_number,
      population_raster IS NOT NULL as has_pop,
      water_raster IS NOT NULL as has_water,
      area_fraction_raster IS NOT NULL as has_area,
      ST_Width(population_raster) as width,
      ST_Height(population_raster) as height,
      pixel_count,
      ROUND(creation_time_seconds::numeric, 3) as time_sec
    FROM sydney_3_raster_columns
    WHERE annulus_number <= 5
    ORDER BY annulus_number;
  ")

  cat("\nFirst 5 rings structure check:\n")
  print(sample_info)

  # Check raster values
  value_check <- dbGetQuery(con, "
    SELECT
      annulus_number,
      ROUND((ST_SummaryStats(population_raster, 1)).mean::numeric, 2) as pop_mean,
      CASE
        WHEN water_raster IS NOT NULL THEN
          ROUND((ST_SummaryStats(water_raster, 1)).mean::numeric, 2)
        ELSE NULL
      END as water_mean,
      ROUND((ST_SummaryStats(area_fraction_raster, 1)).mean::numeric, 3) as area_mean
    FROM sydney_3_raster_columns
    WHERE annulus_number <= 3
    ORDER BY annulus_number;
  ")

  cat("\nSample raster values (first 3 rings):\n")
  print(value_check)
}

# Add raster constraints for QGIS
cat("Adding raster constraints for QGIS...\n")
dbExecute(con, "SELECT AddRasterConstraints('sydney_3_raster_columns'::name, 'population_raster'::name);")
dbExecute(con, "SELECT AddRasterConstraints('sydney_3_raster_columns'::name, 'water_raster'::name);")
dbExecute(con, "SELECT AddRasterConstraints('sydney_3_raster_columns'::name, 'area_fraction_raster'::name);")

dbDisconnect(con)

cat("\n=== SYDNEY 3 RASTER COLUMNS COMPLETE ===\n")
cat("Table: sydney_3_raster_columns ready for QGIS!\n")
cat("Columns available:\n")
cat("- population_raster: Population density per pixel\n")
cat("- water_raster: Water percentage per pixel\n")
cat("- area_fraction_raster: Area fraction (1.0 = full pixel)\n")
cat("\nIn QGIS: Add each raster column as separate layer for analysis\n")