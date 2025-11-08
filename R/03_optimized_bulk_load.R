# Optimized Bulk Loading - Drop indexes during INSERT, recreate after
# This should dramatically improve INSERT performance

library(RPostgres)
library(DBI)
library(tidyverse)

# Database configuration
DB_NAME <- "worldpop_db"
DB_HOST <- "localhost"
DB_PORT <- 5432
DB_USER <- Sys.getenv("POSTGRES_USER", "abrey")
DB_PASSWORD <- Sys.getenv("POSTGRES_PASSWORD")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = DB_NAME,
  host = DB_HOST,
  port = DB_PORT,
  user = DB_USER,
  password = DB_PASSWORD
)

# PostgreSQL optimizations
dbExecute(con, "SET max_parallel_workers_per_gather = 4;")
dbExecute(con, "SET work_mem = '512MB';")

# Function to drop spatial indexes for bulk loading
drop_spatial_indexes <- function(con) {
  cat("Dropping spatial indexes for bulk loading...\n")

  # Drop the heavy spatial indexes
  dbExecute(con, "DROP INDEX IF EXISTS annulus_pixels_center_idx;")
  dbExecute(con, "DROP INDEX IF EXISTS annulus_pixels_geom_idx;")

  # Keep essential indexes for conflict resolution
  # - Primary key (needed)
  # - Unique constraint (needed for ON CONFLICT)
  # - City/ring index (small, fast)

  cat("✓ Spatial indexes dropped\n")
}

# Function to recreate spatial indexes after bulk loading
recreate_spatial_indexes <- function(con) {
  cat("Recreating spatial indexes...\n")

  # Recreate spatial indexes in parallel
  dbExecute(con, "CREATE INDEX CONCURRENTLY annulus_pixels_center_idx ON annulus_pixels USING GIST (pixel_center);")
  dbExecute(con, "CREATE INDEX CONCURRENTLY annulus_pixels_geom_idx ON annulus_pixels USING GIST (pixel_geom);")

  cat("✓ Spatial indexes recreated\n")
}

# Optimized batch processing function (same logic, no index overhead)
process_city_rings_fast <- function(con, geoname_id, city_name, ring_start, ring_end, max_rings) {
  cat(sprintf("  Processing rings %d-%d of %d for %s (FAST MODE)\n", ring_start, ring_end, max_rings, city_name))

  batch_start_time <- Sys.time()

  # Same query as before, but much faster without spatial index updates
  insert_query <- "
    WITH city_rings AS (
      SELECT ring_number, annulus_geom
      FROM city_annulus
      WHERE geoname_id = $1
        AND ring_number BETWEEN $2 AND $3
    ),

    population_pixel_data AS (
      SELECT
        r.ring_number,
        ST_PixelAsPolygons(p.rast, 1) as pixel_data
      FROM city_rings r
      CROSS JOIN worldpop_2025 p
      WHERE ST_Intersects(r.annulus_geom, p.rast)
    ),

    population_pixels AS (
      SELECT
        ring_number,
        (pixel_data).geom as pixel_geom,
        ST_Centroid((pixel_data).geom) as pixel_center,
        (pixel_data).val as population,
        (pixel_data).x as pixel_x,
        (pixel_data).y as pixel_y
      FROM population_pixel_data
      WHERE (pixel_data).val > 0
    ),

    water_pixel_data AS (
      SELECT
        r.ring_number,
        ST_PixelAsPolygons(w.rast, 1) as pixel_data
      FROM city_rings r
      CROSS JOIN water_pct w
      WHERE ST_Intersects(r.annulus_geom, w.rast)
    ),

    water_pixels AS (
      SELECT
        ring_number,
        (pixel_data).geom as pixel_geom,
        ST_Centroid((pixel_data).geom) as pixel_center,
        (pixel_data).val as water_pct,
        (pixel_data).x as pixel_x,
        (pixel_data).y as pixel_y
      FROM water_pixel_data
      WHERE (pixel_data).val >= 0
    ),

    all_pixels AS (
      SELECT
        ring_number, pixel_geom, pixel_center, population,
        NULL::double precision as water_pct, pixel_x, pixel_y
      FROM population_pixels
      UNION
      SELECT
        ring_number, pixel_geom, pixel_center, NULL::double precision as population,
        water_pct, pixel_x, pixel_y
      FROM water_pixels
    ),

    merged_pixels AS (
      SELECT
        ring_number,
        pixel_geom,
        pixel_center,
        MAX(population) as population_raw,
        MAX(water_pct) as water_pct_raw,
        MAX(pixel_x) as pixel_x,
        MAX(pixel_y) as pixel_y
      FROM all_pixels
      GROUP BY ring_number, pixel_geom, pixel_center
    ),

    pixels_with_fractions AS (
      SELECT
        mp.ring_number,
        mp.pixel_geom,
        mp.pixel_center,
        mp.population_raw,
        mp.water_pct_raw,
        mp.pixel_x,
        mp.pixel_y,
        ST_Area(ST_Intersection(mp.pixel_geom, cr.annulus_geom)) / ST_Area(mp.pixel_geom) as area_fraction
      FROM merged_pixels mp
      JOIN city_rings cr ON mp.ring_number = cr.ring_number
      WHERE ST_Intersects(mp.pixel_geom, cr.annulus_geom)
    )

    INSERT INTO annulus_pixels (
      geoname_id, ring_number, pixel_center, pixel_geom,
      population_raw, water_pct_raw, area_fraction,
      population_adjusted, water_pct_adjusted, pixel_x, pixel_y
    )
    SELECT
      $1::BIGINT as geoname_id,
      ring_number,
      pixel_center,
      pixel_geom,
      population_raw,
      water_pct_raw,
      area_fraction,
      CASE WHEN population_raw IS NOT NULL THEN population_raw * area_fraction ELSE NULL END,
      CASE WHEN water_pct_raw IS NOT NULL THEN water_pct_raw * area_fraction ELSE NULL END,
      pixel_x,
      pixel_y
    FROM pixels_with_fractions
    WHERE area_fraction > 0
      AND (population_raw IS NOT NULL OR water_pct_raw IS NOT NULL)
    ON CONFLICT (geoname_id, ring_number, pixel_x, pixel_y) DO NOTHING;
  "

  # Execute the insert
  result <- dbExecute(con, insert_query,
                     params = list(geoname_id, ring_start, ring_end))

  batch_duration <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))

  cat(sprintf("    Inserted %d pixels in %.1fs (%.0f pixels/sec) - FAST!\n",
              result, batch_duration,
              if(batch_duration > 0) result/batch_duration else 0))

  # Still record completion
  for (ring_num in ring_start:ring_end) {
    ring_pixels <- dbGetQuery(con, "
      SELECT COUNT(*) as count
      FROM annulus_pixels
      WHERE geoname_id = $1 AND ring_number = $2
    ", params = list(geoname_id, ring_num))[[1]]

    dbExecute(con, "
      INSERT INTO processing_completion (geoname_id, city_name, ring_number, pixels_processed, max_radius_km)
      VALUES ($1, $2, $3, $4, $5)
      ON CONFLICT (geoname_id, ring_number)
      DO UPDATE SET pixels_processed = $4, completed_at = CURRENT_TIMESTAMP
    ", params = list(geoname_id, city_name, ring_num, ring_pixels, max_rings))
  }

  return(result)
}

# Test the optimized approach on one city
cat("=== FAST BULK LOADING TEST ===\n\n")

# Import table creation functions
source("R/02_join_pop_with_cities.R", local = TRUE)
create_tables(con)

# Test with a city that hasn't been processed yet
test_city <- dbGetQuery(con, "
  SELECT DISTINCT geoname_id, city_name
  FROM city_annulus
  WHERE geoname_id NOT IN (SELECT DISTINCT geoname_id FROM annulus_pixels)
  LIMIT 1
")

if (nrow(test_city) == 0) {
  # Use our test city but clear it first
  cat("Using existing test city (clearing previous data)...\n")
  dbExecute(con, "DELETE FROM annulus_pixels WHERE geoname_id = 14256;")
  dbExecute(con, "DELETE FROM processing_completion WHERE geoname_id = 14256;")
  test_city <- data.frame(geoname_id = 14256, city_name = "Āzādshahr")
}

cat(sprintf("Testing with city: %s (ID: %s)\n\n", test_city$city_name, test_city$geoname_id))

# Step 1: Drop spatial indexes
drop_spatial_indexes(con)

# Step 2: Process city with fast loading (test with 20 rings)
total_start <- Sys.time()

total_pixels <- 0
for (ring_start in seq(1, 20, 5)) {
  ring_end <- min(ring_start + 4, 20)

  pixels_added <- process_city_rings_fast(
    con, test_city$geoname_id, test_city$city_name, ring_start, ring_end, 20
  )

  total_pixels <- total_pixels + pixels_added
}

bulk_time <- as.numeric(difftime(Sys.time(), total_start, units = "secs"))

cat(sprintf("\n✓ Bulk loading complete: %d pixels in %.1fs (%.0f pixels/sec)\n",
            total_pixels, bulk_time, total_pixels/bulk_time))

# Step 3: Recreate spatial indexes
cat("\nRecreating indexes...\n")
index_start <- Sys.time()
recreate_spatial_indexes(con)
index_time <- as.numeric(difftime(Sys.time(), index_start, units = "secs"))

cat(sprintf("✓ Indexes recreated in %.1fs\n", index_time))

total_time <- bulk_time + index_time
cat(sprintf("\n=== PERFORMANCE COMPARISON ===\n"))
cat(sprintf("OLD METHOD (estimated):    60s/batch × 4 batches = 240s total\n"))
cat(sprintf("NEW METHOD (actual):       %.1fs bulk + %.1fs index = %.1fs total\n",
            bulk_time, index_time, total_time))
cat(sprintf("SPEEDUP:                   %.1fx faster!\n", 240/total_time))

dbDisconnect(con)