# RIGOROUS STORAGE APPROACH COMPARISON
# Real timing data, real disk usage, persistent logging

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

# Test parameters
geoname_id <- 2158177  # Melbourne
max_rings <- 75
city_name <- "Melbourne"

cat("=== RIGOROUS STORAGE COMPARISON ===\n")
cat(sprintf("Testing: %s (ID: %s) up to %d rings\n", city_name, geoname_id, max_rings))
cat("All timing and storage data will be logged to files.\n\n")

# Create results logging table
dbExecute(con, "DROP TABLE IF EXISTS comparison_results;")
dbExecute(con, "
  CREATE TABLE comparison_results (
    id SERIAL PRIMARY KEY,
    test_run TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    approach_name VARCHAR(50),
    step_name VARCHAR(50),
    duration_seconds DOUBLE PRECISION,
    row_count INTEGER,
    storage_bytes BIGINT,
    storage_pretty VARCHAR(20),
    notes TEXT
  );
")

# Function to log results
log_result <- function(con, approach, step, duration, rows, storage_bytes, storage_pretty, notes = "") {
  dbExecute(con, "
    INSERT INTO comparison_results (approach_name, step_name, duration_seconds, row_count, storage_bytes, storage_pretty, notes)
    VALUES ($1, $2, $3, $4, $5, $6, $7)
  ", params = list(approach, step, duration, rows, storage_bytes, storage_pretty, notes))
}

# Function to get table size
get_table_size <- function(con, table_name) {
  size_query <- sprintf("
    SELECT
      pg_size_pretty(pg_total_relation_size('%s')) as pretty_size,
      pg_total_relation_size('%s') as bytes
  ", table_name, table_name)
  tryCatch({
    result <- dbGetQuery(con, size_query)
    return(list(pretty = result$pretty_size[1], bytes = as.numeric(result$bytes[1])))
  }, error = function(e) {
    return(list(pretty = "Table not found", bytes = 0))
  })
}

# Function to get row count
get_row_count <- function(con, table_name) {
  tryCatch({
    result <- dbGetQuery(con, sprintf("SELECT COUNT(*) as count FROM %s;", table_name))
    return(as.integer(result$count[1]))
  }, error = function(e) {
    return(0)
  })
}

# =============================================================================
# STEP 0: PREPARE SOURCE DATA (shared across all approaches)
# =============================================================================
cat("STEP 0: Preparing source data...\n")
tic("source_prep")

# Create city boundary
dbExecute(con, "DROP TABLE IF EXISTS temp_city_boundary_rig;")
dbExecute(con, "
  CREATE TEMP TABLE temp_city_boundary_rig AS
  SELECT ST_Union(annulus_geom) as city_geom
  FROM city_annulus
  WHERE geoname_id = $1 AND ring_number <= $2;
", params = list(geoname_id, max_rings))

# Create temp rasters
dbExecute(con, "DROP TABLE IF EXISTS temp_pop_rasters_rig;")
dbExecute(con, "
  CREATE TEMP TABLE temp_pop_rasters_rig AS
  SELECT DISTINCT p.rid, p.rast, p.country_iso3
  FROM temp_city_boundary_rig cb
  CROSS JOIN worldpop_2025 p
  WHERE ST_Intersects(cb.city_geom, p.rast);
")

dbExecute(con, "DROP TABLE IF EXISTS temp_water_rasters_rig;")
dbExecute(con, "
  CREATE TEMP TABLE temp_water_rasters_rig AS
  SELECT DISTINCT w.rid, w.rast
  FROM temp_city_boundary_rig cb
  CROSS JOIN water_pct w
  WHERE ST_Intersects(cb.city_geom, w.rast);
")

# Extract and merge pixels using ultra-optimized approach
dbExecute(con, "DROP TABLE IF EXISTS temp_source_pixels_rig;")
dbExecute(con, "
  CREATE TEMP TABLE temp_source_pixels_rig AS
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2
  ),

  pop_pixel_data AS (
    SELECT
      r.ring_number,
      ST_PixelAsCentroids(p.rast, 1) as pixel_data
    FROM city_rings r
    CROSS JOIN temp_pop_rasters_rig p
    WHERE ST_Intersects(r.annulus_geom, p.rast)
  ),

  pop_points AS (
    SELECT
      ring_number,
      (pixel_data).geom as pixel_center,
      (pixel_data).val as population,
      (pixel_data).x as pixel_x,
      (pixel_data).y as pixel_y
    FROM pop_pixel_data
    WHERE (pixel_data).val > 0
  ),

  water_pixel_data AS (
    SELECT
      r.ring_number,
      ST_PixelAsCentroids(w.rast, 1) as pixel_data
    FROM city_rings r
    CROSS JOIN temp_water_rasters_rig w
    WHERE ST_Intersects(r.annulus_geom, w.rast)
  ),

  water_points AS (
    SELECT
      ring_number,
      (pixel_data).geom as pixel_center,
      (pixel_data).val as water_pct,
      (pixel_data).x as pixel_x,
      (pixel_data).y as pixel_y
    FROM water_pixel_data
    WHERE (pixel_data).val > 5
  ),

  all_pixels AS (
    SELECT ring_number, pixel_center, population, NULL::double precision as water_pct, pixel_x, pixel_y
    FROM pop_points
    UNION
    SELECT ring_number, pixel_center, NULL::double precision, water_pct, pixel_x, pixel_y
    FROM water_points
  ),

  merged_pixels AS (
    SELECT
      ring_number, pixel_center,
      MAX(population) as population_raw,
      MAX(water_pct) as water_pct_raw,
      MAX(pixel_x) as pixel_x,
      MAX(pixel_y) as pixel_y
    FROM all_pixels
    GROUP BY ring_number, pixel_center
  )

  SELECT
    mp.ring_number,
    mp.pixel_center,
    mp.population_raw,
    mp.water_pct_raw,
    mp.pixel_x,
    mp.pixel_y,
    CASE
      WHEN ST_Within(mp.pixel_center, cr.annulus_geom) THEN 1.0
      ELSE 0.7
    END as area_fraction
  FROM merged_pixels mp
  JOIN city_rings cr ON mp.ring_number = cr.ring_number
  WHERE ST_Intersects(mp.pixel_center, cr.annulus_geom);
", params = list(geoname_id, max_rings))

source_time <- toc(quiet = TRUE)
source_duration <- source_time$toc - source_time$tic
source_count <- get_row_count(con, "temp_source_pixels_rig")

cat(sprintf("✓ Source data: %d pixels in %.2fs\n\n", source_count, source_duration))

# Log source preparation
log_result(con, "SOURCE_PREP", "data_extraction", source_duration, source_count, 0, "N/A", "Shared source data for all approaches")

# =============================================================================
# APPROACH 1: DETAILED POINTS STORAGE
# =============================================================================
cat("APPROACH 1: Detailed Points Storage\n")

# Step 1A: Create table structure
tic("1a_create_table")
dbExecute(con, "DROP TABLE IF EXISTS approach1_points;")
dbExecute(con, "
  CREATE TABLE approach1_points (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    pixel_center GEOMETRY(Point, 4326),
    population_raw DOUBLE PRECISION,
    water_pct_raw DOUBLE PRECISION,
    area_fraction DOUBLE PRECISION,
    population_adjusted DOUBLE PRECISION,
    water_pct_adjusted DOUBLE PRECISION,
    pixel_x INTEGER,
    pixel_y INTEGER
  );
")
create1_time <- toc(quiet = TRUE)
create1_duration <- create1_time$toc - create1_time$tic

# Step 1B: Insert data
tic("1b_insert_data")
dbExecute(con, "
  INSERT INTO approach1_points (
    geoname_id, ring_number, pixel_center, population_raw, water_pct_raw,
    area_fraction, population_adjusted, water_pct_adjusted, pixel_x, pixel_y
  )
  SELECT
    $1::BIGINT, ring_number, pixel_center, population_raw, water_pct_raw,
    area_fraction,
    COALESCE(population_raw * area_fraction, 0),
    COALESCE(water_pct_raw * area_fraction, 0),
    pixel_x, pixel_y
  FROM temp_source_pixels_rig;
", params = list(geoname_id))
insert1_time <- toc(quiet = TRUE)
insert1_duration <- insert1_time$toc - insert1_time$tic

# Step 1C: Create indexes
tic("1c_create_indexes")
dbExecute(con, "CREATE INDEX approach1_geoname_ring_idx ON approach1_points (geoname_id, ring_number);")
dbExecute(con, "CREATE INDEX approach1_center_idx ON approach1_points USING GIST (pixel_center);")
index1_time <- toc(quiet = TRUE)
index1_duration <- index1_time$toc - index1_time$tic

# Measure final storage
points1_rows <- get_row_count(con, "approach1_points")
points1_size <- get_table_size(con, "approach1_points")
total1_duration <- create1_duration + insert1_duration + index1_duration

cat(sprintf("  Create table: %.2fs\n", create1_duration))
cat(sprintf("  Insert data:  %.2fs\n", insert1_duration))
cat(sprintf("  Create index: %.2fs\n", index1_duration))
cat(sprintf("  TOTAL:        %.2fs\n", total1_duration))
cat(sprintf("  Rows:         %d\n", points1_rows))
cat(sprintf("  Storage:      %s (%s bytes)\n\n", points1_size$pretty, format(points1_size$bytes, big.mark=",")))

# Log Approach 1 results
log_result(con, "APPROACH1_POINTS", "create_table", create1_duration, 0, 0, "N/A", "Table structure creation")
log_result(con, "APPROACH1_POINTS", "insert_data", insert1_duration, points1_rows, 0, "N/A", "Data insertion")
log_result(con, "APPROACH1_POINTS", "create_indexes", index1_duration, 0, 0, "N/A", "Index creation")
log_result(con, "APPROACH1_POINTS", "TOTAL", total1_duration, points1_rows, points1_size$bytes, points1_size$pretty, "Complete approach")

# =============================================================================
# APPROACH 2: RING AGGREGATION
# =============================================================================
cat("APPROACH 2: Ring Aggregation\n")

# Step 2A: Create table
tic("2a_create_table")
dbExecute(con, "DROP TABLE IF EXISTS approach2_rings;")
dbExecute(con, "
  CREATE TABLE approach2_rings (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    pixel_count INTEGER,
    total_population DOUBLE PRECISION,
    avg_population DOUBLE PRECISION,
    max_population DOUBLE PRECISION,
    total_water_pixels INTEGER,
    avg_water_pct DOUBLE PRECISION,
    avg_area_fraction DOUBLE PRECISION,
    ring_area_km2 DOUBLE PRECISION,
    population_density_per_km2 DOUBLE PRECISION,
    ring_center GEOMETRY(Point, 4326),
    ring_bbox GEOMETRY(Polygon, 4326)
  );
")
create2_time <- toc(quiet = TRUE)
create2_duration <- create2_time$toc - create2_time$tic

# Step 2B: Aggregate data
tic("2b_aggregate_data")
dbExecute(con, "
  INSERT INTO approach2_rings (
    geoname_id, ring_number, pixel_count, total_population, avg_population, max_population,
    total_water_pixels, avg_water_pct, avg_area_fraction, ring_area_km2,
    population_density_per_km2, ring_center, ring_bbox
  )
  SELECT
    $1::BIGINT,
    ring_number,
    COUNT(*) as pixel_count,
    SUM(COALESCE(population_raw * area_fraction, 0)) as total_population,
    AVG(COALESCE(population_raw, 0)) as avg_population,
    MAX(COALESCE(population_raw, 0)) as max_population,
    COUNT(CASE WHEN water_pct_raw > 0 THEN 1 END) as total_water_pixels,
    AVG(COALESCE(water_pct_raw, 0)) as avg_water_pct,
    AVG(area_fraction) as avg_area_fraction,
    COUNT(*) * 0.01 as ring_area_km2,
    SUM(COALESCE(population_raw * area_fraction, 0)) / NULLIF(COUNT(*) * 0.01, 0) as population_density_per_km2,
    ST_Centroid(ST_Collect(pixel_center)) as ring_center,
    ST_Envelope(ST_Collect(pixel_center)) as ring_bbox
  FROM temp_source_pixels_rig
  GROUP BY ring_number
  ORDER BY ring_number;
", params = list(geoname_id))
aggregate2_time <- toc(quiet = TRUE)
aggregate2_duration <- aggregate2_time$toc - aggregate2_time$tic

# Step 2C: Create indexes
tic("2c_create_indexes")
dbExecute(con, "CREATE INDEX approach2_geoname_ring_idx ON approach2_rings (geoname_id, ring_number);")
dbExecute(con, "CREATE INDEX approach2_center_idx ON approach2_rings USING GIST (ring_center);")
index2_time <- toc(quiet = TRUE)
index2_duration <- index2_time$toc - index2_time$tic

rings2_rows <- get_row_count(con, "approach2_rings")
rings2_size <- get_table_size(con, "approach2_rings")
total2_duration <- create2_duration + aggregate2_duration + index2_duration

cat(sprintf("  Create table: %.2fs\n", create2_duration))
cat(sprintf("  Aggregate:    %.2fs\n", aggregate2_duration))
cat(sprintf("  Create index: %.2fs\n", index2_duration))
cat(sprintf("  TOTAL:        %.2fs\n", total2_duration))
cat(sprintf("  Rows:         %d\n", rings2_rows))
cat(sprintf("  Storage:      %s (%s bytes)\n\n", rings2_size$pretty, format(rings2_size$bytes, big.mark=",")))

log_result(con, "APPROACH2_RINGS", "create_table", create2_duration, 0, 0, "N/A", "Table creation")
log_result(con, "APPROACH2_RINGS", "aggregate_data", aggregate2_duration, rings2_rows, 0, "N/A", "Data aggregation")
log_result(con, "APPROACH2_RINGS", "create_indexes", index2_duration, 0, 0, "N/A", "Index creation")
log_result(con, "APPROACH2_RINGS", "TOTAL", total2_duration, rings2_rows, rings2_size$bytes, rings2_size$pretty, "Complete approach")

# =============================================================================
# APPROACH 3: TILE-BASED AGGREGATION
# =============================================================================
cat("APPROACH 3: Tile-Based Aggregation\n")

# Step 3A: Create table
tic("3a_create_table")
dbExecute(con, "DROP TABLE IF EXISTS approach3_tiles;")
dbExecute(con, "
  CREATE TABLE approach3_tiles (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    tile_x INTEGER,
    tile_y INTEGER,
    tile_id VARCHAR(20),
    min_ring INTEGER,
    max_ring INTEGER,
    ring_count INTEGER,
    rings_in_tile INTEGER[],
    pixel_count INTEGER,
    total_population DOUBLE PRECISION,
    avg_population DOUBLE PRECISION,
    total_water_pixels INTEGER,
    avg_water_pct DOUBLE PRECISION,
    tile_area_km2 DOUBLE PRECISION,
    population_density_per_km2 DOUBLE PRECISION,
    tile_center GEOMETRY(Point, 4326),
    tile_bbox GEOMETRY(Polygon, 4326)
  );
")
create3_time <- toc(quiet = TRUE)
create3_duration <- create3_time$toc - create3_time$tic

# Step 3B: Aggregate by tiles
tic("3b_aggregate_tiles")
dbExecute(con, "
  INSERT INTO approach3_tiles (
    geoname_id, tile_x, tile_y, tile_id, min_ring, max_ring, ring_count,
    rings_in_tile, pixel_count, total_population, avg_population,
    total_water_pixels, avg_water_pct, tile_area_km2, population_density_per_km2,
    tile_center, tile_bbox
  )
  SELECT
    $1::BIGINT,
    FLOOR(pixel_x / 100) as tile_x,
    FLOOR(pixel_y / 100) as tile_y,
    CONCAT('T_', FLOOR(pixel_x / 100), '_', FLOOR(pixel_y / 100)) as tile_id,
    MIN(ring_number),
    MAX(ring_number),
    COUNT(DISTINCT ring_number),
    ARRAY_AGG(DISTINCT ring_number ORDER BY ring_number),
    COUNT(*) as pixel_count,
    SUM(COALESCE(population_raw * area_fraction, 0)) as total_population,
    AVG(COALESCE(population_raw, 0)) as avg_population,
    COUNT(CASE WHEN water_pct_raw > 0 THEN 1 END) as total_water_pixels,
    AVG(COALESCE(water_pct_raw, 0)) as avg_water_pct,
    COUNT(*) * 0.01 as tile_area_km2,
    SUM(COALESCE(population_raw * area_fraction, 0)) / NULLIF(COUNT(*) * 0.01, 0) as population_density_per_km2,
    ST_Centroid(ST_Collect(pixel_center)) as tile_center,
    ST_Envelope(ST_Collect(pixel_center)) as tile_bbox
  FROM temp_source_pixels_rig
  GROUP BY FLOOR(pixel_x / 100), FLOOR(pixel_y / 100)
  HAVING COUNT(*) > 0
  ORDER BY tile_x, tile_y;
", params = list(geoname_id))
aggregate3_time <- toc(quiet = TRUE)
aggregate3_duration <- aggregate3_time$toc - aggregate3_time$tic

# Step 3C: Create indexes
tic("3c_create_indexes")
dbExecute(con, "CREATE INDEX approach3_geoname_tile_idx ON approach3_tiles (geoname_id, tile_x, tile_y);")
dbExecute(con, "CREATE INDEX approach3_rings_idx ON approach3_tiles USING GIN (rings_in_tile);")
dbExecute(con, "CREATE INDEX approach3_center_idx ON approach3_tiles USING GIST (tile_center);")
index3_time <- toc(quiet = TRUE)
index3_duration <- index3_time$toc - index3_time$tic

tiles3_rows <- get_row_count(con, "approach3_tiles")
tiles3_size <- get_table_size(con, "approach3_tiles")
total3_duration <- create3_duration + aggregate3_duration + index3_duration

cat(sprintf("  Create table: %.2fs\n", create3_duration))
cat(sprintf("  Aggregate:    %.2fs\n", aggregate3_duration))
cat(sprintf("  Create index: %.2fs\n", index3_duration))
cat(sprintf("  TOTAL:        %.2fs\n", total3_duration))
cat(sprintf("  Rows:         %d\n", tiles3_rows))
cat(sprintf("  Storage:      %s (%s bytes)\n\n", tiles3_size$pretty, format(tiles3_size$bytes, big.mark=",")))

log_result(con, "APPROACH3_TILES", "create_table", create3_duration, 0, 0, "N/A", "Table creation")
log_result(con, "APPROACH3_TILES", "aggregate_data", aggregate3_duration, tiles3_rows, 0, "N/A", "Tile aggregation")
log_result(con, "APPROACH3_TILES", "create_indexes", index3_duration, 0, 0, "N/A", "Index creation")
log_result(con, "APPROACH3_TILES", "TOTAL", total3_duration, tiles3_rows, tiles3_size$bytes, tiles3_size$pretty, "Complete approach")

# =============================================================================
# APPROACH 4: JSON COMPRESSED STORAGE
# =============================================================================
cat("APPROACH 4: JSON Compressed Storage\n")

# Step 4A: Create table
tic("4a_create_table")
dbExecute(con, "DROP TABLE IF EXISTS approach4_json;")
dbExecute(con, "
  CREATE TABLE approach4_json (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    pixel_data JSONB,
    summary_stats JSONB,
    pixel_count INTEGER
  );
")
create4_time <- toc(quiet = TRUE)
create4_duration <- create4_time$toc - create4_time$tic

# Step 4B: Insert JSON data
tic("4b_insert_json")
dbExecute(con, "
  INSERT INTO approach4_json (geoname_id, ring_number, pixel_data, summary_stats, pixel_count)
  SELECT
    $1::BIGINT,
    ring_number,
    jsonb_build_object(
      'pixels', jsonb_agg(
        jsonb_build_object(
          'x', pixel_x,
          'y', pixel_y,
          'lat', ST_Y(pixel_center),
          'lon', ST_X(pixel_center),
          'pop', population_raw,
          'water', water_pct_raw,
          'frac', area_fraction
        )
      )
    ) as pixel_data,
    jsonb_build_object(
      'total_pop', SUM(COALESCE(population_raw * area_fraction, 0)),
      'avg_pop', AVG(COALESCE(population_raw, 0)),
      'max_pop', MAX(COALESCE(population_raw, 0)),
      'total_water_pixels', COUNT(CASE WHEN water_pct_raw > 0 THEN 1 END),
      'avg_water', AVG(COALESCE(water_pct_raw, 0)),
      'avg_fraction', AVG(area_fraction),
      'pixel_count', COUNT(*),
      'area_km2', COUNT(*) * 0.01,
      'density_per_km2', SUM(COALESCE(population_raw * area_fraction, 0)) / NULLIF(COUNT(*) * 0.01, 0)
    ) as summary_stats,
    COUNT(*) as pixel_count
  FROM temp_source_pixels_rig
  GROUP BY ring_number
  ORDER BY ring_number;
", params = list(geoname_id))
insert4_time <- toc(quiet = TRUE)
insert4_duration <- insert4_time$toc - insert4_time$tic

# Step 4C: Create indexes
tic("4c_create_indexes")
dbExecute(con, "CREATE INDEX approach4_geoname_ring_idx ON approach4_json (geoname_id, ring_number);")
dbExecute(con, "CREATE INDEX approach4_summary_idx ON approach4_json USING GIN (summary_stats);")
index4_time <- toc(quiet = TRUE)
index4_duration <- index4_time$toc - index4_time$tic

json4_rows <- get_row_count(con, "approach4_json")
json4_size <- get_table_size(con, "approach4_json")
total4_duration <- create4_duration + insert4_duration + index4_duration

cat(sprintf("  Create table: %.2fs\n", create4_duration))
cat(sprintf("  Insert JSON:  %.2fs\n", insert4_duration))
cat(sprintf("  Create index: %.2fs\n", index4_duration))
cat(sprintf("  TOTAL:        %.2fs\n", total4_duration))
cat(sprintf("  Rows:         %d\n", json4_rows))
cat(sprintf("  Storage:      %s (%s bytes)\n\n", json4_size$pretty, format(json4_size$bytes, big.mark=",")))

log_result(con, "APPROACH4_JSON", "create_table", create4_duration, 0, 0, "N/A", "Table creation")
log_result(con, "APPROACH4_JSON", "insert_data", insert4_duration, json4_rows, 0, "N/A", "JSON data insertion")
log_result(con, "APPROACH4_JSON", "create_indexes", index4_duration, 0, 0, "N/A", "Index creation")
log_result(con, "APPROACH4_JSON", "TOTAL", total4_duration, json4_rows, json4_size$bytes, json4_size$pretty, "Complete approach")

# =============================================================================
# COMPARISON SUMMARY
# =============================================================================
cat("=== FINAL COMPARISON SUMMARY ===\n")
cat(sprintf("%-20s %10s %8s %15s %15s\n", "Approach", "Time (s)", "Rows", "Storage", "Bytes"))
cat(sprintf("%-20s %10s %8s %15s %15s\n", "--------", "--------", "----", "-------", "-----"))
cat(sprintf("%-20s %10.2f %8d %15s %15s\n", "1. Points Detail", total1_duration, points1_rows, points1_size$pretty, format(points1_size$bytes, big.mark=",")))
cat(sprintf("%-20s %10.2f %8d %15s %15s\n", "2. Ring Aggregation", total2_duration, rings2_rows, rings2_size$pretty, format(rings2_size$bytes, big.mark=",")))
cat(sprintf("%-20s %10.2f %8d %15s %15s\n", "3. Tile Aggregation", total3_duration, tiles3_rows, tiles3_size$pretty, format(tiles3_size$bytes, big.mark=",")))
cat(sprintf("%-20s %10.2f %8d %15s %15s\n", "4. JSON Compressed", total4_duration, json4_rows, json4_size$pretty, format(json4_size$bytes, big.mark=",")))

# Storage efficiency analysis
baseline_bytes <- points1_size$bytes
cat(sprintf("\n=== STORAGE EFFICIENCY ===\n"))
cat(sprintf("Baseline (Points):    %s\n", points1_size$pretty))
cat(sprintf("Ring Aggregation:     %.0fx smaller (%s)\n", baseline_bytes / rings2_size$bytes, rings2_size$pretty))
cat(sprintf("Tile Aggregation:     %.0fx smaller (%s)\n", baseline_bytes / tiles3_size$bytes, tiles3_size$pretty))
cat(sprintf("JSON Compressed:      %.0fx smaller (%s)\n", baseline_bytes / json4_size$bytes, json4_size$pretty))

# Global estimates
melbourne_scale <- 1612 * (150/75)^2
cat(sprintf("\n=== GLOBAL ESTIMATES (1612 cities × 150km radius) ===\n"))
cat(sprintf("Points Detail:    %s\n", format(points1_size$bytes * melbourne_scale, big.mark=",")))
cat(sprintf("Ring Aggregation: %s\n", format(rings2_size$bytes * melbourne_scale, big.mark=",")))
cat(sprintf("Tile Aggregation: %s\n", format(tiles3_size$bytes * melbourne_scale, big.mark=",")))
cat(sprintf("JSON Compressed:  %s\n", format(json4_size$bytes * melbourne_scale, big.mark=",")))

# Export detailed results to CSV
results_data <- dbGetQuery(con, "
  SELECT approach_name, step_name, duration_seconds, row_count, storage_bytes, storage_pretty, notes
  FROM comparison_results
  ORDER BY test_run, approach_name,
    CASE step_name
      WHEN 'create_table' THEN 1
      WHEN 'insert_data' THEN 2
      WHEN 'aggregate_data' THEN 2
      WHEN 'create_indexes' THEN 3
      WHEN 'TOTAL' THEN 4
      ELSE 5
    END
")

write.csv(results_data, "raster_comparison_results.csv", row.names = FALSE)
cat(sprintf("\n✓ Detailed results exported to raster_comparison_results.csv\n"))

# Keep results table for further analysis
cat(sprintf("✓ Results stored in comparison_results table for SQL analysis\n"))

# Clean up test tables but keep results
dbExecute(con, "DROP TABLE IF EXISTS approach1_points;")
dbExecute(con, "DROP TABLE IF EXISTS approach2_rings;")
dbExecute(con, "DROP TABLE IF EXISTS approach3_tiles;")
dbExecute(con, "DROP TABLE IF EXISTS approach4_json;")

dbDisconnect(con)

cat("\n=== RIGOROUS COMPARISON COMPLETE ===\n")
cat("All timing and storage data measured with real PostgreSQL operations.\n")
cat("Results logged to database table and CSV file for further analysis.\n")