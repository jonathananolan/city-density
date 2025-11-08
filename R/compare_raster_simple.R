# SIMPLE RASTER APPROACH COMPARISON - Focus on storage analysis
# Test different storage methods for Melbourne 75km

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

cat("=== SIMPLE STORAGE COMPARISON ===\n")
cat(sprintf("Testing: %s (ID: %s) up to %d rings\n\n", city_name, geoname_id, max_rings))

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

# Use existing data from ultra-optimized approach if available
source_exists <- dbExistsTable(con, "temp_source_pixels")
if (!source_exists) {
  cat("Creating source data from scratch...\n")
  tic("prepare_source")

  # Create simplified source data
  dbExecute(con, "DROP TABLE IF EXISTS temp_source_pixels;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_source_pixels AS
    SELECT
      ring_number,
      pixel_center,
      population_raw,
      water_pct_raw,
      area_fraction,
      pixel_x,
      pixel_y
    FROM annulus_pixels_points
    WHERE geoname_id = $1 AND ring_number <= $2;
  ", params = list(geoname_id, max_rings))

  source_time <- toc(quiet = TRUE)
  cat(sprintf("✓ Source created in %.2fs\n\n", source_time$toc - source_time$tic))
} else {
  cat("✓ Using existing source data\n\n")
}

source_count <- get_row_count(con, "temp_source_pixels")
cat(sprintf("Source data: %d pixels\n\n", source_count))

# =============================================================================
# APPROACH 1: Standard Points Table (current approach)
# =============================================================================
cat("APPROACH 1: Standard Points Table (current approach)\n")
tic("1_points_table")

dbExecute(con, "DROP TABLE IF EXISTS storage_test_points;")
dbExecute(con, "
  CREATE TABLE storage_test_points (
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

dbExecute(con, "
  INSERT INTO storage_test_points (
    geoname_id, ring_number, pixel_center, population_raw, water_pct_raw,
    area_fraction, population_adjusted, water_pct_adjusted, pixel_x, pixel_y
  )
  SELECT
    $1::BIGINT, ring_number, pixel_center, population_raw, water_pct_raw,
    area_fraction,
    COALESCE(population_raw * area_fraction, 0),
    COALESCE(water_pct_raw * area_fraction, 0),
    pixel_x, pixel_y
  FROM temp_source_pixels;
", params = list(geoname_id))

points_time <- toc(quiet = TRUE)
points_rows <- get_row_count(con, "storage_test_points")
points_size <- get_table_size(con, "storage_test_points")
cat(sprintf("✓ Created points table: %d rows in %.2fs\n", points_rows, points_time$toc - points_time$tic))
cat(sprintf("  Storage: %s (%s bytes)\n\n", points_size$pretty, format(points_size$bytes, big.mark=",")))

# =============================================================================
# APPROACH 2: Aggregated by Ring (summary statistics per ring)
# =============================================================================
cat("APPROACH 2: Aggregated by Ring (summary per ring)\n")
tic("2_by_ring")

dbExecute(con, "DROP TABLE IF EXISTS storage_test_by_ring;")
dbExecute(con, "
  CREATE TABLE storage_test_by_ring (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    pixel_count INTEGER,
    total_population DOUBLE PRECISION,
    avg_population DOUBLE PRECISION,
    total_water_area DOUBLE PRECISION,
    avg_area_fraction DOUBLE PRECISION,
    ring_area_km2 DOUBLE PRECISION,
    density_per_km2 DOUBLE PRECISION
  );
")

dbExecute(con, "
  INSERT INTO storage_test_by_ring (
    geoname_id, ring_number, pixel_count, total_population, avg_population,
    total_water_area, avg_area_fraction, ring_area_km2, density_per_km2
  )
  SELECT
    $1::BIGINT,
    ring_number,
    COUNT(*) as pixel_count,
    SUM(COALESCE(population_raw * area_fraction, 0)) as total_population,
    AVG(COALESCE(population_raw, 0)) as avg_population,
    SUM(COALESCE(water_pct_raw * area_fraction / 100.0, 0)) as total_water_area,
    AVG(area_fraction) as avg_area_fraction,
    COUNT(*) * 0.01 as ring_area_km2,  -- 100m x 100m = 0.01 km2 per pixel
    SUM(COALESCE(population_raw * area_fraction, 0)) / NULLIF(COUNT(*) * 0.01, 0) as density_per_km2
  FROM temp_source_pixels
  GROUP BY ring_number
  ORDER BY ring_number;
", params = list(geoname_id))

ring_time <- toc(quiet = TRUE)
ring_rows <- get_row_count(con, "storage_test_by_ring")
ring_size <- get_table_size(con, "storage_test_by_ring")
cat(sprintf("✓ Created ring summary: %d rows in %.2fs\n", ring_rows, ring_time$toc - ring_time$tic))
cat(sprintf("  Storage: %s (%s bytes)\n\n", ring_size$pretty, format(ring_size$bytes, big.mark=",")))

# =============================================================================
# APPROACH 3: Tiled Storage (group by spatial tiles)
# =============================================================================
cat("APPROACH 3: Tiled Storage (group by 10km tiles)\n")
tic("3_by_tile")

dbExecute(con, "DROP TABLE IF EXISTS storage_test_by_tile;")
dbExecute(con, "
  CREATE TABLE storage_test_by_tile (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    tile_x INTEGER,
    tile_y INTEGER,
    min_ring INTEGER,
    max_ring INTEGER,
    rings_in_tile INTEGER[],
    pixel_count INTEGER,
    total_population DOUBLE PRECISION,
    total_water_area DOUBLE PRECISION,
    tile_center GEOMETRY(Point, 4326)
  );
")

dbExecute(con, "
  INSERT INTO storage_test_by_tile (
    geoname_id, tile_x, tile_y, min_ring, max_ring, rings_in_tile,
    pixel_count, total_population, total_water_area, tile_center
  )
  SELECT
    $1::BIGINT,
    FLOOR(pixel_x / 100) as tile_x,  -- 10km tiles
    FLOOR(pixel_y / 100) as tile_y,
    MIN(ring_number),
    MAX(ring_number),
    ARRAY_AGG(DISTINCT ring_number ORDER BY ring_number),
    COUNT(*) as pixel_count,
    SUM(COALESCE(population_raw * area_fraction, 0)) as total_population,
    SUM(COALESCE(water_pct_raw * area_fraction / 100.0, 0)) as total_water_area,
    ST_Centroid(ST_Collect(pixel_center)) as tile_center
  FROM temp_source_pixels
  GROUP BY FLOOR(pixel_x / 100), FLOOR(pixel_y / 100)
  HAVING COUNT(*) > 0
  ORDER BY tile_x, tile_y;
", params = list(geoname_id))

tile_time <- toc(quiet = TRUE)
tile_rows <- get_row_count(con, "storage_test_by_tile")
tile_size <- get_table_size(con, "storage_test_by_tile")
cat(sprintf("✓ Created tile summary: %d rows in %.2fs\n", tile_rows, tile_time$toc - tile_time$tic))
cat(sprintf("  Storage: %s (%s bytes)\n\n", tile_size$pretty, format(tile_size$bytes, big.mark=",")))

# =============================================================================
# APPROACH 4: Compressed Binary Data (JSON storage)
# =============================================================================
cat("APPROACH 4: JSON Compressed Storage\n")
tic("4_json_compressed")

dbExecute(con, "DROP TABLE IF EXISTS storage_test_json;")
dbExecute(con, "
  CREATE TABLE storage_test_json (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    pixel_data JSONB,
    pixel_count INTEGER
  );
")

dbExecute(con, "
  INSERT INTO storage_test_json (geoname_id, ring_number, pixel_data, pixel_count)
  SELECT
    $1::BIGINT,
    ring_number,
    jsonb_build_object(
      'pixels', jsonb_agg(
        jsonb_build_object(
          'x', pixel_x,
          'y', pixel_y,
          'pop', population_raw,
          'water', water_pct_raw,
          'frac', area_fraction
        )
      ),
      'summary', jsonb_build_object(
        'total_pop', SUM(COALESCE(population_raw * area_fraction, 0)),
        'avg_pop', AVG(COALESCE(population_raw, 0)),
        'pixel_count', COUNT(*)
      )
    ) as pixel_data,
    COUNT(*) as pixel_count
  FROM temp_source_pixels
  GROUP BY ring_number
  ORDER BY ring_number;
", params = list(geoname_id))

json_time <- toc(quiet = TRUE)
json_rows <- get_row_count(con, "storage_test_json")
json_size <- get_table_size(con, "storage_test_json")
cat(sprintf("✓ Created JSON storage: %d rows in %.2fs\n", json_rows, json_time$toc - json_time$tic))
cat(sprintf("  Storage: %s (%s bytes)\n\n", json_size$pretty, format(json_size$bytes, big.mark=",")))

# =============================================================================
# SUMMARY COMPARISON
# =============================================================================
cat("=== STORAGE COMPARISON SUMMARY ===\n")
cat(sprintf("%-20s %10s %8s %15s %12s\n", "Approach", "Time (s)", "Rows", "Storage", "Bytes"))
cat(sprintf("%-20s %10s %8s %15s %12s\n", "--------", "--------", "----", "-------", "-----"))
cat(sprintf("%-20s %10.2f %8d %15s %12s\n", "1. Points Table", points_time$toc - points_time$tic, points_rows, points_size$pretty, format(points_size$bytes, big.mark=",")))
cat(sprintf("%-20s %10.2f %8d %15s %12s\n", "2. By Ring", ring_time$toc - ring_time$tic, ring_rows, ring_size$pretty, format(ring_size$bytes, big.mark=",")))
cat(sprintf("%-20s %10.2f %8d %15s %12s\n", "3. By Tile", tile_time$toc - tile_time$tic, tile_rows, tile_size$pretty, format(tile_size$bytes, big.mark=",")))
cat(sprintf("%-20s %10.2f %8d %15s %12s\n", "4. JSON Compressed", json_time$toc - json_time$tic, json_rows, json_size$pretty, format(json_size$bytes, big.mark=",")))

# Calculate storage efficiency
cat(sprintf("\\n=== STORAGE EFFICIENCY ===\\n"))
baseline_bytes <- points_size$bytes
cat(sprintf("Baseline (Points): %s\\n", points_size$pretty))
cat(sprintf("By Ring:    %.1fx smaller (%s)\\n", baseline_bytes / ring_size$bytes, ring_size$pretty))
cat(sprintf("By Tile:    %.1fx smaller (%s)\\n", baseline_bytes / tile_size$bytes, tile_size$pretty))
cat(sprintf("JSON:       %.1fx smaller (%s)\\n", baseline_bytes / json_size$bytes, json_size$pretty))

# Estimate global storage needs
cat(sprintf("\\n=== GLOBAL STORAGE ESTIMATES ===\\n"))
melbourne_multiplier <- 1612 * (150/75)^2  # 1612 cities × (150km/75km)^2
cat(sprintf("For all 1612 cities at 150km radius:\\n"))
cat(sprintf("Points Table: %s\\n", format(points_size$bytes * melbourne_multiplier, big.mark=",")))
cat(sprintf("By Ring:      %s\\n", format(ring_size$bytes * melbourne_multiplier, big.mark=",")))
cat(sprintf("By Tile:      %s\\n", format(tile_size$bytes * melbourne_multiplier, big.mark=",")))
cat(sprintf("JSON:         %s\\n", format(json_size$bytes * melbourne_multiplier, big.mark=",")))

# Cleanup
dbExecute(con, "DROP TABLE IF EXISTS storage_test_points;")
dbExecute(con, "DROP TABLE IF EXISTS storage_test_by_ring;")
dbExecute(con, "DROP TABLE IF EXISTS storage_test_by_tile;")
dbExecute(con, "DROP TABLE IF EXISTS storage_test_json;")

dbDisconnect(con)

cat("\\n=== COMPARISON COMPLETE ===\\n")