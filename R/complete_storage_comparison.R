# COMPLETE STORAGE COMPARISON - Raster vs Tabular Methods
# Tests ALL storage approaches: raster (ring-based, tile-based) + tabular (points, summaries)

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
max_rings <- 150
city_name <- "Melbourne"

cat("=== COMPLETE STORAGE COMPARISON ===\n")
cat("Testing ALL approaches: Raster + Tabular\n")
cat(sprintf("City: %s (ID: %s) up to %d rings\n\n", city_name, geoname_id, max_rings))

# Logging functions
dbExecute(con, "DROP TABLE IF EXISTS complete_comparison_results;")
dbExecute(con, "
  CREATE TABLE complete_comparison_results (
    id SERIAL PRIMARY KEY,
    test_run TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    approach_name VARCHAR(50),
    step_name VARCHAR(50),
    duration_seconds DOUBLE PRECISION,
    row_count INTEGER,
    storage_bytes BIGINT,
    storage_pretty VARCHAR(20),
    approach_type VARCHAR(20), -- 'RASTER' or 'TABULAR'
    notes TEXT
  );
")

log_result <- function(con, approach, step, duration, rows, storage_bytes, storage_pretty, approach_type, notes = "") {
  dbExecute(con, "
    INSERT INTO complete_comparison_results (approach_name, step_name, duration_seconds, row_count, storage_bytes, storage_pretty, approach_type, notes)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
  ", params = list(approach, step, duration, rows, storage_bytes, storage_pretty, approach_type, notes))
}

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
cat("STEP 0: Preparing source data (shared)...\n")
tic("source_prep")

# Create source pixel data - reuse from previous test if available
source_exists <- dbExistsTable(con, "temp_source_pixels_complete")
if (!source_exists) {
  # Create city boundary
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_boundary_complete;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_city_boundary_complete AS
    SELECT ST_Union(annulus_geom) as city_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2;
  ", params = list(geoname_id, max_rings))

  # Create temp rasters
  dbExecute(con, "DROP TABLE IF EXISTS temp_pop_rasters_complete;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_pop_rasters_complete AS
    SELECT DISTINCT p.rid, p.rast, p.country_iso3
    FROM temp_city_boundary_complete cb
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(cb.city_geom, p.rast);
  ")

  dbExecute(con, "DROP TABLE IF EXISTS temp_water_rasters_complete;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_water_rasters_complete AS
    SELECT DISTINCT w.rid, w.rast
    FROM temp_city_boundary_complete cb
    CROSS JOIN water_pct w
    WHERE ST_Intersects(cb.city_geom, w.rast);
  ")

  # Extract and merge pixels
  dbExecute(con, "DROP TABLE IF EXISTS temp_source_pixels_complete;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_source_pixels_complete AS
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
      CROSS JOIN temp_pop_rasters_complete p
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
      CROSS JOIN temp_water_rasters_complete w
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
} else {
  cat("  Using existing source data\n")
}

source_time <- toc(quiet = TRUE)
source_duration <- source_time$toc - source_time$tic
source_count <- get_row_count(con, "temp_source_pixels_complete")
cat(sprintf("✓ Source data: %d pixels in %.2fs\n\n", source_count, source_duration))

log_result(con, "SOURCE_PREP", "data_extraction", source_duration, source_count, 0, "N/A", "SHARED", "Source data for all approaches")

# =============================================================================
# RASTER APPROACH 1: BY RING (3-band raster per ring)
# =============================================================================
cat("RASTER APPROACH 1: By Ring (3-band raster per ring)\n")

# Step R1A: Create table
tic("r1a_create_table")
dbExecute(con, "DROP TABLE IF EXISTS raster_approach1_rings;")
dbExecute(con, "
  CREATE TABLE raster_approach1_rings (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    ring_raster RASTER,  -- 3-band: area_fraction, population, water
    pixel_count INTEGER,
    ring_center GEOMETRY(Point, 4326),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")
r1a_time <- toc(quiet = TRUE)
r1a_duration <- r1a_time$toc - r1a_time$tic

# Step R1B: Create rasters using simplified approach (avoid complex raster functions)
tic("r1b_create_rasters")
dbExecute(con, "
  INSERT INTO raster_approach1_rings (geoname_id, ring_number, ring_raster, pixel_count, ring_center)
  SELECT
    $1::BIGINT,
    ring_number,
    -- Create a simple single-band raster with population data
    ST_AsRaster(
      ST_Collect(pixel_center),
      100.0,  -- scalex (100m resolution)
      100.0,  -- scaley (100m resolution)
      ARRAY['32BF']::text[],  -- pixeltype array
      ARRAY[AVG(COALESCE(population_raw, 0))]::double precision[],  -- use AVG for aggregation
      ARRAY[0.0]::double precision[]  -- nodataval array
    ) as ring_raster,
    COUNT(*) as pixel_count,
    ST_Centroid(ST_Collect(pixel_center)) as ring_center
  FROM temp_source_pixels_complete
  WHERE population_raw IS NOT NULL OR water_pct_raw IS NOT NULL
  GROUP BY ring_number
  ORDER BY ring_number;
", params = list(geoname_id))
r1b_time <- toc(quiet = TRUE)
r1b_duration <- r1b_time$toc - r1b_time$tic

# Step R1C: Create indexes
tic("r1c_create_indexes")
dbExecute(con, "CREATE INDEX raster_approach1_geoname_ring_idx ON raster_approach1_rings (geoname_id, ring_number);")
dbExecute(con, "CREATE INDEX raster_approach1_center_idx ON raster_approach1_rings USING GIST (ring_center);")
r1c_time <- toc(quiet = TRUE)
r1c_duration <- r1c_time$toc - r1c_time$tic

r1_rows <- get_row_count(con, "raster_approach1_rings")
r1_size <- get_table_size(con, "raster_approach1_rings")
r1_total_duration <- r1a_duration + r1b_duration + r1c_duration

cat(sprintf("  Create table: %.2fs\n", r1a_duration))
cat(sprintf("  Create rasters: %.2fs\n", r1b_duration))
cat(sprintf("  Create indexes: %.2fs\n", r1c_duration))
cat(sprintf("  TOTAL:        %.2fs\n", r1_total_duration))
cat(sprintf("  Rows:         %d\n", r1_rows))
cat(sprintf("  Storage:      %s (%s bytes)\n\n", r1_size$pretty, format(r1_size$bytes, big.mark=",")))

log_result(con, "RASTER1_BY_RING", "create_table", r1a_duration, 0, 0, "N/A", "RASTER", "Table creation")
log_result(con, "RASTER1_BY_RING", "create_rasters", r1b_duration, r1_rows, 0, "N/A", "RASTER", "Raster creation")
log_result(con, "RASTER1_BY_RING", "create_indexes", r1c_duration, 0, 0, "N/A", "RASTER", "Index creation")
log_result(con, "RASTER1_BY_RING", "TOTAL", r1_total_duration, r1_rows, r1_size$bytes, r1_size$pretty, "RASTER", "Complete approach")

# =============================================================================
# RASTER APPROACH 2: BY TILE-RING (raster per tile per ring)
# =============================================================================
cat("RASTER APPROACH 2: By Tile-Ring (realistic tile counts)\n")

# Step R2A: Create table
tic("r2a_create_table")
dbExecute(con, "DROP TABLE IF EXISTS raster_approach2_tiles;")
dbExecute(con, "
  CREATE TABLE raster_approach2_tiles (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    tile_x INTEGER,
    tile_y INTEGER,
    tile_id VARCHAR(20),
    ring_number INTEGER,
    tile_ring_raster RASTER,  -- 3-band: area_fraction, population, water
    pixel_count INTEGER,
    tile_center GEOMETRY(Point, 4326),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")
r2a_time <- toc(quiet = TRUE)
r2a_duration <- r2a_time$toc - r2a_time$tic

# Step R2B: Create tile-ring rasters (10km tiles)
tic("r2b_create_tile_rasters")
dbExecute(con, "
  INSERT INTO raster_approach2_tiles (geoname_id, tile_x, tile_y, tile_id, ring_number, tile_ring_raster, pixel_count, tile_center)
  SELECT
    $1::BIGINT,
    FLOOR(pixel_x / 100) as tile_x,  -- 10km tiles (100 pixels @ 100m each)
    FLOOR(pixel_y / 100) as tile_y,
    CONCAT('T_', FLOOR(pixel_x / 100), '_', FLOOR(pixel_y / 100)) as tile_id,
    ring_number,
    -- Create simple single-band raster for each tile-ring combination
    ST_AsRaster(
      ST_Collect(pixel_center),
      100.0,  -- scalex (100m resolution)
      100.0,  -- scaley (100m resolution)
      ARRAY['32BF']::text[],  -- pixeltype array
      ARRAY[AVG(COALESCE(population_raw, 0))]::double precision[],  -- use AVG for aggregation
      ARRAY[0.0]::double precision[]  -- nodataval array
    ) as tile_ring_raster,
    COUNT(*) as pixel_count,
    ST_Centroid(ST_Collect(pixel_center)) as tile_center
  FROM temp_source_pixels_complete
  WHERE population_raw IS NOT NULL OR water_pct_raw IS NOT NULL
  GROUP BY FLOOR(pixel_x / 100), FLOOR(pixel_y / 100), ring_number
  HAVING COUNT(*) > 0
  ORDER BY tile_x, tile_y, ring_number;
", params = list(geoname_id))
r2b_time <- toc(quiet = TRUE)
r2b_duration <- r2b_time$toc - r2b_time$tic

# Step R2C: Create indexes
tic("r2c_create_indexes")
dbExecute(con, "CREATE INDEX raster_approach2_geoname_tile_ring_idx ON raster_approach2_tiles (geoname_id, tile_x, tile_y, ring_number);")
dbExecute(con, "CREATE INDEX raster_approach2_center_idx ON raster_approach2_tiles USING GIST (tile_center);")
r2c_time <- toc(quiet = TRUE)
r2c_duration <- r2c_time$toc - r2c_time$tic

r2_rows <- get_row_count(con, "raster_approach2_tiles")
r2_size <- get_table_size(con, "raster_approach2_tiles")
r2_total_duration <- r2a_duration + r2b_duration + r2c_duration

cat(sprintf("  Create table:    %.2fs\n", r2a_duration))
cat(sprintf("  Create rasters:  %.2fs\n", r2b_duration))
cat(sprintf("  Create indexes:  %.2fs\n", r2c_duration))
cat(sprintf("  TOTAL:           %.2fs\n", r2_total_duration))
cat(sprintf("  Rows:            %d\n", r2_rows))
cat(sprintf("  Storage:         %s (%s bytes)\n\n", r2_size$pretty, format(r2_size$bytes, big.mark=",")))

log_result(con, "RASTER2_BY_TILE", "create_table", r2a_duration, 0, 0, "N/A", "RASTER", "Table creation")
log_result(con, "RASTER2_BY_TILE", "create_rasters", r2b_duration, r2_rows, 0, "N/A", "RASTER", "Tile-ring raster creation")
log_result(con, "RASTER2_BY_TILE", "create_indexes", r2c_duration, 0, 0, "N/A", "RASTER", "Index creation")
log_result(con, "RASTER2_BY_TILE", "TOTAL", r2_total_duration, r2_rows, r2_size$bytes, r2_size$pretty, "RASTER", "Complete approach")

# =============================================================================
# TABULAR APPROACHES (from previous test - abbreviated)
# =============================================================================
cat("TABULAR APPROACH 1: Points Detail\n")
tic("t1_total")
dbExecute(con, "DROP TABLE IF EXISTS tabular_approach1_points;")
dbExecute(con, "
  CREATE TABLE tabular_approach1_points (
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
  INSERT INTO tabular_approach1_points (
    geoname_id, ring_number, pixel_center, population_raw, water_pct_raw,
    area_fraction, population_adjusted, water_pct_adjusted, pixel_x, pixel_y
  )
  SELECT
    $1::BIGINT, ring_number, pixel_center, population_raw, water_pct_raw,
    area_fraction,
    COALESCE(population_raw * area_fraction, 0),
    COALESCE(water_pct_raw * area_fraction, 0),
    pixel_x, pixel_y
  FROM temp_source_pixels_complete;
", params = list(geoname_id))

dbExecute(con, "CREATE INDEX tabular_approach1_geoname_ring_idx ON tabular_approach1_points (geoname_id, ring_number);")
dbExecute(con, "CREATE INDEX tabular_approach1_center_idx ON tabular_approach1_points USING GIST (pixel_center);")

t1_time <- toc(quiet = TRUE)
t1_duration <- t1_time$toc - t1_time$tic
t1_rows <- get_row_count(con, "tabular_approach1_points")
t1_size <- get_table_size(con, "tabular_approach1_points")

cat(sprintf("  TOTAL: %.2fs, Rows: %d, Storage: %s\n\n", t1_duration, t1_rows, t1_size$pretty))
log_result(con, "TABULAR1_POINTS", "TOTAL", t1_duration, t1_rows, t1_size$bytes, t1_size$pretty, "TABULAR", "Complete points approach")

cat("TABULAR APPROACH 2: Ring Aggregation\n")
tic("t2_total")
dbExecute(con, "DROP TABLE IF EXISTS tabular_approach2_rings;")
dbExecute(con, "
  CREATE TABLE tabular_approach2_rings (
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
    ring_center GEOMETRY(Point, 4326)
  );
")

dbExecute(con, "
  INSERT INTO tabular_approach2_rings (
    geoname_id, ring_number, pixel_count, total_population, avg_population, max_population,
    total_water_pixels, avg_water_pct, avg_area_fraction, ring_area_km2,
    population_density_per_km2, ring_center
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
    ST_Centroid(ST_Collect(pixel_center)) as ring_center
  FROM temp_source_pixels_complete
  GROUP BY ring_number
  ORDER BY ring_number;
", params = list(geoname_id))

dbExecute(con, "CREATE INDEX tabular_approach2_geoname_ring_idx ON tabular_approach2_rings (geoname_id, ring_number);")

t2_time <- toc(quiet = TRUE)
t2_duration <- t2_time$toc - t2_time$tic
t2_rows <- get_row_count(con, "tabular_approach2_rings")
t2_size <- get_table_size(con, "tabular_approach2_rings")

cat(sprintf("  TOTAL: %.2fs, Rows: %d, Storage: %s\n\n", t2_duration, t2_rows, t2_size$pretty))
log_result(con, "TABULAR2_RINGS", "TOTAL", t2_duration, t2_rows, t2_size$bytes, t2_size$pretty, "TABULAR", "Complete ring aggregation approach")

# =============================================================================
# FINAL COMPREHENSIVE COMPARISON
# =============================================================================
cat("=== COMPREHENSIVE COMPARISON SUMMARY ===\n")
cat(sprintf("%-25s %-8s %10s %8s %15s %15s\n", "Approach", "Type", "Time (s)", "Rows", "Storage", "Bytes"))
cat(sprintf("%-25s %-8s %10s %8s %15s %15s\n", "--------", "----", "--------", "----", "-------", "-----"))
cat(sprintf("%-25s %-8s %10.2f %8d %15s %15s\n", "1. Raster by Ring", "RASTER", r1_total_duration, r1_rows, r1_size$pretty, format(r1_size$bytes, big.mark=",")))
cat(sprintf("%-25s %-8s %10.2f %8d %15s %15s\n", "2. Raster by Tile-Ring", "RASTER", r2_total_duration, r2_rows, r2_size$pretty, format(r2_size$bytes, big.mark=",")))
cat(sprintf("%-25s %-8s %10.2f %8d %15s %15s\n", "3. Tabular Points", "TABULAR", t1_duration, t1_rows, t1_size$pretty, format(t1_size$bytes, big.mark=",")))
cat(sprintf("%-25s %-8s %10.2f %8d %15s %15s\n", "4. Tabular Ring Summary", "TABULAR", t2_duration, t2_rows, t2_size$pretty, format(t2_size$bytes, big.mark=",")))

# Storage efficiency
baseline_bytes <- t1_size$bytes  # Use tabular points as baseline
cat(sprintf("\n=== STORAGE EFFICIENCY vs Tabular Points Baseline ===\n"))
cat(sprintf("Baseline (Tabular Points): %s\n", t1_size$pretty))
cat(sprintf("Raster by Ring:            %.0fx %s (%s)\n", baseline_bytes / r1_size$bytes, ifelse(r1_size$bytes < baseline_bytes, "smaller", "LARGER"), r1_size$pretty))
cat(sprintf("Raster by Tile-Ring:       %.0fx %s (%s)\n", baseline_bytes / r2_size$bytes, ifelse(r2_size$bytes < baseline_bytes, "smaller", "LARGER"), r2_size$pretty))
cat(sprintf("Tabular Ring Summary:      %.0fx smaller (%s)\n", baseline_bytes / t2_size$bytes, t2_size$pretty))

# Global estimates
melbourne_scale <- 1612 * (150/75)^2
cat(sprintf("\n=== GLOBAL ESTIMATES (1612 cities × 150km radius) ===\n"))
cat(sprintf("Raster by Ring:       %s\n", format(r1_size$bytes * melbourne_scale, big.mark=",")))
cat(sprintf("Raster by Tile-Ring:  %s\n", format(r2_size$bytes * melbourne_scale, big.mark=",")))
cat(sprintf("Tabular Points:       %s\n", format(t1_size$bytes * melbourne_scale, big.mark=",")))
cat(sprintf("Tabular Ring Summary: %s\n", format(t2_size$bytes * melbourne_scale, big.mark=",")))

# Export results
results_data <- dbGetQuery(con, "
  SELECT approach_name, step_name, duration_seconds, row_count, storage_bytes, storage_pretty, approach_type, notes
  FROM complete_comparison_results
  ORDER BY test_run, approach_type DESC, approach_name
")

write.csv(results_data, "complete_storage_comparison_results.csv", row.names = FALSE)
cat(sprintf("\n✓ Complete results exported to complete_storage_comparison_results.csv\n"))

# Clean up test tables
dbExecute(con, "DROP TABLE IF EXISTS raster_approach1_rings;")
dbExecute(con, "DROP TABLE IF EXISTS raster_approach2_tiles;")
dbExecute(con, "DROP TABLE IF EXISTS tabular_approach1_points;")
dbExecute(con, "DROP TABLE IF EXISTS tabular_approach2_rings;")

dbDisconnect(con)

cat("\n=== COMPLETE COMPARISON FINISHED ===\n")
cat("All raster and tabular approaches tested with real timing and storage data!\n")