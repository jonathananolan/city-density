# COMPARE RASTER APPROACHES - Pixel Extraction vs Direct Tile Clipping
# Test both methods for creating 3-band annulus rasters

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

# PostgreSQL optimizations
dbExecute(con, "SET work_mem = '512MB';")
dbExecute(con, "SET max_parallel_workers_per_gather = 4;")

# Test parameters
geoname_id <- 2158177  # Melbourne
test_rings <- 10       # Test first 10 rings only for comparison
city_name <- "Melbourne"

cat("=== RASTER APPROACH COMPARISON ===\n")
cat(sprintf("Testing: %s (ID: %s) up to %d rings\n\n", city_name, geoname_id, test_rings))

# Function to get table size
get_table_size <- function(con, table_name) {
  size_query <- sprintf("SELECT pg_size_pretty(pg_total_relation_size('%s')) as size;", table_name)
  tryCatch({
    result <- dbGetQuery(con, size_query)
    return(result$size[1])
  }, error = function(e) {
    return("Table not found")
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

# First, create source data if needed (using ultra-optimized approach)
cat("0. Preparing source data...\n")
tic("0_prepare_data")

# Check if we already have the source data
source_exists <- dbExistsTable(con, "temp_source_pixels")
if (!source_exists) {
  cat("  Creating source pixel data...\n")

  # Create city boundary
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_boundary_comp;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_city_boundary_comp AS
    SELECT ST_Union(annulus_geom) as city_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2;
  ", params = list(geoname_id, max_rings))

  # Create temp rasters
  dbExecute(con, "DROP TABLE IF EXISTS temp_pop_rasters_comp;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_pop_rasters_comp AS
    SELECT DISTINCT p.rid, p.rast, p.country_iso3
    FROM temp_city_boundary_comp cb
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(cb.city_geom, p.rast);
  ")

  dbExecute(con, "DROP TABLE IF EXISTS temp_water_rasters_comp;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_water_rasters_comp AS
    SELECT DISTINCT w.rid, w.rast
    FROM temp_city_boundary_comp cb
    CROSS JOIN water_pct w
    WHERE ST_Intersects(cb.city_geom, w.rast);
  ")

  # Create source pixel data with area fractions
  dbExecute(con, "DROP TABLE IF EXISTS temp_source_pixels;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_source_pixels AS
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
      CROSS JOIN temp_pop_rasters_comp p
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
      CROSS JOIN temp_water_rasters_comp w
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
        ELSE 0.7  -- Edge pixel approximation
      END as area_fraction
    FROM merged_pixels mp
    JOIN city_rings cr ON mp.ring_number = cr.ring_number
    WHERE ST_Intersects(mp.pixel_center, cr.annulus_geom);
  ", params = list(geoname_id, max_rings))

  cat("  ✓ Source data created\n")
} else {
  cat("  ✓ Source data already exists\n")
}

prep_time <- toc(quiet = TRUE)
source_count <- get_row_count(con, "temp_source_pixels")
cat(sprintf("✓ Source data ready: %d pixels in %.2fs\n\n", source_count, prep_time$toc - prep_time$tic))

# =============================================================================
# APPROACH 1: By Annulus (3-band raster per ring)
# =============================================================================
cat("APPROACH 1: By Annulus (3-band raster per ring)\n")
tic("1_by_annulus")

dbExecute(con, "DROP TABLE IF EXISTS raster_by_annulus;")
dbExecute(con, "
  CREATE TABLE raster_by_annulus (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    annulus_raster RASTER,
    pixel_count INTEGER
  );
")

# Create one raster per ring
dbExecute(con, "
  INSERT INTO raster_by_annulus (geoname_id, ring_number, annulus_raster, pixel_count)
  SELECT
    $1::BIGINT,
    ring_number,
    ST_AddBand(
      ST_AddBand(
        ST_AsRaster(
          ST_Collect(pixel_center),
          ARRAY[area_fraction],
          100, 100,
          ARRAY['32BF']
        ),
        ST_AsRaster(
          ST_Collect(pixel_center),
          ARRAY[COALESCE(population_raw, 0)],
          100, 100,
          ARRAY['32BF']
        )
      ),
      ST_AsRaster(
        ST_Collect(pixel_center),
        ARRAY[COALESCE(water_pct_raw, 0)],
        100, 100,
        ARRAY['32BF']
      )
    ) as annulus_raster,
    COUNT(*)
  FROM temp_source_pixels
  WHERE population_raw IS NOT NULL OR water_pct_raw IS NOT NULL
  GROUP BY ring_number;
", params = list(geoname_id))

annulus_time <- toc(quiet = TRUE)
annulus_rows <- get_row_count(con, "raster_by_annulus")
annulus_size <- get_table_size(con, "raster_by_annulus")
cat(sprintf("✓ Created %d annulus rasters in %.2fs\n", annulus_rows, annulus_time$toc - annulus_time$tic))
cat(sprintf("  Storage: %s\n\n", annulus_size))

# =============================================================================
# APPROACH 2: By Tile (identify tiles, create raster per tile per ring)
# =============================================================================
cat("APPROACH 2: By Tile (raster per tile per ring)\n")
tic("2_by_tile")

dbExecute(con, "DROP TABLE IF EXISTS raster_by_tile;")
dbExecute(con, "
  CREATE TABLE raster_by_tile (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    tile_x INTEGER,
    tile_y INTEGER,
    ring_number INTEGER,
    tile_ring_raster RASTER,
    pixel_count INTEGER
  );
")

# Create tile-based rasters (group pixels into 10km tiles)
dbExecute(con, "
  INSERT INTO raster_by_tile (geoname_id, tile_x, tile_y, ring_number, tile_ring_raster, pixel_count)
  SELECT
    $1::BIGINT,
    FLOOR(pixel_x / 100) as tile_x,  -- 10km tiles (100 pixels @ 100m each)
    FLOOR(pixel_y / 100) as tile_y,
    ring_number,
    ST_AddBand(
      ST_AddBand(
        ST_AsRaster(
          ST_Collect(pixel_center),
          ARRAY[area_fraction],
          100, 100,
          ARRAY['32BF']
        ),
        ST_AsRaster(
          ST_Collect(pixel_center),
          ARRAY[COALESCE(population_raw, 0)],
          100, 100,
          ARRAY['32BF']
        )
      ),
      ST_AsRaster(
        ST_Collect(pixel_center),
        ARRAY[COALESCE(water_pct_raw, 0)],
        100, 100,
        ARRAY['32BF']
      )
    ) as tile_ring_raster,
    COUNT(*)
  FROM temp_source_pixels
  WHERE population_raw IS NOT NULL OR water_pct_raw IS NOT NULL
  GROUP BY FLOOR(pixel_x / 100), FLOOR(pixel_y / 100), ring_number
  HAVING COUNT(*) > 0;
", params = list(geoname_id))

tile_time <- toc(quiet = TRUE)
tile_rows <- get_row_count(con, "raster_by_tile")
tile_size <- get_table_size(con, "raster_by_tile")
cat(sprintf("✓ Created %d tile-ring rasters in %.2fs\n", tile_rows, tile_time$toc - tile_time$tic))
cat(sprintf("  Storage: %s\n\n", tile_size))

# =============================================================================
# APPROACH 3: Single Large Raster (all rings in one 4-band raster)
# =============================================================================
cat("APPROACH 3: Single Large Raster (4-band: ring, area_fraction, population, water)\n")
tic("3_single_large")

dbExecute(con, "DROP TABLE IF EXISTS raster_single_large;")
dbExecute(con, "
  CREATE TABLE raster_single_large (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    city_raster RASTER,
    pixel_count INTEGER
  );
")

# Create one large 4-band raster for entire city
dbExecute(con, "
  INSERT INTO raster_single_large (geoname_id, city_raster, pixel_count)
  SELECT
    $1::BIGINT,
    ST_AddBand(
      ST_AddBand(
        ST_AddBand(
          ST_AsRaster(
            ST_Collect(pixel_center),
            ARRAY[ring_number::double precision],  -- Band 1: ring number
            100, 100,
            ARRAY['32BF']
          ),
          ST_AsRaster(
            ST_Collect(pixel_center),
            ARRAY[area_fraction],                   -- Band 2: area fraction
            100, 100,
            ARRAY['32BF']
          )
        ),
        ST_AsRaster(
          ST_Collect(pixel_center),
          ARRAY[COALESCE(population_raw, 0)],      -- Band 3: population
          100, 100,
          ARRAY['32BF']
        )
      ),
      ST_AsRaster(
        ST_Collect(pixel_center),
        ARRAY[COALESCE(water_pct_raw, 0)],         -- Band 4: water
        100, 100,
        ARRAY['32BF']
      )
    ) as city_raster,
    COUNT(*)
  FROM temp_source_pixels
  WHERE population_raw IS NOT NULL OR water_pct_raw IS NOT NULL;
", params = list(geoname_id))

large_time <- toc(quiet = TRUE)
large_rows <- get_row_count(con, "raster_single_large")
large_size <- get_table_size(con, "raster_single_large")
cat(sprintf("✓ Created %d large city raster in %.2fs\n", large_rows, large_time$toc - large_time$tic))
cat(sprintf("  Storage: %s\n\n", large_size))

# =============================================================================
# APPROACH 4: Compressed Tiles (with PostGIS compression)
# =============================================================================
cat("APPROACH 4: Compressed Tile Rasters\n")
tic("4_compressed")

dbExecute(con, "DROP TABLE IF EXISTS raster_compressed_tiles;")
dbExecute(con, "
  CREATE TABLE raster_compressed_tiles (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    tile_id VARCHAR(20),
    tile_raster RASTER,
    ring_count INTEGER,
    pixel_count INTEGER
  );
")

# Enable compression
dbExecute(con, "ALTER TABLE raster_compressed_tiles ALTER COLUMN tile_raster SET COMPRESSION pglz;")

# Create compressed tile rasters (one raster per tile with all rings)
dbExecute(con, "
  INSERT INTO raster_compressed_tiles (geoname_id, tile_id, tile_raster, ring_count, pixel_count)
  SELECT
    $1::BIGINT,
    CONCAT('T_', FLOOR(pixel_x / 100), '_', FLOOR(pixel_y / 100)) as tile_id,
    ST_AddBand(
      ST_AddBand(
        ST_AddBand(
          ST_AsRaster(
            ST_Collect(pixel_center),
            ARRAY[ring_number::double precision],
            100, 100,
            ARRAY['32BF']
          ),
          ST_AsRaster(
            ST_Collect(pixel_center),
            ARRAY[area_fraction],
            100, 100,
            ARRAY['32BF']
          )
        ),
        ST_AsRaster(
          ST_Collect(pixel_center),
          ARRAY[COALESCE(population_raw, 0)],
          100, 100,
          ARRAY['32BF']
        )
      ),
      ST_AsRaster(
        ST_Collect(pixel_center),
        ARRAY[COALESCE(water_pct_raw, 0)],
        100, 100,
        ARRAY['32BF']
      )
    ) as tile_raster,
    COUNT(DISTINCT ring_number) as ring_count,
    COUNT(*) as pixel_count
  FROM temp_source_pixels
  WHERE population_raw IS NOT NULL OR water_pct_raw IS NOT NULL
  GROUP BY FLOOR(pixel_x / 100), FLOOR(pixel_y / 100)
  HAVING COUNT(*) > 0;
", params = list(geoname_id))

compressed_time <- toc(quiet = TRUE)
compressed_rows <- get_row_count(con, "raster_compressed_tiles")
compressed_size <- get_table_size(con, "raster_compressed_tiles")
cat(sprintf("✓ Created %d compressed tile rasters in %.2fs\n", compressed_rows, compressed_time$toc - compressed_time$tic))
cat(sprintf("  Storage: %s\n\n", compressed_size))

# =============================================================================
# SUMMARY COMPARISON
# =============================================================================
cat("=== PERFORMANCE & STORAGE COMPARISON ===\n")
cat(sprintf("%-25s %10s %10s %12s\n", "Approach", "Time (s)", "Rows", "Storage"))
cat(sprintf("%-25s %10s %10s %12s\n", "--------", "--------", "----", "-------"))
cat(sprintf("%-25s %10.2f %10d %12s\n", "1. By Annulus", annulus_time$toc - annulus_time$tic, annulus_rows, annulus_size))
cat(sprintf("%-25s %10.2f %10d %12s\n", "2. By Tile", tile_time$toc - tile_time$tic, tile_rows, tile_size))
cat(sprintf("%-25s %10.2f %10d %12s\n", "3. Single Large", large_time$toc - large_time$tic, large_rows, large_size))
cat(sprintf("%-25s %10.2f %10d %12s\n", "4. Compressed Tiles", compressed_time$toc - compressed_time$tic, compressed_rows, compressed_size))

# Test query performance on each approach
cat("\n=== QUERY PERFORMANCE TEST ===\n")

# Test 1: Get total population for ring 30
test_ring <- 30
cat(sprintf("Testing query: Total population for ring %d\n", test_ring))

# Query Approach 1
cat("1. By Annulus: ")
tic("query_1_annulus")
result1 <- dbGetQuery(con, "
  SELECT ST_SummaryStats(annulus_raster, 2).sum as total_pop
  FROM raster_by_annulus
  WHERE ring_number = $1 AND geoname_id = $2;
", params = list(test_ring, geoname_id))
q1_time <- toc(quiet = TRUE)
cat(sprintf("%.3fs\n", q1_time$toc - q1_time$tic))

# Query Approach 2
cat("2. By Tile: ")
tic("query_2_tile")
result2 <- dbGetQuery(con, "
  SELECT SUM(ST_SummaryStats(tile_ring_raster, 2).sum) as total_pop
  FROM raster_by_tile
  WHERE ring_number = $1 AND geoname_id = $2;
", params = list(test_ring, geoname_id))
q2_time <- toc(quiet = TRUE)
cat(sprintf("%.3fs\n", q2_time$toc - q2_time$tic))

# Query Approach 3
cat("3. Single Large: ")
tic("query_3_large")
result3 <- dbGetQuery(con, "
  SELECT ST_SummaryStats(
    ST_MapAlgebra(city_raster, 1, city_raster, 3, '[rast1] == $1 ? [rast2] : NULL')
  ).sum as total_pop
  FROM raster_single_large
  WHERE geoname_id = $2;
", params = list(test_ring, geoname_id))
q3_time <- toc(quiet = TRUE)
cat(sprintf("%.3fs\n", q3_time$toc - q3_time$tic))

# Query Approach 4
cat("4. Compressed Tiles: ")
tic("query_4_compressed")
result4 <- dbGetQuery(con, "
  SELECT SUM(ST_SummaryStats(
    ST_MapAlgebra(tile_raster, 1, tile_raster, 3, '[rast1] == $1 ? [rast2] : NULL')
  ).sum) as total_pop
  FROM raster_compressed_tiles
  WHERE geoname_id = $2;
", params = list(geoname_id))
q4_time <- toc(quiet = TRUE)
cat(sprintf("%.3fs\n", q4_time$toc - q4_time$tic))

cat("\n=== QUERY PERFORMANCE COMPARISON ===\n")
cat(sprintf("%-25s %10s\n", "Approach", "Query Time"))
cat(sprintf("%-25s %10s\n", "--------", "----------"))
cat(sprintf("%-25s %10.3fs\n", "1. By Annulus", q1_time$toc - q1_time$tic))
cat(sprintf("%-25s %10.3fs\n", "2. By Tile", q2_time$toc - q2_time$tic))
cat(sprintf("%-25s %10.3fs\n", "3. Single Large", q3_time$toc - q3_time$tic))
cat(sprintf("%-25s %10.3fs\n", "4. Compressed Tiles", q4_time$toc - q4_time$tic))

# Cleanup
dbExecute(con, "DROP TABLE IF EXISTS raster_by_annulus;")
dbExecute(con, "DROP TABLE IF EXISTS raster_by_tile;")
dbExecute(con, "DROP TABLE IF EXISTS raster_single_large;")
dbExecute(con, "DROP TABLE IF EXISTS raster_compressed_tiles;")

dbDisconnect(con)

cat("\n=== COMPARISON COMPLETE ===\n")
cat("Check the results above to see which approach offers the best balance of speed and storage!\n")