# Test INSERT performance - why is 9000 rows taking 60 seconds?!
library(RPostgres)
library(DBI)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "worldpop_db",
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)

cat("=== INSERT Performance Investigation ===\n\n")

# First, let's see what indexes exist on our table
cat("Current table structure:\n")
table_info <- dbGetQuery(con, "
  SELECT
    indexname,
    indexdef
  FROM pg_indexes
  WHERE tablename = 'annulus_pixels'
  ORDER BY indexname;
")
print(table_info)
cat("\n")

# Test 1: Time just the data preparation (no INSERT)
cat("TEST 1: Data preparation time...\n")
prep_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = 14256 AND ring_number BETWEEN 11 AND 15
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

  pixels_with_fractions AS (
    SELECT
      pp.ring_number,
      pp.pixel_geom,
      pp.pixel_center,
      pp.population,
      pp.pixel_x,
      pp.pixel_y,
      ST_Area(ST_Intersection(pp.pixel_geom, cr.annulus_geom)) / ST_Area(pp.pixel_geom) as area_fraction
    FROM population_pixels pp
    JOIN city_rings cr ON pp.ring_number = cr.ring_number
    WHERE ST_Intersects(pp.pixel_geom, cr.annulus_geom)
  )

  SELECT COUNT(*) as prepared_rows
  FROM pixels_with_fractions
  WHERE area_fraction > 0;
"

start_time <- Sys.time()
prep_result <- dbGetQuery(con, prep_query)
prep_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Prepared %d rows in %.2f seconds\n\n", prep_result[[1]], prep_time))

# Test 2: Create a simple test table without all the indexes
cat("TEST 2: Creating simple test table without indexes...\n")
dbExecute(con, "DROP TABLE IF EXISTS test_insert_simple;")
dbExecute(con, "
  CREATE TABLE test_insert_simple (
    id SERIAL,
    geoname_id BIGINT,
    ring_number INTEGER,
    population_raw DOUBLE PRECISION,
    area_fraction DOUBLE PRECISION
  );
")

# Test simple INSERT performance
simple_insert_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = 14256 AND ring_number BETWEEN 11 AND 15
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
      (pixel_data).val as population,
      (pixel_data).x as pixel_x,
      (pixel_data).y as pixel_y
    FROM population_pixel_data
    WHERE (pixel_data).val > 0
  )

  INSERT INTO test_insert_simple (geoname_id, ring_number, population_raw, area_fraction)
  SELECT
    14256::BIGINT,
    ring_number,
    population,
    0.5  -- fake area fraction
  FROM population_pixels;
"

start_time <- Sys.time()
simple_result <- dbExecute(con, simple_insert_query)
simple_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Inserted %d rows into simple table in %.2f seconds\n", simple_result, simple_time))
cat(sprintf("  Rate: %.0f rows/second\n\n", simple_result/simple_time))

# Test 3: Test INSERT with geometry but no indexes
cat("TEST 3: Creating test table with geometry but no indexes...\n")
dbExecute(con, "DROP TABLE IF EXISTS test_insert_geom;")
dbExecute(con, "
  CREATE TABLE test_insert_geom (
    id SERIAL,
    geoname_id BIGINT,
    ring_number INTEGER,
    pixel_center GEOMETRY(Point, 4326),
    pixel_geom GEOMETRY(Polygon, 4326),
    population_raw DOUBLE PRECISION
  );
")

geom_insert_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = 14256 AND ring_number BETWEEN 11 AND 15
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
      (pixel_data).val as population
    FROM population_pixel_data
    WHERE (pixel_data).val > 0
    LIMIT 1000  -- Limit to 1000 for this test
  )

  INSERT INTO test_insert_geom (geoname_id, ring_number, pixel_center, pixel_geom, population_raw)
  SELECT
    14256::BIGINT,
    ring_number,
    pixel_center,
    pixel_geom,
    population
  FROM population_pixels;
"

start_time <- Sys.time()
geom_result <- dbExecute(con, geom_insert_query)
geom_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Inserted %d rows with geometry in %.2f seconds\n", geom_result, geom_time))
cat(sprintf("  Rate: %.0f rows/second\n\n", geom_result/geom_time))

# Test 4: Check what happens with our actual table indexes
cat("TEST 4: Testing INSERT into actual table structure...\n")

# Delete existing test data
dbExecute(con, "DELETE FROM annulus_pixels WHERE geoname_id = 14256 AND ring_number BETWEEN 16 AND 16;")

actual_insert_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = 14256 AND ring_number = 16
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

  pixels_with_fractions AS (
    SELECT
      pp.ring_number,
      pp.pixel_geom,
      pp.pixel_center,
      pp.population,
      pp.pixel_x,
      pp.pixel_y,
      0.9 as area_fraction  -- Simplified for testing
    FROM population_pixels pp
    JOIN city_rings cr ON pp.ring_number = cr.ring_number
    WHERE ST_Intersects(pp.pixel_geom, cr.annulus_geom)
  )

  INSERT INTO annulus_pixels (
    geoname_id, ring_number, pixel_center, pixel_geom,
    population_raw, water_pct_raw, area_fraction,
    population_adjusted, water_pct_adjusted, pixel_x, pixel_y
  )
  SELECT
    14256::BIGINT,
    ring_number,
    pixel_center,
    pixel_geom,
    population,
    NULL,
    area_fraction,
    population * area_fraction,
    NULL,
    pixel_x,
    pixel_y
  FROM pixels_with_fractions;
"

start_time <- Sys.time()
actual_result <- dbExecute(con, actual_insert_query)
actual_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Inserted %d rows into actual table in %.2f seconds\n", actual_result, actual_time))
cat(sprintf("  Rate: %.0f rows/second\n\n", actual_result/actual_time))

# Summary
cat("=== PERFORMANCE SUMMARY ===\n")
cat(sprintf("Simple table (no geometry):  %.0f rows/sec\n", simple_result/simple_time))
cat(sprintf("Geometry table (no indexes): %.0f rows/sec\n", geom_result/geom_time))
cat(sprintf("Actual table (with indexes): %.0f rows/sec\n", actual_result/actual_time))

# Clean up
dbExecute(con, "DROP TABLE IF EXISTS test_insert_simple;")
dbExecute(con, "DROP TABLE IF EXISTS test_insert_geom;")

dbDisconnect(con)