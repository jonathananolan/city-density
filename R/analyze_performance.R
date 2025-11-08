# Performance Analysis - Find the bottleneck
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

# Enable query timing and analysis
dbExecute(con, "SET log_statement_stats = off;")
dbExecute(con, "SET log_parser_stats = off;")
dbExecute(con, "SET log_planner_stats = off;")
dbExecute(con, "SET log_executor_stats = off;")

cat("=== Performance Analysis ===\n\n")

# Test 1: Just finding intersecting rasters (no pixel extraction)
cat("1. Testing raster intersection finding...\n")
test1_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = 14256 AND ring_number BETWEEN 1 AND 3
  )
  SELECT COUNT(*)
  FROM city_rings r
  CROSS JOIN worldpop_2025 p
  WHERE ST_Intersects(r.annulus_geom, p.rast);
"

start_time <- Sys.time()
result1 <- dbGetQuery(con, test1_query)
time1 <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Found %d intersecting raster tiles in %.2fs\n\n", result1[[1]], time1))

# Test 2: Pixel extraction (most expensive step)
cat("2. Testing pixel extraction...\n")
test2_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = 14256 AND ring_number BETWEEN 1 AND 3
  ),
  pixel_data AS (
    SELECT
      r.ring_number,
      ST_PixelAsPolygons(p.rast, 1) as pixel_info
    FROM city_rings r
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(r.annulus_geom, p.rast)
    LIMIT 100
  )
  SELECT COUNT(*) FROM pixel_data;
"

start_time <- Sys.time()
result2 <- dbGetQuery(con, test2_query)
time2 <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Extracted %d pixels in %.2fs\n\n", result2[[1]], time2))

# Test 3: Geometry operations (area fractions)
cat("3. Testing geometry calculations...\n")
test3_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = 14256 AND ring_number BETWEEN 1 AND 3
  ),
  pixel_data AS (
    SELECT
      r.ring_number,
      r.annulus_geom,
      ST_PixelAsPolygons(p.rast, 1) as pixel_info
    FROM city_rings r
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(r.annulus_geom, p.rast)
    LIMIT 50
  ),
  geom_calc AS (
    SELECT
      ring_number,
      (pixel_info).geom as pixel_geom,
      annulus_geom,
      ST_Area(ST_Intersection((pixel_info).geom, annulus_geom)) / ST_Area((pixel_info).geom) as area_fraction
    FROM pixel_data
    WHERE ST_Intersects((pixel_info).geom, annulus_geom)
  )
  SELECT COUNT(*), AVG(area_fraction) FROM geom_calc;
"

start_time <- Sys.time()
result3 <- dbGetQuery(con, test3_query)
time3 <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Calculated area fractions for %d pixels in %.2fs\n\n", result3[[1]], time3))

# Test 4: Full insertion process
cat("4. Testing complete insertion process...\n")
# First clear test data
dbExecute(con, "DELETE FROM annulus_pixels WHERE geoname_id = 14256 AND ring_number BETWEEN 1 AND 2;")

test4_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = 14256 AND ring_number BETWEEN 1 AND 2
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

  INSERT INTO annulus_pixels (
    geoname_id, ring_number, pixel_center, pixel_geom,
    population_raw, water_pct_raw, area_fraction,
    population_adjusted, water_pct_adjusted, pixel_x, pixel_y
  )
  SELECT
    14256::BIGINT as geoname_id,
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
  FROM pixels_with_fractions
  WHERE area_fraction > 0;
"

start_time <- Sys.time()
result4 <- dbExecute(con, test4_query)
time4 <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Inserted %d pixels in %.2fs\n\n", result4, time4))

# Summary
cat("=== Performance Breakdown ===\n")
cat(sprintf("1. Raster intersection: %.2fs\n", time1))
cat(sprintf("2. Pixel extraction:    %.2fs (50-100 pixels)\n", time2))
cat(sprintf("3. Geometry calculations: %.2fs (50 pixels)\n", time3))
cat(sprintf("4. Full process:        %.2fs (%d pixels)\n", time4, result4))

# Calculate bottlenecks
total_test_time <- time1 + time2 + time3 + time4
cat(sprintf("\nBottleneck analysis:\n"))
cat(sprintf("- Raster intersection: %.1f%% of time\n", (time1/total_test_time)*100))
cat(sprintf("- Pixel extraction:    %.1f%% of time\n", (time2/total_test_time)*100))
cat(sprintf("- Geometry calc:       %.1f%% of time\n", (time3/total_test_time)*100))
cat(sprintf("- Full process:        %.1f%% of time\n", (time4/total_test_time)*100))

dbDisconnect(con)