# Performance Profiling Script - Find the Bottleneck!
# Break down the processing into discrete steps and time each one

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

cat("=== PERFORMANCE PROFILING ===\n\n")

# Test parameters - MUCH LARGER TEST (up to 75km radius)
geoname_id <- 14256  # Our test city
ring_start <- 1
ring_end <- 75  # 75km radius - much more representative!
city_name <- "Āzādshahr"

cat(sprintf("Testing city %s (ID: %s), rings %d-%d (LARGE TEST - up to 75km!)\n\n", city_name, geoname_id, ring_start, ring_end))

# STEP 1: Create city boundary (temp table)
cat("STEP 1: Creating city boundary temp table...\n")
tic("1_city_boundary")
dbExecute(con, "DROP TABLE IF EXISTS temp_city_boundary_profile;")
dbExecute(con, "
  CREATE TEMP TABLE temp_city_boundary_profile AS
  SELECT ST_Union(annulus_geom) as city_geom
  FROM city_annulus
  WHERE geoname_id = $1 AND ring_number BETWEEN $2 AND $3;
", params = list(geoname_id, ring_start, ring_end))
boundary_time <- toc(quiet = TRUE)
cat(sprintf("✓ City boundary created in %.2fs\n\n", boundary_time$toc - boundary_time$tic))

# STEP 2: Find intersecting population rasters
cat("STEP 2: Finding intersecting population rasters...\n")
tic("2_find_pop_rasters")
dbExecute(con, "DROP TABLE IF EXISTS temp_intersecting_pop_rasters;")
dbExecute(con, "
  CREATE TEMP TABLE temp_intersecting_pop_rasters AS
  SELECT DISTINCT p.rid, p.rast, p.country_iso3
  FROM temp_city_boundary_profile cb
  CROSS JOIN worldpop_2025 p
  WHERE ST_Intersects(cb.city_geom, p.rast);
")
pop_raster_time <- toc(quiet = TRUE)
pop_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_intersecting_pop_rasters;")[[1]])
cat(sprintf("✓ Found %d intersecting population rasters in %.2fs\n\n", pop_count, pop_raster_time$toc - pop_raster_time$tic))

# STEP 3: Find intersecting water rasters
cat("STEP 3: Finding intersecting water rasters...\n")
tic("3_find_water_rasters")
dbExecute(con, "DROP TABLE IF EXISTS temp_intersecting_water_rasters;")
dbExecute(con, "
  CREATE TEMP TABLE temp_intersecting_water_rasters AS
  SELECT DISTINCT w.rid, w.rast
  FROM temp_city_boundary_profile cb
  CROSS JOIN water_pct w
  WHERE ST_Intersects(cb.city_geom, w.rast);
")
water_raster_time <- toc(quiet = TRUE)
water_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_intersecting_water_rasters;")[[1]])
cat(sprintf("✓ Found %d intersecting water rasters in %.2fs\n\n", water_count, water_raster_time$toc - water_raster_time$tic))

# STEP 4: Extract population pixels as polygons
cat("STEP 4: Extracting population pixels as polygons...\n")
tic("4_extract_pop_pixels")
dbExecute(con, "DROP TABLE IF EXISTS temp_pop_pixels;")
dbExecute(con, "
  CREATE TEMP TABLE temp_pop_pixels AS
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number BETWEEN $2 AND $3
  ),
  pixel_data AS (
    SELECT
      r.ring_number,
      ST_PixelAsPolygons(p.rast, 1) as pixel_info
    FROM city_rings r
    CROSS JOIN temp_intersecting_pop_rasters p
    WHERE ST_Intersects(r.annulus_geom, p.rast)
  )
  SELECT
    ring_number,
    (pixel_info).geom as pixel_geom,
    ST_Centroid((pixel_info).geom) as pixel_center,
    (pixel_info).val as population,
    (pixel_info).x as pixel_x,
    (pixel_info).y as pixel_y
  FROM pixel_data
  WHERE (pixel_info).val > 0;
", params = list(geoname_id, ring_start, ring_end))
pop_pixel_time <- toc(quiet = TRUE)
pop_pixel_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_pop_pixels;")[[1]])
cat(sprintf("✓ Extracted %d population pixels in %.2fs\n\n", pop_pixel_count, pop_pixel_time$toc - pop_pixel_time$tic))

# STEP 5: Extract water pixels as polygons
cat("STEP 5: Extracting water pixels as polygons...\n")
tic("5_extract_water_pixels")
dbExecute(con, "DROP TABLE IF EXISTS temp_water_pixels;")
dbExecute(con, "
  CREATE TEMP TABLE temp_water_pixels AS
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number BETWEEN $2 AND $3
  ),
  pixel_data AS (
    SELECT
      r.ring_number,
      ST_PixelAsPolygons(w.rast, 1) as pixel_info
    FROM city_rings r
    CROSS JOIN temp_intersecting_water_rasters w
    WHERE ST_Intersects(r.annulus_geom, w.rast)
  )
  SELECT
    ring_number,
    (pixel_info).geom as pixel_geom,
    ST_Centroid((pixel_info).geom) as pixel_center,
    (pixel_info).val as water_pct,
    (pixel_info).x as pixel_x,
    (pixel_info).y as pixel_y
  FROM pixel_data
  WHERE (pixel_info).val >= 0;
", params = list(geoname_id, ring_start, ring_end))
water_pixel_time <- toc(quiet = TRUE)
water_pixel_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_water_pixels;")[[1]])
cat(sprintf("✓ Extracted %d water pixels in %.2fs\n\n", water_pixel_count, water_pixel_time$toc - water_pixel_time$tic))

# STEP 6: Merge population and water pixels
cat("STEP 6: Merging population and water pixels...\n")
tic("6_merge_pixels")
dbExecute(con, "DROP TABLE IF EXISTS temp_merged_pixels;")
dbExecute(con, "
  CREATE TEMP TABLE temp_merged_pixels AS
  WITH all_pixels AS (
    SELECT
      ring_number, pixel_geom, pixel_center, population,
      NULL::double precision as water_pct, pixel_x, pixel_y
    FROM temp_pop_pixels
    UNION
    SELECT
      ring_number, pixel_geom, pixel_center, NULL::double precision as population,
      water_pct, pixel_x, pixel_y
    FROM temp_water_pixels
  )
  SELECT
    ring_number,
    pixel_geom,
    pixel_center,
    MAX(population) as population_raw,
    MAX(water_pct) as water_pct_raw,
    MAX(pixel_x) as pixel_x,
    MAX(pixel_y) as pixel_y
  FROM all_pixels
  GROUP BY ring_number, pixel_geom, pixel_center;
")
merge_time <- toc(quiet = TRUE)
merged_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_merged_pixels;")[[1]])
cat(sprintf("✓ Merged into %d unique pixels in %.2fs\n\n", merged_count, merge_time$toc - merge_time$tic))

# STEP 7: Calculate area fractions (the expensive part?)
cat("STEP 7: Calculating area fractions...\n")
tic("7_area_fractions")
dbExecute(con, "DROP TABLE IF EXISTS temp_pixels_with_fractions;")
dbExecute(con, "
  CREATE TEMP TABLE temp_pixels_with_fractions AS
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number BETWEEN $2 AND $3
  )
  SELECT
    mp.ring_number,
    mp.pixel_geom,
    mp.pixel_center,
    mp.population_raw,
    mp.water_pct_raw,
    mp.pixel_x,
    mp.pixel_y,
    ST_Area(ST_Intersection(mp.pixel_geom, cr.annulus_geom)) / ST_Area(mp.pixel_geom) as area_fraction
  FROM temp_merged_pixels mp
  JOIN city_rings cr ON mp.ring_number = cr.ring_number
  WHERE ST_Intersects(mp.pixel_geom, cr.annulus_geom);
", params = list(geoname_id, ring_start, ring_end))
fraction_time <- toc(quiet = TRUE)
fraction_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_pixels_with_fractions;")[[1]])
cat(sprintf("✓ Calculated area fractions for %d pixels in %.2fs\n\n", fraction_count, fraction_time$toc - fraction_time$tic))

# STEP 8: Filter and prepare final data
cat("STEP 8: Filtering and preparing final data...\n")
tic("8_final_prep")
dbExecute(con, "DROP TABLE IF EXISTS temp_final_pixels;")
dbExecute(con, "
  CREATE TEMP TABLE temp_final_pixels AS
  SELECT
    $1::BIGINT as geoname_id,
    ring_number,
    pixel_center,
    pixel_geom,
    population_raw,
    water_pct_raw,
    area_fraction,
    CASE WHEN population_raw IS NOT NULL THEN population_raw * area_fraction ELSE NULL END as population_adjusted,
    CASE WHEN water_pct_raw IS NOT NULL THEN water_pct_raw * area_fraction ELSE NULL END as water_pct_adjusted,
    pixel_x,
    pixel_y
  FROM temp_pixels_with_fractions
  WHERE area_fraction > 0
    AND (population_raw IS NOT NULL OR water_pct_raw IS NOT NULL);
", params = list(geoname_id))
prep_time <- toc(quiet = TRUE)
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_final_pixels;")[[1]])
cat(sprintf("✓ Prepared %d final pixels in %.2fs\n\n", final_count, prep_time$toc - prep_time$tic))

# STEP 9: INSERT into target table (without indexes for speed)
cat("STEP 9: Creating test table and inserting data...\n")
tic("9_insert")
dbExecute(con, "DROP TABLE IF EXISTS test_performance_insert;")
dbExecute(con, "
  CREATE TABLE test_performance_insert (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    pixel_center GEOMETRY(Point, 4326),
    pixel_geom GEOMETRY(Polygon, 4326),
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
  INSERT INTO test_performance_insert (
    geoname_id, ring_number, pixel_center, pixel_geom,
    population_raw, water_pct_raw, area_fraction,
    population_adjusted, water_pct_adjusted, pixel_x, pixel_y
  )
  SELECT
    geoname_id, ring_number, pixel_center, pixel_geom,
    population_raw, water_pct_raw, area_fraction,
    population_adjusted, water_pct_adjusted, pixel_x, pixel_y
  FROM temp_final_pixels;
")
insert_time <- toc(quiet = TRUE)
insert_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM test_performance_insert;")[[1]])
cat(sprintf("✓ Inserted %d rows in %.2fs\n\n", insert_count, insert_time$toc - insert_time$tic))

# SUMMARY
cat("=== PERFORMANCE SUMMARY ===\n")
total_time <- (boundary_time$toc - boundary_time$tic) +
              (pop_raster_time$toc - pop_raster_time$tic) +
              (water_raster_time$toc - water_raster_time$tic) +
              (pop_pixel_time$toc - pop_pixel_time$tic) +
              (water_pixel_time$toc - water_pixel_time$tic) +
              (merge_time$toc - merge_time$tic) +
              (fraction_time$toc - fraction_time$tic) +
              (prep_time$toc - prep_time$tic) +
              (insert_time$toc - insert_time$tic)

cat(sprintf("1. City boundary:         %6.2fs (%4.1f%%)\\n", boundary_time$toc - boundary_time$tic,
            100 * (boundary_time$toc - boundary_time$tic) / total_time))
cat(sprintf("2. Find pop rasters:      %6.2fs (%4.1f%%)\\n", pop_raster_time$toc - pop_raster_time$tic,
            100 * (pop_raster_time$toc - pop_raster_time$tic) / total_time))
cat(sprintf("3. Find water rasters:    %6.2fs (%4.1f%%)\\n", water_raster_time$toc - water_raster_time$tic,
            100 * (water_raster_time$toc - water_raster_time$tic) / total_time))
cat(sprintf("4. Extract pop pixels:    %6.2fs (%4.1f%%)\\n", pop_pixel_time$toc - pop_pixel_time$tic,
            100 * (pop_pixel_time$toc - pop_pixel_time$tic) / total_time))
cat(sprintf("5. Extract water pixels:  %6.2fs (%4.1f%%)\\n", water_pixel_time$toc - water_pixel_time$tic,
            100 * (water_pixel_time$toc - water_pixel_time$tic) / total_time))
cat(sprintf("6. Merge pixels:          %6.2fs (%4.1f%%)\\n", merge_time$toc - merge_time$tic,
            100 * (merge_time$toc - merge_time$tic) / total_time))
cat(sprintf("7. Calculate fractions:   %6.2fs (%4.1f%%)\\n", fraction_time$toc - fraction_time$tic,
            100 * (fraction_time$toc - fraction_time$tic) / total_time))
cat(sprintf("8. Final preparation:     %6.2fs (%4.1f%%)\\n", prep_time$toc - prep_time$tic,
            100 * (prep_time$toc - prep_time$tic) / total_time))
cat(sprintf("9. Insert into table:     %6.2fs (%4.1f%%)\\n", insert_time$toc - insert_time$tic,
            100 * (insert_time$toc - insert_time$tic) / total_time))
cat(sprintf("   %-21s %6.2fs (100.0%%)\\n", "TOTAL:", total_time))

cat(sprintf("\\nFinal result: %d pixels processed in %.2fs\\n", insert_count, total_time))
cat(sprintf("Processing rate: %.0f pixels/second\\n", insert_count / total_time))

# Clean up
dbExecute(con, "DROP TABLE IF EXISTS test_performance_insert;")

dbDisconnect(con)

cat("\\n=== Analysis Complete ===\\n")
cat("Look at the percentages above to see which step is the bottleneck!\\n")