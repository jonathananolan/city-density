# POINTS-ONLY Ultra-Fast Processing - No Polygon Geometries!
# This eliminates the heavy polygon operations for maximum speed

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
cat("Optimizing PostgreSQL session for raster operations...\n")
dbExecute(con, "SET max_parallel_workers_per_gather = 4;")
dbExecute(con, "SET work_mem = '512MB';")
dbExecute(con, "SET maintenance_work_mem = '1GB';")
dbExecute(con, "SET enable_parallel_hash = on;")
dbExecute(con, "SET random_page_cost = 1.1;")
cat("✓ PostgreSQL session optimized\n")

# Create points-only table structure
create_points_table <- function(con) {
  cat("Creating annulus_pixels_points table...\n")

  pixels_exists <- dbExistsTable(con, "annulus_pixels_points")
  if (!pixels_exists) {
    create_query <- "
      CREATE TABLE annulus_pixels_points (
        id SERIAL PRIMARY KEY,
        geoname_id BIGINT NOT NULL,
        ring_number INTEGER NOT NULL,
        pixel_center GEOMETRY(Point, 4326) NOT NULL,
        population_raw DOUBLE PRECISION,
        water_pct_raw DOUBLE PRECISION,
        area_fraction DOUBLE PRECISION NOT NULL,
        population_adjusted DOUBLE PRECISION,
        water_pct_adjusted DOUBLE PRECISION,
        pixel_x INTEGER,
        pixel_y INTEGER,
        UNIQUE (geoname_id, ring_number, pixel_x, pixel_y)
      );
    "
    dbExecute(con, create_query)

    # Create essential indexes (will drop spatial during bulk loading)
    dbExecute(con, "CREATE INDEX IF NOT EXISTS annulus_pixels_points_center_idx ON annulus_pixels_points USING GIST (pixel_center);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS annulus_pixels_points_city_ring_idx ON annulus_pixels_points (geoname_id, ring_number);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS annulus_pixels_points_coords_idx ON annulus_pixels_points (pixel_x, pixel_y);")
    cat("✓ annulus_pixels_points table created\n")
  } else {
    cat("✓ annulus_pixels_points table already exists\n")
  }

  # Ensure completion tracking table exists
  completion_exists <- dbExistsTable(con, "processing_completion_points")
  if (!completion_exists) {
    completion_query <- "
      CREATE TABLE processing_completion_points (
        geoname_id BIGINT NOT NULL,
        city_name VARCHAR(255),
        ring_number INTEGER NOT NULL,
        pixels_processed INTEGER NOT NULL,
        completed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        max_radius_km INTEGER,
        PRIMARY KEY (geoname_id, ring_number)
      );
    "
    dbExecute(con, completion_query)
    cat("✓ processing_completion_points table created\n")
  } else {
    cat("✓ processing_completion_points table already exists\n")
  }
}

# Create city-specific temp tables (same as before)
create_city_temp_tables <- function(con, geoname_id, max_radius_km) {
  cat("Creating temp tables for city...\n")

  # Create temp city boundary
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_boundary;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_city_boundary AS
    SELECT ST_Union(annulus_geom) as city_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2;
  ", params = list(geoname_id, max_radius_km))

  # Create temp population rasters
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_population;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_city_population AS
    SELECT DISTINCT p.rid, p.rast, p.country_iso3
    FROM temp_city_boundary cb
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(cb.city_geom, p.rast);
  ", params = list())

  # Create temp water rasters
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_water;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_city_water AS
    SELECT DISTINCT w.rid, w.rast
    FROM temp_city_boundary cb
    CROSS JOIN water_pct w
    WHERE ST_Intersects(cb.city_geom, w.rast);
  ", params = list())

  # Get stats
  pop_count <- dbGetQuery(con, "SELECT COUNT(*) FROM temp_city_population;")[[1]]
  water_count <- dbGetQuery(con, "SELECT COUNT(*) FROM temp_city_water;")[[1]]

  cat(sprintf("  Population rasters: %d\n", pop_count))
  cat(sprintf("  Water rasters: %d\n", water_count))

  return(list(pop_rasters = pop_count, water_rasters = water_count))
}

# POINTS-ONLY processing function - much faster!
process_city_rings_points_only <- function(con, geoname_id, city_name, ring_start, ring_end, max_rings) {
  cat(sprintf("  Processing rings %d-%d of %d for %s (POINTS ONLY!)\n", ring_start, ring_end, max_rings, city_name))

  batch_start_time <- Sys.time()

  # Ultra-fast points-only query using ST_PixelAsCentroids
  insert_query <- "
    WITH city_rings AS (
      SELECT ring_number, annulus_geom
      FROM city_annulus
      WHERE geoname_id = $1
        AND ring_number BETWEEN $2 AND $3
    ),

    population_point_data AS (
      SELECT
        r.ring_number,
        ST_PixelAsCentroids(p.rast, 1) as pixel_data
      FROM city_rings r
      CROSS JOIN temp_city_population p
      WHERE ST_Intersects(r.annulus_geom, p.rast)
    ),

    population_points AS (
      SELECT
        ring_number,
        (pixel_data).geom as pixel_center,
        (pixel_data).val as population,
        (pixel_data).x as pixel_x,
        (pixel_data).y as pixel_y
      FROM population_point_data
      WHERE (pixel_data).val > 0
    ),

    water_point_data AS (
      SELECT
        r.ring_number,
        ST_PixelAsCentroids(w.rast, 1) as pixel_data
      FROM city_rings r
      CROSS JOIN temp_city_water w
      WHERE ST_Intersects(r.annulus_geom, w.rast)
    ),

    water_points AS (
      SELECT
        ring_number,
        (pixel_data).geom as pixel_center,
        (pixel_data).val as water_pct,
        (pixel_data).x as pixel_x,
        (pixel_data).y as pixel_y
      FROM water_point_data
      WHERE (pixel_data).val >= 0
    ),

    all_points AS (
      SELECT
        ring_number, pixel_center, population,
        NULL::double precision as water_pct, pixel_x, pixel_y
      FROM population_points
      UNION
      SELECT
        ring_number, pixel_center, NULL::double precision as population,
        water_pct, pixel_x, pixel_y
      FROM water_points
    ),

    merged_points AS (
      SELECT
        ring_number,
        pixel_center,
        MAX(population) as population_raw,
        MAX(water_pct) as water_pct_raw,
        MAX(pixel_x) as pixel_x,
        MAX(pixel_y) as pixel_y
      FROM all_points
      GROUP BY ring_number, pixel_center
    ),

    points_with_fractions AS (
      SELECT
        mp.ring_number,
        mp.pixel_center,
        mp.population_raw,
        mp.water_pct_raw,
        mp.pixel_x,
        mp.pixel_y,
        CASE
          WHEN ST_Within(mp.pixel_center, cr.annulus_geom) THEN 1.0
          ELSE 0.8  -- Approximate fraction for edge pixels
        END as area_fraction
      FROM merged_points mp
      JOIN city_rings cr ON mp.ring_number = cr.ring_number
      WHERE ST_Intersects(mp.pixel_center, cr.annulus_geom)
    )

    INSERT INTO annulus_pixels_points (
      geoname_id, ring_number, pixel_center,
      population_raw, water_pct_raw, area_fraction,
      population_adjusted, water_pct_adjusted, pixel_x, pixel_y
    )
    SELECT
      $1::BIGINT as geoname_id,
      ring_number,
      pixel_center,
      population_raw,
      water_pct_raw,
      area_fraction,
      CASE WHEN population_raw IS NOT NULL THEN population_raw * area_fraction ELSE NULL END,
      CASE WHEN water_pct_raw IS NOT NULL THEN water_pct_raw * area_fraction ELSE NULL END,
      pixel_x,
      pixel_y
    FROM points_with_fractions
    WHERE area_fraction > 0
      AND (population_raw IS NOT NULL OR water_pct_raw IS NOT NULL)
    ON CONFLICT (geoname_id, ring_number, pixel_x, pixel_y) DO NOTHING;
  "

  # Execute the ultra-fast points-only insert
  result <- dbExecute(con, insert_query, params = list(geoname_id, ring_start, ring_end))

  batch_duration <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))

  cat(sprintf("    Inserted %d points in %.1fs (%.0f points/sec) - LIGHTNING FAST!\n",
              result, batch_duration,
              if(batch_duration > 0) result/batch_duration else 0))

  # Update completion records
  for (ring_num in ring_start:ring_end) {
    ring_pixels <- dbGetQuery(con, "
      SELECT COUNT(*) as count
      FROM annulus_pixels_points
      WHERE geoname_id = $1 AND ring_number = $2
    ", params = list(geoname_id, ring_num))[[1]]

    dbExecute(con, "
      INSERT INTO processing_completion_points (geoname_id, city_name, ring_number, pixels_processed, max_radius_km)
      VALUES ($1, $2, $3, $4, $5)
      ON CONFLICT (geoname_id, ring_number)
      DO UPDATE SET pixels_processed = $4, completed_at = CURRENT_TIMESTAMP
    ", params = list(geoname_id, city_name, ring_num, ring_pixels, max_rings))
  }

  return(result)
}

# Resumption logic for points processing
get_resumption_point_points <- function(con, geoname_id, max_radius_km) {
  completed_query <- "
    SELECT COALESCE(MAX(ring_number), 0) as last_completed
    FROM processing_completion_points
    WHERE geoname_id = $1 AND max_radius_km = $2
  "
  return(as.integer(dbGetQuery(con, completed_query, params = list(geoname_id, max_radius_km))[[1]]))
}

# Points-only city processing
process_city_points_only <- function(con, geoname_id, city_name, max_radius_km = 150, batch_rings = 20) {
  cat(sprintf("\n⚡ POINTS-ONLY PROCESSING: %s (ID: %s) up to %dkm\n",
              city_name, as.character(geoname_id), max_radius_km))

  city_start_time <- Sys.time()

  # Check resumption point
  last_completed <- get_resumption_point_points(con, geoname_id, max_radius_km)
  max_rings <- as.integer(dbGetQuery(con, "
    SELECT COUNT(*) FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2
  ", params = list(geoname_id, max_radius_km))[[1]])

  if (last_completed >= max_rings) {
    cat(sprintf("  ✓ City already complete (%d/%d rings)\n", last_completed, max_rings))
    return(0)
  }

  start_ring <- last_completed + 1
  cat(sprintf("Processing rings %d-%d\n", start_ring, max_rings))

  # STEP 1: Drop spatial indexes for bulk loading
  cat("\n1. Optimizing table structure...\n")
  dbExecute(con, "DROP INDEX IF EXISTS annulus_pixels_points_center_idx;")
  cat("  ✓ Spatial index dropped\n")

  # STEP 2: Create city temp tables
  cat("\n2. Creating city temp tables...\n")
  temp_stats <- create_city_temp_tables(con, geoname_id, max_radius_km)

  # STEP 3: Process all rings with points-only approach
  cat("\n3. Processing rings with POINTS-ONLY approach...\n")
  total_pixels <- 0

  for (ring_start in seq(start_ring, max_rings, batch_rings)) {
    ring_end <- min(ring_start + batch_rings - 1, max_rings)

    pixels_added <- process_city_rings_points_only(
      con, geoname_id, city_name, ring_start, ring_end, max_rings
    )

    total_pixels <- total_pixels + pixels_added
  }

  # STEP 4: Recreate spatial index
  cat("\n4. Recreating spatial index...\n")
  dbExecute(con, "CREATE INDEX CONCURRENTLY annulus_pixels_points_center_idx ON annulus_pixels_points USING GIST (pixel_center);")
  cat("  ✓ Spatial index recreated\n")

  # Clean up temp tables
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_boundary;")
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_population;")
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_water;")

  city_duration <- as.numeric(difftime(Sys.time(), city_start_time, units = "secs"))

  cat(sprintf("\n✅ City completed: %d points in %.1fs (%.0f points/sec)\n",
              total_pixels, city_duration, total_pixels/city_duration))

  return(total_pixels)
}

# Process all cities with points-only approach
process_all_cities_points <- function(con, max_radius_km = 150, limit_cities = NULL) {
  cities_query <- "
    SELECT DISTINCT geoname_id, city_name
    FROM city_annulus
    ORDER BY geoname_id
  "

  if (!is.null(limit_cities)) {
    cities_query <- paste(cities_query, "LIMIT", limit_cities)
  }

  cities <- dbGetQuery(con, cities_query)
  cat(sprintf("🚀 Found %d cities to process with POINTS-ONLY approach (max %dkm radius)\n",
              nrow(cities), max_radius_km))

  total_processed <- 0
  global_start_time <- Sys.time()

  for (i in 1:nrow(cities)) {
    city_start_time <- Sys.time()

    pixels_added <- process_city_points_only(
      con,
      cities$geoname_id[i],
      cities$city_name[i],
      max_radius_km
    )

    total_processed <- total_processed + pixels_added

    city_duration <- as.numeric(difftime(Sys.time(), city_start_time, units = "mins"))
    total_duration <- as.numeric(difftime(Sys.time(), global_start_time, units = "mins"))

    cat(sprintf("\n✅ CITY %d/%d COMPLETED: %s\n", i, nrow(cities), cities$city_name[i]))
    cat(sprintf("   City time: %.1f minutes\n", city_duration))
    cat(sprintf("   Total time: %.1f minutes (%.1f hours)\n", total_duration, total_duration/60))
    cat(sprintf("   Total points: %d\n", total_processed))
    cat(sprintf("   Remaining cities: %d\n", nrow(cities) - i))
    if (i > 1) {
      avg_time_per_city <- total_duration / i
      est_remaining_time <- avg_time_per_city * (nrow(cities) - i)
      cat(sprintf("   Estimated remaining: %.1f hours\n", est_remaining_time/60))
    }
    cat(sprintf("   ---\n\n"))
  }

  total_time_hours <- as.numeric(difftime(Sys.time(), global_start_time, units = "hours"))

  cat(sprintf("🎉 === POINTS-ONLY PROCESSING COMPLETE! ===\n"))
  cat(sprintf("   Cities processed: %d\n", nrow(cities)))
  cat(sprintf("   Total points: %d\n", total_processed))
  cat(sprintf("   Total time: %.2f hours\n", total_time_hours))
  cat(sprintf("   Average points/city: %.0f\n", total_processed/nrow(cities)))
  cat(sprintf("   Processing rate: %.0f points/hour\n", total_processed/total_time_hours))

  return(total_processed)
}

# MAIN EXECUTION
cat("=== POINTS-ONLY Ultra-Fast Processing ===\n\n")

# Create the points table
create_points_table(con)

# TEST MODE: Process just 1 city first to verify speed improvement
cat("🧪 TESTING with 1 city first...\n")
total_pixels <- process_all_cities_points(con, limit_cities = 1, max_radius_km = 150)

# Get final statistics
cat("\n=== Final Statistics ===\n")
stats <- dbGetQuery(con, "
  SELECT
    COUNT(*) as total_points,
    COUNT(DISTINCT geoname_id) as cities,
    COUNT(DISTINCT (geoname_id, ring_number)) as city_rings,
    SUM(CASE WHEN population_raw IS NOT NULL THEN 1 ELSE 0 END) as points_with_population,
    SUM(CASE WHEN water_pct_raw IS NOT NULL THEN 1 ELSE 0 END) as points_with_water,
    ROUND(AVG(area_fraction)::numeric, 3) as avg_area_fraction,
    ROUND(AVG(CASE WHEN population_raw IS NOT NULL THEN population_raw END)::numeric, 2) as avg_population_raw,
    ROUND(SUM(population_adjusted)::numeric, 0) as total_population_adjusted
  FROM annulus_pixels_points
")
print(stats)

# Show sample data
cat("\n=== Sample Points Data ===\n")
sample_data <- dbGetQuery(con, "
  SELECT
    geoname_id, ring_number, pixel_x, pixel_y,
    population_raw, population_adjusted,
    water_pct_raw, water_pct_adjusted,
    area_fraction
  FROM annulus_pixels_points
  WHERE population_raw IS NOT NULL
  ORDER BY ring_number, pixel_x, pixel_y
  LIMIT 5
")
print(sample_data)

dbDisconnect(con)

cat("\n⚡ POINTS-ONLY processing completed successfully!\n")
cat("This should be significantly faster than polygon processing!\n")