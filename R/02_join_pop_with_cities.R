# Join Population and Water Data with City Annuli - Polygon Approach
# This script creates annulus_pixels table with both point and polygon geometries

# Load required libraries
library(RPostgres)
library(DBI)
library(tidyverse)

# Database configuration
DB_NAME <- "worldpop_db"
DB_HOST <- "localhost"
DB_PORT <- 5432
DB_USER <- Sys.getenv("POSTGRES_USER", "abrey")
DB_PASSWORD <- Sys.getenv("POSTGRES_PASSWORD")

# Connect to database with performance optimizations
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = DB_NAME,
  host = DB_HOST,
  port = DB_PORT,
  user = DB_USER,
  password = DB_PASSWORD
)

# Apply PostgreSQL performance settings for raster operations
optimize_postgres_session <- function(con) {
  cat("Optimizing PostgreSQL session for raster operations...\n")

  # Enable parallel workers for complex queries
  dbExecute(con, "SET max_parallel_workers_per_gather = 4;")

  # Increase working memory for large raster operations
  dbExecute(con, "SET work_mem = '512MB';")

  # Optimize for raster operations
  dbExecute(con, "SET maintenance_work_mem = '1GB';")

  # Enable parallel raster processing
  dbExecute(con, "SET enable_parallel_hash = on;")
  dbExecute(con, "SET enable_parallel_append = on;")

  # Optimize for large result sets
  dbExecute(con, "SET random_page_cost = 1.1;") # SSD optimization

  cat("✓ PostgreSQL session optimized\n")
}

# Apply optimizations
optimize_postgres_session(con)

# Function to create both annulus_pixels and completion tracking tables
create_tables <- function(con) {
  cat("Creating tables...\n")

  # Create annulus_pixels table (don't drop if exists)
  pixels_exists <- dbExistsTable(con, "annulus_pixels")
  if (!pixels_exists) {
    cat("Creating annulus_pixels table...\n")
    create_query <- "
      CREATE TABLE annulus_pixels (
        id SERIAL PRIMARY KEY,
        geoname_id BIGINT NOT NULL,
        ring_number INTEGER NOT NULL,
        pixel_center GEOMETRY(Point, 4326) NOT NULL,
        pixel_geom GEOMETRY(Polygon, 4326) NOT NULL,
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

    # Create indexes
    dbExecute(con, "CREATE INDEX IF NOT EXISTS annulus_pixels_center_idx ON annulus_pixels USING GIST (pixel_center);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS annulus_pixels_geom_idx ON annulus_pixels USING GIST (pixel_geom);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS annulus_pixels_city_ring_idx ON annulus_pixels (geoname_id, ring_number);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS annulus_pixels_coords_idx ON annulus_pixels (pixel_x, pixel_y);")
    cat("✓ annulus_pixels table created\n")
  } else {
    cat("✓ annulus_pixels table already exists\n")
  }

  # Create completion tracking table
  completion_exists <- dbExistsTable(con, "processing_completion")
  if (!completion_exists) {
    cat("Creating processing_completion table...\n")
    completion_query <- "
      CREATE TABLE processing_completion (
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
    dbExecute(con, "CREATE INDEX IF NOT EXISTS completion_city_idx ON processing_completion (geoname_id);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS completion_ring_idx ON processing_completion (ring_number);")
    cat("✓ processing_completion table created\n")
  } else {
    cat("✓ processing_completion table already exists\n")
  }
}

# Function to process a batch of rings for a city
process_city_rings_batch <- function(con, geoname_id, city_name, ring_start, ring_end, max_rings) {
  cat(sprintf("  Processing rings %d-%d of %d for %s\n", ring_start, ring_end, max_rings, city_name))

  batch_start_time <- Sys.time()

  # Main query to extract pixels with population and/or water data
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

  # Execute the insert with parameters
  result <- dbExecute(con, insert_query,
                     params = list(geoname_id, ring_start, ring_end))

  batch_duration <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))

  cat(sprintf("    Inserted %d pixels in %.1fs (%.0f pixels/sec)\n",
              result, batch_duration,
              if(batch_duration > 0) result/batch_duration else 0))

  # Record completion for each ring in this batch
  for (ring_num in ring_start:ring_end) {
    ring_pixels <- dbGetQuery(con, "
      SELECT COUNT(*) as count
      FROM annulus_pixels
      WHERE geoname_id = $1 AND ring_number = $2
    ", params = list(geoname_id, ring_num))[[1]]

    # Insert or update completion record
    dbExecute(con, "
      INSERT INTO processing_completion (geoname_id, city_name, ring_number, pixels_processed, max_radius_km)
      VALUES ($1, $2, $3, $4, $5)
      ON CONFLICT (geoname_id, ring_number)
      DO UPDATE SET pixels_processed = $4, completed_at = CURRENT_TIMESTAMP
    ", params = list(geoname_id, city_name, ring_num, ring_pixels, max_rings))
  }

  return(result)
}

# Function to check what's already been processed for a city
get_resumption_point <- function(con, geoname_id, max_radius_km) {
  # Get the highest completed ring for this city
  completed_query <- "
    SELECT COALESCE(MAX(ring_number), 0) as last_completed
    FROM processing_completion
    WHERE geoname_id = $1 AND max_radius_km = $2
  "
  last_completed <- as.integer(dbGetQuery(con, completed_query, params = list(geoname_id, max_radius_km))[[1]])

  return(last_completed)
}

# Function to clean up partial processing
cleanup_partial_city <- function(con, geoname_id, from_ring) {
  if (from_ring > 0) {
    cat(sprintf("  Cleaning up partial data from ring %d onwards...\n", from_ring + 1))

    # Delete pixels from incomplete rings
    dbExecute(con, "
      DELETE FROM annulus_pixels
      WHERE geoname_id = $1 AND ring_number > $2
    ", params = list(geoname_id, from_ring))

    # Delete completion records from incomplete rings
    dbExecute(con, "
      DELETE FROM processing_completion
      WHERE geoname_id = $1 AND ring_number > $2
    ", params = list(geoname_id, from_ring))
  }
}

# Function to process a single city with optimized batching
process_city <- function(con, geoname_id, city_name, max_radius_km = 150, batch_rings = 20) {
  cat(sprintf("Processing city: %s (ID: %s) up to %dkm\n", city_name, as.character(geoname_id), max_radius_km))

  # Get rings up to max_radius_km
  rings_query <- "
    SELECT COUNT(*) as ring_count
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2
  "
  max_rings <- as.integer(dbGetQuery(con, rings_query, params = list(geoname_id, max_radius_km))[[1]])

  if (max_rings == 0) {
    cat(sprintf("  No rings found for city %s within %dkm\n", as.character(geoname_id), max_radius_km))
    return(0)
  }

  # Check resumption point
  last_completed <- get_resumption_point(con, geoname_id, max_radius_km)

  if (last_completed >= max_rings) {
    cat(sprintf("  ✓ City %s already complete (%d/%d rings)\n", city_name, last_completed, max_rings))
    return(0)
  }

  if (last_completed > 0) {
    cat(sprintf("  Resuming from ring %d (already completed %d/%d rings)\n",
                last_completed + 1, last_completed, max_rings))
  }

  # Clean up any partial processing beyond the last completed ring
  cleanup_partial_city(con, geoname_id, last_completed)

  total_pixels <- 0
  start_ring <- last_completed + 1

  # Process remaining rings in batches
  for (ring_start in seq(start_ring, max_rings, batch_rings)) {
    ring_end <- min(ring_start + batch_rings - 1, max_rings)

    pixels_added <- process_city_rings_batch(
      con, geoname_id, city_name, ring_start, ring_end, max_rings
    )

    total_pixels <- total_pixels + pixels_added
  }

  cat(sprintf("  ✓ Completed city %s: %d new pixels processed\n", city_name, total_pixels))
  return(total_pixels)
}

# Function to process cities with limit
process_cities <- function(con, limit_cities = NULL, max_radius_km = 10) {
  # Get list of cities
  cities_query <- "
    SELECT DISTINCT geoname_id, city_name
    FROM city_annulus
    ORDER BY geoname_id
  "

  if (!is.null(limit_cities)) {
    cities_query <- paste(cities_query, "LIMIT", limit_cities)
  }

  cities <- dbGetQuery(con, cities_query)
  cat(sprintf("Found %d cities to process (max %dkm radius)\n", nrow(cities), max_radius_km))

  total_processed <- 0
  start_time <- Sys.time()

  for (i in 1:nrow(cities)) {
    city_start <- Sys.time()

    pixels_added <- process_city(
      con,
      cities$geoname_id[i],
      cities$city_name[i],
      max_radius_km
    )

    total_processed <- total_processed + pixels_added

    city_duration <- as.numeric(difftime(Sys.time(), city_start, units = "secs"))
    total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    cat(sprintf("City %d/%d completed in %.1fs. Total: %d pixels in %.1fs\n",
                i, nrow(cities), city_duration, total_processed, total_duration))

    # Auto-commit is fine for now
  }

  cat(sprintf("\n✓ Processing complete! Total pixels: %d\n", total_processed))
  return(total_processed)
}

# Function to get statistics
get_statistics <- function(con) {
  stats_query <- "
    SELECT
      COUNT(*) as total_pixels,
      COUNT(DISTINCT geoname_id) as cities,
      COUNT(DISTINCT (geoname_id, ring_number)) as city_rings,
      SUM(CASE WHEN population_raw IS NOT NULL THEN 1 ELSE 0 END) as pixels_with_population,
      SUM(CASE WHEN water_pct_raw IS NOT NULL THEN 1 ELSE 0 END) as pixels_with_water,
      SUM(CASE WHEN population_raw IS NOT NULL AND water_pct_raw IS NOT NULL THEN 1 ELSE 0 END) as pixels_with_both,
      ROUND(AVG(area_fraction)::numeric, 3) as avg_area_fraction,
      ROUND(AVG(CASE WHEN population_raw IS NOT NULL THEN population_raw END)::numeric, 2) as avg_population_raw,
      ROUND(AVG(CASE WHEN population_adjusted IS NOT NULL THEN population_adjusted END)::numeric, 2) as avg_population_adjusted,
      ROUND(SUM(population_adjusted)::numeric, 0) as total_population_adjusted
    FROM annulus_pixels
  "

  return(dbGetQuery(con, stats_query))
}

# Main execution
cat("=== City Annuli Polygon Processing ===\n\n")

# Create the tables (with resumption support)
create_tables(con)

# Process cities - TEST MODE: 1 city, 5km radius for quick feedback
# For production: use limit_cities = NULL, max_radius_km = 150
total_pixels <- process_cities(con, limit_cities = NULL, max_radius_km = 150)

# Get statistics
cat("\n=== Final Statistics ===\n")
stats <- get_statistics(con)
print(stats)

# Show sample data
cat("\n=== Sample Data ===\n")
sample_query <- "
  SELECT
    geoname_id, ring_number, pixel_x, pixel_y,
    population_raw, population_adjusted,
    water_pct_raw, water_pct_adjusted,
    area_fraction
  FROM annulus_pixels
  WHERE population_raw IS NOT NULL
  ORDER BY ring_number, pixel_x, pixel_y
  LIMIT 5
"

sample_data <- dbGetQuery(con, sample_query)
print(sample_data)

# Close connection
dbDisconnect(con)

cat("\n✓ Polygon processing completed successfully!\n")