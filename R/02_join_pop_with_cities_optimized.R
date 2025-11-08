# Optimized City Annuli Processing - PostgreSQL Query Optimized
# This version focuses on PostgreSQL performance instead of R parallelization

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

# PostgreSQL performance optimization
optimize_postgres_session <- function(con) {
  cat("Optimizing PostgreSQL session for raster operations...\n")

  # Enable parallel workers for complex queries (M2 Mac has 8 cores)
  dbExecute(con, "SET max_parallel_workers_per_gather = 4;")

  # Increase working memory for large raster operations
  dbExecute(con, "SET work_mem = '512MB';")

  # Optimize for raster operations
  dbExecute(con, "SET maintenance_work_mem = '1GB';")

  # Enable parallel processing features
  dbExecute(con, "SET enable_parallel_hash = on;")
  dbExecute(con, "SET enable_parallel_append = on;")

  # SSD optimization (M2 Mac has fast SSD)
  dbExecute(con, "SET random_page_cost = 1.1;")
  dbExecute(con, "SET seq_page_cost = 1.0;")

  # Optimize for large result sets
  dbExecute(con, "SET effective_cache_size = '4GB';")

  cat("✓ PostgreSQL session optimized for M2 Mac\n")
}

# Apply optimizations
optimize_postgres_session(con)

# Import table creation and utility functions from original script
source("R/02_join_pop_with_cities.R", local = TRUE)

# Optimized batch processing function with larger batches
process_city_rings_optimized <- function(con, geoname_id, city_name, ring_start, ring_end, max_rings) {
  cat(sprintf("  Processing rings %d-%d of %d for %s\n", ring_start, ring_end, max_rings, city_name))

  batch_start_time <- Sys.time()

  # Highly optimized query with reduced intermediate steps
  insert_query <- "
    WITH city_rings AS (
      SELECT ring_number, annulus_geom
      FROM city_annulus
      WHERE geoname_id = $1
        AND ring_number BETWEEN $2 AND $3
    ),

    -- Single pass through raster data with spatial optimization
    raster_data AS (
      SELECT
        r.ring_number,
        r.annulus_geom,
        p.rast as pop_rast,
        w.rast as water_rast
      FROM city_rings r
      LEFT JOIN worldpop_2025 p ON ST_Intersects(r.annulus_geom, p.rast)
      LEFT JOIN water_pct w ON ST_Intersects(r.annulus_geom, w.rast)
      WHERE p.rast IS NOT NULL OR w.rast IS NOT NULL
    ),

    -- Extract all pixels in one operation
    all_pixels AS (
      SELECT
        rd.ring_number,
        rd.annulus_geom,
        COALESCE(
          (ST_PixelAsPolygons(rd.pop_rast, 1)).geom,
          (ST_PixelAsPolygons(rd.water_rast, 1)).geom
        ) as pixel_geom,
        COALESCE(
          (ST_PixelAsPolygons(rd.pop_rast, 1)).val,
          0
        ) as population_raw,
        COALESCE(
          (ST_PixelAsPolygons(rd.water_rast, 1)).val,
          0
        ) as water_pct_raw,
        COALESCE(
          (ST_PixelAsPolygons(rd.pop_rast, 1)).x,
          (ST_PixelAsPolygons(rd.water_rast, 1)).x
        ) as pixel_x,
        COALESCE(
          (ST_PixelAsPolygons(rd.pop_rast, 1)).y,
          (ST_PixelAsPolygons(rd.water_rast, 1)).y
        ) as pixel_y
      FROM raster_data rd
      WHERE rd.pop_rast IS NOT NULL OR rd.water_rast IS NOT NULL
    ),

    -- Aggregate pixels and calculate area fractions efficiently
    final_pixels AS (
      SELECT
        ap.ring_number,
        ap.pixel_geom,
        ST_Centroid(ap.pixel_geom) as pixel_center,
        MAX(ap.population_raw) as population_raw,
        MAX(ap.water_pct_raw) as water_pct_raw,
        ap.pixel_x,
        ap.pixel_y,
        -- Optimize area fraction calculation
        CASE
          WHEN ST_Within(ap.pixel_geom, ap.annulus_geom) THEN 1.0
          ELSE ST_Area(ST_Intersection(ap.pixel_geom, ap.annulus_geom)) / ST_Area(ap.pixel_geom)
        END as area_fraction
      FROM all_pixels ap
      WHERE ST_Intersects(ap.pixel_geom, ap.annulus_geom)
        AND (ap.population_raw > 0 OR ap.water_pct_raw >= 0)
      GROUP BY ap.ring_number, ap.pixel_geom, ap.annulus_geom, ap.pixel_x, ap.pixel_y
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
      CASE WHEN population_raw > 0 THEN population_raw ELSE NULL END,
      CASE WHEN water_pct_raw >= 0 THEN water_pct_raw ELSE NULL END,
      area_fraction,
      CASE WHEN population_raw > 0 THEN population_raw * area_fraction ELSE NULL END,
      CASE WHEN water_pct_raw >= 0 THEN water_pct_raw * area_fraction ELSE NULL END,
      pixel_x,
      pixel_y
    FROM final_pixels
    WHERE area_fraction > 0
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

# Optimized city processing with larger batch sizes
process_city_optimized <- function(con, geoname_id, city_name, max_radius_km = 150, batch_rings = 30) {
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

  # Process remaining rings in larger batches for better performance
  for (ring_start in seq(start_ring, max_rings, batch_rings)) {
    ring_end <- min(ring_start + batch_rings - 1, max_rings)

    pixels_added <- process_city_rings_optimized(
      con, geoname_id, city_name, ring_start, ring_end, max_rings
    )

    total_pixels <- total_pixels + pixels_added
  }

  cat(sprintf("  ✓ Completed city %s: %d new pixels processed\n", city_name, total_pixels))
  return(total_pixels)
}

# Optimized main processing function
process_cities_optimized <- function(con, limit_cities = NULL, max_radius_km = 150) {
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

    pixels_added <- process_city_optimized(
      con,
      cities$geoname_id[i],
      cities$city_name[i],
      max_radius_km
    )

    total_processed <- total_processed + pixels_added

    city_duration <- as.numeric(difftime(Sys.time(), city_start, units = "secs"))
    total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    if (pixels_added > 0) {
      cat(sprintf("City %d/%d completed in %.1fs (%.0f pixels/sec). Total: %d pixels in %.1fs\n",
                  i, nrow(cities), city_duration,
                  if(city_duration > 0) pixels_added/city_duration else 0,
                  total_processed, total_duration))
    }
  }

  cat(sprintf("\n✓ Optimized processing complete! Total pixels: %d\n", total_processed))
  return(total_processed)
}

# Main execution
cat("=== Optimized PostgreSQL City Processing ===\n\n")

# Create the tables (reuse function from original script)
create_tables(con)

# Test with one city first, then can scale to all cities
# For testing: limit_cities = 1, max_radius_km = 20
# For production: limit_cities = NULL, max_radius_km = 150
total_pixels <- process_cities_optimized(con, limit_cities = 1, max_radius_km = 20)

# Get final statistics
stats_query <- "
  SELECT
    COUNT(*) as total_pixels,
    COUNT(DISTINCT geoname_id) as cities,
    COUNT(DISTINCT (geoname_id, ring_number)) as city_rings,
    SUM(CASE WHEN population_raw IS NOT NULL THEN 1 ELSE 0 END) as pixels_with_population,
    SUM(CASE WHEN water_pct_raw IS NOT NULL THEN 1 ELSE 0 END) as pixels_with_water,
    ROUND(AVG(area_fraction)::numeric, 3) as avg_area_fraction,
    ROUND(SUM(population_adjusted)::numeric, 0) as total_population_adjusted
  FROM annulus_pixels
"

cat("\n=== Performance Summary ===\n")
stats <- dbGetQuery(con, stats_query)
print(stats)

dbDisconnect(con)
cat("\n✓ Optimized processing completed successfully!\n")