# Detailed timing breakdown - what takes 1 minute per batch?
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

cat("=== Detailed Timing Breakdown ===\n\n")

# Test with rings 11-15 (typical batch size)
geoname_id <- 14256
ring_start <- 11
ring_end <- 15

cat(sprintf("Testing rings %d-%d for city %d\n\n", ring_start, ring_end, geoname_id))

# Step 1: Just find intersecting rasters
cat("STEP 1: Finding intersecting rasters...\n")
step1_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number BETWEEN $2 AND $3
  )
  SELECT r.ring_number, COUNT(p.rast) as pop_tiles, COUNT(w.rast) as water_tiles
  FROM city_rings r
  LEFT JOIN worldpop_2025 p ON ST_Intersects(r.annulus_geom, p.rast)
  LEFT JOIN water_pct w ON ST_Intersects(r.annulus_geom, w.rast)
  GROUP BY r.ring_number
  ORDER BY r.ring_number;
"

start_time <- Sys.time()
step1_result <- dbGetQuery(con, step1_query, params = list(geoname_id, ring_start, ring_end))
step1_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Time: %.2f seconds\n", step1_time))
print(step1_result)
cat("\n")

# Step 2: Extract population pixels (without filtering)
cat("STEP 2: Extracting population pixels...\n")
step2_query <- "
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
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(r.annulus_geom, p.rast)
  )
  SELECT ring_number, COUNT(*) as pixels
  FROM pixel_data
  GROUP BY ring_number
  ORDER BY ring_number;
"

start_time <- Sys.time()
step2_result <- dbGetQuery(con, step2_query, params = list(geoname_id, ring_start, ring_end))
step2_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Time: %.2f seconds\n", step2_time))
print(step2_result)
cat("\n")

# Step 3: Add filtering and geometry processing
cat("STEP 3: Filtering pixels with population > 0...\n")
step3_query <- "
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
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(r.annulus_geom, p.rast)
  ),
  filtered_pixels AS (
    SELECT
      ring_number,
      (pixel_info).geom as pixel_geom,
      (pixel_info).val as population
    FROM pixel_data
    WHERE (pixel_info).val > 0
  )
  SELECT ring_number, COUNT(*) as pixels_with_population
  FROM filtered_pixels
  GROUP BY ring_number
  ORDER BY ring_number;
"

start_time <- Sys.time()
step3_result <- dbGetQuery(con, step3_query, params = list(geoname_id, ring_start, ring_end))
step3_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Time: %.2f seconds\n", step3_time))
print(step3_result)
cat("\n")

# Step 4: Add area fraction calculations
cat("STEP 4: Area fraction calculations...\n")
step4_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number BETWEEN $2 AND $3
  ),
  pixel_data AS (
    SELECT
      r.ring_number,
      r.annulus_geom,
      ST_PixelAsPolygons(p.rast, 1) as pixel_info
    FROM city_rings r
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(r.annulus_geom, p.rast)
  ),
  processed_pixels AS (
    SELECT
      ring_number,
      annulus_geom,
      (pixel_info).geom as pixel_geom,
      (pixel_info).val as population
    FROM pixel_data
    WHERE (pixel_info).val > 0
  ),
  with_fractions AS (
    SELECT
      ring_number,
      pixel_geom,
      population,
      ST_Area(ST_Intersection(pixel_geom, annulus_geom)) / ST_Area(pixel_geom) as area_fraction
    FROM processed_pixels
    WHERE ST_Intersects(pixel_geom, annulus_geom)
  )
  SELECT ring_number, COUNT(*) as final_pixels, AVG(area_fraction) as avg_fraction
  FROM with_fractions
  WHERE area_fraction > 0
  GROUP BY ring_number
  ORDER BY ring_number;
"

start_time <- Sys.time()
step4_result <- dbGetQuery(con, step4_query, params = list(geoname_id, ring_start, ring_end))
step4_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Time: %.2f seconds\n", step4_time))
print(step4_result)
cat("\n")

# Summary
total_time <- step1_time + step2_time + step3_time + step4_time
cat("=== TIMING BREAKDOWN ===\n")
cat(sprintf("1. Find intersecting rasters:  %6.2fs (%4.1f%%)\n", step1_time, (step1_time/total_time)*100))
cat(sprintf("2. Extract pixels:             %6.2fs (%4.1f%%)\n", step2_time, (step2_time/total_time)*100))
cat(sprintf("3. Filter pixels (val > 0):    %6.2fs (%4.1f%%)\n", step3_time, (step3_time/total_time)*100))
cat(sprintf("4. Calculate area fractions:   %6.2fs (%4.1f%%)\n", step4_time, (step4_time/total_time)*100))
cat(sprintf("   TOTAL:                      %6.2fs\n", total_time))

cat(sprintf("\nIf full insert takes ~60s, the remaining %.2fs is:\n", 60 - total_time))
cat("- INSERT operation and index updates\n")
cat("- Completion record updates\n")
cat("- Transaction overhead\n")

dbDisconnect(con)