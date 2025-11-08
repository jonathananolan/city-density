# Analyze pixel duplication across annuli - is temp table approach worth it?
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

cat("=== Analyzing Pixel Duplication Across Annuli ===\n\n")

# Test with city 14256, rings 1-20 to see duplication patterns
geoname_id <- 14256

# Query 1: How many unique pixels vs total pixels across rings?
cat("1. Checking pixel duplication across rings...\n")
duplication_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number BETWEEN 1 AND 20
  ),

  all_intersecting_rasters AS (
    SELECT DISTINCT
      r.ring_number,
      p.rid as raster_id,
      p.rast
    FROM city_rings r
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(r.annulus_geom, p.rast)
  )

  SELECT
    ring_number,
    COUNT(*) as raster_tiles_per_ring
  FROM all_intersecting_rasters
  GROUP BY ring_number
  ORDER BY ring_number;
"

ring_rasters <- dbGetQuery(con, duplication_query, params = list(geoname_id))
print(ring_rasters)

total_raster_lookups <- sum(ring_rasters$raster_tiles_per_ring)
unique_rasters <- length(unique(dbGetQuery(con, "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number BETWEEN 1 AND 20
  )
  SELECT DISTINCT p.rid
  FROM city_rings r
  CROSS JOIN worldpop_2025 p
  WHERE ST_Intersects(r.annulus_geom, p.rast)
", params = list(geoname_id))$rid))

cat(sprintf("\nDuplication Analysis:\n"))
cat(sprintf("Total raster tile lookups: %d\n", total_raster_lookups))
cat(sprintf("Unique raster tiles: %d\n", unique_rasters))
cat(sprintf("Duplication factor: %.1fx\n", total_raster_lookups / unique_rasters))
cat(sprintf("Wasted work: %.1f%%\n\n", (1 - unique_rasters/total_raster_lookups) * 100))

# Query 2: Test temp table approach timing
cat("2. Testing temp table approach...\n")

# Approach A: Current method (multiple ring queries)
cat("APPROACH A: Current method (process rings 16-20 separately)\n")
start_time <- Sys.time()

for (ring in 16:20) {
  result <- dbGetQuery(con, "
    WITH city_ring AS (
      SELECT annulus_geom
      FROM city_annulus
      WHERE geoname_id = $1 AND ring_number = $2
    )
    SELECT COUNT(*) as pixels
    FROM city_ring r
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(r.annulus_geom, p.rast)
  ", params = list(geoname_id, ring))

  cat(sprintf("  Ring %d: %d intersecting tiles\n", ring, result[[1]]))
}

approach_a_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("  Total time: %.2f seconds\n\n", approach_a_time))

# Approach B: Temp table method
cat("APPROACH B: Temp table method (get all city rasters once)\n")
start_time <- Sys.time()

# Create temp table with all rasters for this city
dbExecute(con, "DROP TABLE IF EXISTS temp_city_rasters;")
dbExecute(con, "
  CREATE TEMP TABLE temp_city_rasters AS
  WITH city_boundary AS (
    SELECT ST_Union(annulus_geom) as city_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number BETWEEN 1 AND 20
  )
  SELECT DISTINCT p.rid, p.rast
  FROM city_boundary cb
  CROSS JOIN worldpop_2025 p
  WHERE ST_Intersects(cb.city_geom, p.rast);
", params = list(geoname_id))

temp_creation_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

# Now query each ring against temp table
ring_start_time <- Sys.time()
for (ring in 16:20) {
  result <- dbGetQuery(con, "
    WITH city_ring AS (
      SELECT annulus_geom
      FROM city_annulus
      WHERE geoname_id = $1 AND ring_number = $2
    )
    SELECT COUNT(*) as pixels
    FROM city_ring r
    CROSS JOIN temp_city_rasters t
    WHERE ST_Intersects(r.annulus_geom, t.rast)
  ", params = list(geoname_id, ring))

  cat(sprintf("  Ring %d: %d intersecting tiles\n", ring, result[[1]]))
}

ring_query_time <- as.numeric(difftime(Sys.time(), ring_start_time, units = "secs"))
approach_b_time <- temp_creation_time + ring_query_time

cat(sprintf("  Temp table creation: %.2f seconds\n", temp_creation_time))
cat(sprintf("  Ring queries: %.2f seconds\n", ring_query_time))
cat(sprintf("  Total time: %.2f seconds\n\n", approach_b_time))

# Query 3: Check temp table size and efficiency
temp_size <- dbGetQuery(con, "SELECT COUNT(*) as raster_count FROM temp_city_rasters;")
cat(sprintf("Temp table contains %d raster tiles\n", temp_size[[1]]))

# Approach C: Single query for all rings
cat("\nAPPROACH C: Single query for all rings (best case)\n")
start_time <- Sys.time()

single_query_result <- dbGetQuery(con, "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number BETWEEN 16 AND 20
  )
  SELECT
    r.ring_number,
    COUNT(*) as intersecting_tiles
  FROM city_rings r
  CROSS JOIN worldpop_2025 p
  WHERE ST_Intersects(r.annulus_geom, p.rast)
  GROUP BY r.ring_number
  ORDER BY r.ring_number;
", params = list(geoname_id))

approach_c_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
print(single_query_result)
cat(sprintf("  Total time: %.2f seconds\n\n", approach_c_time))

# Summary
cat("=== PERFORMANCE COMPARISON ===\n")
cat(sprintf("Approach A (current):     %.2f seconds\n", approach_a_time))
cat(sprintf("Approach B (temp table):  %.2f seconds\n", approach_b_time))
cat(sprintf("Approach C (single query): %.2f seconds\n", approach_c_time))

if (approach_b_time < approach_a_time) {
  cat(sprintf("✓ Temp table is %.1fx FASTER\n", approach_a_time / approach_b_time))
} else {
  cat(sprintf("✗ Temp table is %.1fx SLOWER\n", approach_b_time / approach_a_time))
}

if (approach_c_time < approach_a_time) {
  cat(sprintf("✓ Single query is %.1fx FASTER\n", approach_a_time / approach_c_time))
}

# Cleanup
dbExecute(con, "DROP TABLE IF EXISTS temp_city_rasters;")

dbDisconnect(con)