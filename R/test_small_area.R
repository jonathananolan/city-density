# Simple test with very small area - just 3 rings for debugging
library(RPostgres)
library(DBI)

# Database connection
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "worldpop_db",
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)

cat("=== Small Area Test ===\n")

# Test with just 3 rings to see what's happening
test_query <- "
  WITH city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = 14256
      AND ring_number BETWEEN 1 AND 3
  ),

  pixel_data AS (
    SELECT
      r.ring_number,
      ST_PixelAsPolygons(p.rast, 1) as pixel_info
    FROM city_rings r
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(r.annulus_geom, p.rast)
    LIMIT 20
  ),

  pop_pixels AS (
    SELECT
      ring_number,
      (pixel_info).geom as pixel_geom,
      (pixel_info).val as population
    FROM pixel_data
    WHERE (pixel_info).val > 0
  )

  SELECT ring_number, population, ST_AsText(pixel_geom)
  FROM pop_pixels
  ORDER BY ring_number
  LIMIT 10;
"

cat("Testing basic pixel extraction...\n")
start_time <- Sys.time()
result <- dbGetQuery(con, test_query)
end_time <- Sys.time()

cat(sprintf("Query completed in %.1f seconds\n", as.numeric(difftime(end_time, start_time, units = "secs"))))
cat(sprintf("Found %d pixels\n", nrow(result)))

if (nrow(result) > 0) {
  print(head(result))
} else {
  cat("No pixels found! Checking data availability...\n")

  # Check if worldpop data exists
  worldpop_count <- dbGetQuery(con, "SELECT COUNT(*) FROM worldpop_2025")
  cat(sprintf("WorldPop tiles: %d\n", worldpop_count[[1]]))

  # Check if city rings exist
  rings_count <- dbGetQuery(con, "SELECT COUNT(*) FROM city_annulus WHERE geoname_id = 14256 AND ring_number <= 3")
  cat(sprintf("City rings (1-3): %d\n", rings_count[[1]]))
}

dbDisconnect(con)