# TIME SYDNEY TEST - How long does one complete city take?
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

# Sydney parameters
sydney_geoname_id <- 2147714  # Sydney
max_rings <- 150

cat("=== SYDNEY TIMING TEST ===\n")
cat(sprintf("Testing Sydney (ID: %s) for all %d rings\n", sydney_geoname_id, max_rings))

tic("sydney_total")

# Create test table
dbExecute(con, "DROP TABLE IF EXISTS sydney_timing_test;")
dbExecute(con, "
  CREATE TABLE sydney_timing_test (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    annulus_number INTEGER,
    annulus_raster RASTER,
    pixel_count INTEGER,
    creation_time_seconds DOUBLE PRECISION
  );
")

cat("Processing all 150 Sydney rings...\n")

total_ring_time <- 0
rings_processed <- 0

# Process each ring with individual timing
for (ring_num in 1:max_rings) {
  tic(paste0("sydney_ring_", ring_num))

  # Check if this ring exists for Sydney
  ring_exists <- dbGetQuery(con, "
    SELECT COUNT(*) as count
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number = $2;
  ", params = list(sydney_geoname_id, ring_num))

  if (ring_exists$count > 0) {
    # Process ring using direct clipping
    dbExecute(con, "
      WITH ring_geom AS (
        SELECT annulus_geom
        FROM city_annulus
        WHERE geoname_id = $1 AND ring_number = $2
      ),

      intersecting_tiles AS (
        SELECT p.rast
        FROM worldpop_2025 p, ring_geom r
        WHERE ST_Intersects(r.annulus_geom, p.rast)
      ),

      clipped_rasters AS (
        SELECT
          ST_Clip(t.rast, r.annulus_geom, true) as clipped_rast
        FROM intersecting_tiles t, ring_geom r
        WHERE ST_Clip(t.rast, r.annulus_geom, true) IS NOT NULL
      ),

      unioned_raster AS (
        SELECT
          ST_Union(clipped_rast) as final_raster,
          SUM((ST_SummaryStats(clipped_rast)).count) as total_pixels
        FROM clipped_rasters
        WHERE clipped_rast IS NOT NULL
      )

      INSERT INTO sydney_timing_test (geoname_id, annulus_number, annulus_raster, pixel_count)
      SELECT
        $1::BIGINT,
        $2,
        final_raster,
        total_pixels::integer
      FROM unioned_raster
      WHERE final_raster IS NOT NULL;
    ", params = list(sydney_geoname_id, ring_num))

    rings_processed <- rings_processed + 1
  }

  ring_time <- toc(quiet = TRUE)
  ring_duration <- ring_time$toc - ring_time$tic
  total_ring_time <- total_ring_time + ring_duration

  # Update timing if ring was processed
  if (ring_exists$count > 0) {
    dbExecute(con, "
      UPDATE sydney_timing_test
      SET creation_time_seconds = $1
      WHERE annulus_number = $2;
    ", params = list(ring_duration, ring_num))
  }

  # Progress reporting
  if (ring_num %% 20 == 0) {
    avg_time <- total_ring_time / ring_num
    remaining_rings <- max_rings - ring_num
    estimated_remaining <- remaining_rings * avg_time
    cat(sprintf("✓ Processed %d/%d rings (%d created). Avg: %.3fs/ring. Est remaining: %.1fm\n",
                ring_num, max_rings, rings_processed, avg_time, estimated_remaining/60))
  }
}

sydney_total_time <- toc(quiet = TRUE)

# Get final results
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sydney_timing_test;")[[1]])
table_size <- dbGetQuery(con, "SELECT pg_size_pretty(pg_total_relation_size('sydney_timing_test')) as size;")[[1]]

# Summary statistics
avg_time_per_ring <- total_ring_time / max_rings
avg_time_per_created_ring <- if(rings_processed > 0) total_ring_time / rings_processed else 0

cat("\n=== SYDNEY TIMING RESULTS ===\n")
cat(sprintf("✓ Total time: %.2f minutes (%.1fs)\n", (sydney_total_time$toc - sydney_total_time$tic)/60, sydney_total_time$toc - sydney_total_time$tic))
cat(sprintf("✓ Rings processed: %d/%d\n", rings_processed, max_rings))
cat(sprintf("✓ Rings created: %d\n", final_count))
cat(sprintf("✓ Avg time per ring checked: %.3fs\n", avg_time_per_ring))
cat(sprintf("✓ Avg time per ring created: %.3fs\n", avg_time_per_created_ring))
cat(sprintf("✓ Storage: %s\n", table_size))

# Sample raster info
if (final_count > 0) {
  sample_info <- dbGetQuery(con, "
    SELECT
      annulus_number,
      ST_Width(annulus_raster) as width,
      ST_Height(annulus_raster) as height,
      ST_NumBands(annulus_raster) as bands,
      pixel_count,
      ROUND(creation_time_seconds::numeric, 3) as time_sec
    FROM sydney_timing_test
    WHERE annulus_number <= 10
    ORDER BY annulus_number;
  ")

  cat("\nFirst 10 rings sample:\n")
  print(sample_info)
}

# Global estimates
cat(sprintf("\n=== GLOBAL ESTIMATES ===\n"))
cat(sprintf("Time per city: %.2f minutes\n", (sydney_total_time$toc - sydney_total_time$tic)/60))
cat(sprintf("For 1,612 cities: %.1f hours (%.1f days)\n",
            (sydney_total_time$toc - sydney_total_time$tic) * 1612 / 3600,
            (sydney_total_time$toc - sydney_total_time$tic) * 1612 / (3600 * 24)))

# Clean up

dbDisconnect(con)

cat("\n=== SYDNEY TIMING TEST COMPLETE ===\n")