# CREATE FULL ANNULUS RASTERS - All 150 rings using direct clipping approach
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

geoname_id <- 2158177  # Melbourne
max_rings <- 150

cat("=== CREATING FULL ANNULUS RASTER TABLE ===\n")
cat(sprintf("Melbourne rings 1-%d using direct clipping approach\n", max_rings))
cat("Table: mel_annulus_pixel_test\n\n")

tic("total_creation")

# Create the table
dbExecute(con, "DROP TABLE IF EXISTS mel_annulus_pixel_test;")
dbExecute(con, "
  CREATE TABLE mel_annulus_pixel_test (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    annulus_number INTEGER,
    annulus_raster RASTER,
    pixel_count INTEGER,
    creation_time_seconds DOUBLE PRECISION,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")

cat("Processing all 150 rings using direct clipping...\n")

# Process each ring
for (ring_num in 1:max_rings) {
  tic(paste0("ring_", ring_num))

  # Direct clipping approach
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
    )

    INSERT INTO mel_annulus_pixel_test (geoname_id, annulus_number, annulus_raster, pixel_count)
    SELECT
      $1::BIGINT,
      $2,
      final_raster,
      total_pixels::integer
    FROM unioned_raster
    WHERE final_raster IS NOT NULL;
  ", params = list(geoname_id, ring_num))

  ring_time <- toc(quiet = TRUE)
  ring_duration <- ring_time$toc - ring_time$tic

  # Update timing
  dbExecute(con, "
    UPDATE mel_annulus_pixel_test
    SET creation_time_seconds = $1
    WHERE annulus_number = $2;
  ", params = list(ring_duration, ring_num))

  # Progress reporting
  if (ring_num %% 20 == 0) {
    cat(sprintf("✓ Completed %d/%d rings\n", ring_num, max_rings))
  }
}

total_time <- toc(quiet = TRUE)

# Add raster constraints for QGIS
cat("Adding raster constraints for QGIS...\n")
dbExecute(con, "SELECT AddRasterConstraints('mel_annulus_pixel_test'::name, 'annulus_raster'::name);")

# Final summary
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM mel_annulus_pixel_test;")[[1]])
table_size <- dbGetQuery(con, "SELECT pg_size_pretty(pg_total_relation_size('mel_annulus_pixel_test')) as size;")[[1]]

cat("\n=== SUMMARY ===\n")
cat(sprintf("✓ Created table: mel_annulus_pixel_test\n"))
cat(sprintf("✓ Rings processed: %d/%d\n", final_count, max_rings))
cat(sprintf("✓ Total time: %.2fs\n", total_time$toc - total_time$tic))
cat(sprintf("✓ Storage: %s\n", table_size))
cat(sprintf("✓ QGIS-ready with raster constraints\n"))

# Sample raster info
sample_info <- dbGetQuery(con, "
  SELECT
    annulus_number,
    ST_Width(annulus_raster) as width,
    ST_Height(annulus_raster) as height,
    ST_NumBands(annulus_raster) as bands,
    pixel_count
  FROM mel_annulus_pixel_test
  WHERE annulus_number <= 5
  ORDER BY annulus_number;
")

cat("\nSample raster dimensions:\n")
print(sample_info)

dbDisconnect(con)

cat("\n=== COMPLETE ===\n")
cat("Table mel_annulus_pixel_test ready for QGIS viewing!\n")