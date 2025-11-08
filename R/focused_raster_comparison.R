# FOCUSED RASTER COMPARISON - Two Approaches
# 1. Pixel Extraction Method (current approach)
# 2. Direct Tile Clipping Method (alternative)

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

# Test parameters
geoname_id <- 2158177  # Melbourne
test_rings <- 10       # Test first 10 rings for comparison
city_name <- "Melbourne"

cat("=== FOCUSED RASTER APPROACH COMPARISON ===\n")
cat(sprintf("Testing: %s rings 1-%d\n", city_name, test_rings))
cat("Two approaches:\n")
cat("1. mel_pixel_extraction (extract pixels once, then build rasters)\n")
cat("2. mel_direct_clipping (clip tiles directly per annulus)\n\n")

# =============================================================================
# APPROACH 1: PIXEL EXTRACTION METHOD
# =============================================================================
cat("=== APPROACH 1: PIXEL EXTRACTION METHOD ===\n")
cat("Step 1: Extract all pixels once, Step 2: Build rasters from pixels\n")

tic("approach1_total")

# Step 1A: Extract source pixels (shared cost)
cat("Step 1A: Extracting source pixels...\n")
tic("extract_pixels")

dbExecute(con, "DROP TABLE IF EXISTS temp_source_pixels_comparison;")
dbExecute(con, "
  CREATE TEMP TABLE temp_source_pixels_comparison AS
  WITH city_boundary AS (
    SELECT ST_Union(annulus_geom) as city_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2
  ),

  intersecting_tiles AS (
    SELECT DISTINCT p.rid, p.rast
    FROM city_boundary cb
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(cb.city_geom, p.rast)
  ),

  city_rings AS (
    SELECT ring_number, annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2
  ),

  pixel_extraction AS (
    SELECT
      r.ring_number,
      (ST_PixelAsCentroids(t.rast, 1)).geom as pixel_center,
      (ST_PixelAsCentroids(t.rast, 1)).val as population_raw,
      (ST_PixelAsCentroids(t.rast, 1)).x as pixel_x,
      (ST_PixelAsCentroids(t.rast, 1)).y as pixel_y
    FROM city_rings r
    CROSS JOIN intersecting_tiles t
    WHERE ST_Intersects(r.annulus_geom, t.rast)
  )

  SELECT
    ring_number,
    pixel_center,
    population_raw,
    0::double precision as water_pct_raw,  -- Simplified for comparison
    pixel_x,
    pixel_y,
    1.0 as area_fraction  -- Simplified for comparison
  FROM pixel_extraction
  WHERE population_raw > 0;
", params = list(geoname_id, test_rings))

extract_time <- toc(quiet = TRUE)
extract_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_source_pixels_comparison;")[[1]])
cat(sprintf("✓ Extracted %d pixels in %.2fs\n", extract_count, extract_time$toc - extract_time$tic))

# Step 1B: Build rasters from extracted pixels
cat("Step 1B: Building rasters from extracted pixels...\n")
tic("build_rasters_from_pixels")

dbExecute(con, "DROP TABLE IF EXISTS mel_pixel_extraction;")
dbExecute(con, "
  CREATE TABLE mel_pixel_extraction (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    annulus_raster RASTER,
    pixel_count INTEGER,
    creation_time_seconds DOUBLE PRECISION
  );
")

approach1_raster_time <- 0
for (ring_num in 1:test_rings) {
  tic(paste0("ring_", ring_num))

  dbExecute(con, "
    INSERT INTO mel_pixel_extraction (geoname_id, ring_number, annulus_raster, pixel_count)
    SELECT
      $1::BIGINT,
      $2,
      ST_AsRaster(
        ST_Collect(pixel_center),
        100.0, 100.0,
        ARRAY['32BF']::text[],
        ARRAY[AVG(COALESCE(population_raw, 0))]::double precision[],
        ARRAY[0.0]::double precision[]
      ) as annulus_raster,
      COUNT(*) as pixel_count
    FROM temp_source_pixels_comparison
    WHERE ring_number = $2;
  ", params = list(geoname_id, ring_num))

  ring_time <- toc(quiet = TRUE)
  ring_duration <- ring_time$toc - ring_time$tic
  approach1_raster_time <- approach1_raster_time + ring_duration

  dbExecute(con, "
    UPDATE mel_pixel_extraction SET creation_time_seconds = $1 WHERE ring_number = $2;
  ", params = list(ring_duration, ring_num))
}

build_time <- toc(quiet = TRUE)
approach1_total_time <- toc(quiet = TRUE)

approach1_rows <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM mel_pixel_extraction;")[[1]])
approach1_size <- dbGetQuery(con, "SELECT pg_size_pretty(pg_total_relation_size('mel_pixel_extraction')) as size;")[[1]]

cat(sprintf("✓ Built %d rasters in %.2fs\n", approach1_rows, build_time$toc - build_time$tic))
cat(sprintf("✓ Approach 1 total: %.2fs (%.2fs extract + %.2fs build)\n",
            approach1_total_time$toc - approach1_total_time$tic,
            extract_time$toc - extract_time$tic,
            build_time$toc - build_time$tic))
cat(sprintf("  Storage: %s\n\n", approach1_size))

# =============================================================================
# APPROACH 2: DIRECT TILE CLIPPING METHOD
# =============================================================================
cat("=== APPROACH 2: DIRECT TILE CLIPPING METHOD ===\n")
cat("For each annulus: find tiles, clip them, union into single raster\n")

tic("approach2_total")

dbExecute(con, "DROP TABLE IF EXISTS mel_direct_clipping;")
dbExecute(con, "
  CREATE TABLE mel_direct_clipping (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    annulus_raster RASTER,
    pixel_count INTEGER,
    creation_time_seconds DOUBLE PRECISION
  );
")

approach2_total_time <- 0
for (ring_num in 1:test_rings) {
  cat(sprintf("Processing ring %d...\n", ring_num))
  tic(paste0("direct_ring_", ring_num))

  # Direct approach: clip tiles to annulus boundary
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

    INSERT INTO mel_direct_clipping (geoname_id, ring_number, annulus_raster, pixel_count)
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
  approach2_total_time <- approach2_total_time + ring_duration

  dbExecute(con, "
    UPDATE mel_direct_clipping SET creation_time_seconds = $1 WHERE ring_number = $2;
  ", params = list(ring_duration, ring_num))

  cat(sprintf("  Ring %d: %.2fs\n", ring_num, ring_duration))
}

approach2_total <- toc(quiet = TRUE)
approach2_rows <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM mel_direct_clipping;")[[1]])
approach2_size <- dbGetQuery(con, "SELECT pg_size_pretty(pg_total_relation_size('mel_direct_clipping')) as size;")[[1]]

cat(sprintf("✓ Approach 2 total: %.2fs (avg %.2fs per ring)\n",
            approach2_total$toc - approach2_total$tic,
            approach2_total_time / test_rings))
cat(sprintf("  Storage: %s\n\n", approach2_size))

# =============================================================================
# COMPARISON RESULTS
# =============================================================================
cat("=== COMPARISON RESULTS ===\n")
cat(sprintf("%-20s %10s %8s %12s %15s\n", "Approach", "Time (s)", "Rows", "Storage", "Avg/Ring (s)"))
cat(sprintf("%-20s %10s %8s %12s %15s\n", "--------", "--------", "----", "-------", "----------"))
cat(sprintf("%-20s %10.2f %8d %12s %15.3f\n",
            "Pixel Extraction",
            approach1_total_time$toc - approach1_total_time$tic,
            approach1_rows,
            approach1_size,
            approach1_raster_time / test_rings))
cat(sprintf("%-20s %10.2f %8d %12s %15.3f\n",
            "Direct Clipping",
            approach2_total$toc - approach2_total$tic,
            approach2_rows,
            approach2_size,
            approach2_total_time / test_rings))

# Determine winner
if ((approach1_total_time$toc - approach1_total_time$tic) < (approach2_total$toc - approach2_total$tic)) {
  speedup <- (approach2_total$toc - approach2_total$tic) / (approach1_total_time$toc - approach1_total_time$tic)
  cat(sprintf("\n🏆 WINNER: Pixel Extraction (%.1fx faster)\n", speedup))
  cat("- Better for batch processing multiple annuli\n")
  cat("- Upfront pixel extraction cost pays off\n")
} else {
  speedup <- (approach1_total_time$toc - approach1_total_time$tic) / (approach2_total$toc - approach2_total$tic)
  cat(sprintf("\n🏆 WINNER: Direct Clipping (%.1fx faster)\n", speedup))
  cat("- Better for single annulus processing\n")
  cat("- Lower memory footprint\n")
}

# Check raster dimensions
cat("\n=== RASTER QUALITY COMPARISON ===\n")
sample_comparison <- dbGetQuery(con, "
  SELECT
    'Pixel Extraction' as approach,
    ring_number,
    ST_Width(annulus_raster) as width,
    ST_Height(annulus_raster) as height,
    ST_NumBands(annulus_raster) as bands,
    pixel_count
  FROM mel_pixel_extraction
  WHERE ring_number <= 3
  UNION ALL
  SELECT
    'Direct Clipping' as approach,
    ring_number,
    ST_Width(annulus_raster) as width,
    ST_Height(annulus_raster) as height,
    ST_NumBands(annulus_raster) as bands,
    pixel_count
  FROM mel_direct_clipping
  WHERE ring_number <= 3
  ORDER BY ring_number, approach;
")
print(sample_comparison)

cat("\n✓ Both tables created for QGIS testing:\n")
cat("- mel_pixel_extraction\n")
cat("- mel_direct_clipping\n")

dbDisconnect(con)

cat("\n=== COMPARISON COMPLETE ===\n")