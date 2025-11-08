# CREATE 3-BAND SYDNEY RASTERS
# Band 1: Population (from WorldPop)
# Band 2: Water percentage (from water_pct table)
# Band 3: Area fraction (calculated from pixel coverage)

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

sydney_geoname_id <- 2147714  # Sydney
max_rings <- 150

cat("=== CREATING 3-BAND SYDNEY RASTERS ===\n")
cat("Band 1: Population, Band 2: Water %, Band 3: Area fraction\n\n")

tic("sydney_3band_total")

# Create the table
dbExecute(con, "DROP TABLE IF EXISTS sydney_3band_rasters;")
dbExecute(con, "
  CREATE TABLE sydney_3band_rasters (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    annulus_number INTEGER,
    annulus_raster RASTER,
    pixel_count INTEGER,
    creation_time_seconds DOUBLE PRECISION,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")

cat("Processing all 150 Sydney rings with 3-band rasters...\n")

total_ring_time <- 0
rings_processed <- 0

# Process each ring with 3-band raster creation
for (ring_num in 1:max_rings) {
  tic(paste0("sydney_3band_ring_", ring_num))

  # Check if this ring exists for Sydney
  ring_exists <- dbGetQuery(con, "
    SELECT COUNT(*) as count
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number = $2;
  ", params = list(sydney_geoname_id, ring_num))

  if (ring_exists$count > 0) {
    # Create 3-band raster using pixel-level approach
    dbExecute(con, "
      WITH ring_geom AS (
        SELECT annulus_geom
        FROM city_annulus
        WHERE geoname_id = $1 AND ring_number = $2
      ),

      -- Get population pixels
      pop_pixels AS (
        SELECT
          (ST_PixelAsCentroids(ST_Clip(p.rast, r.annulus_geom, true), 1)).geom as pixel_center,
          (ST_PixelAsCentroids(ST_Clip(p.rast, r.annulus_geom, true), 1)).val as population,
          (ST_PixelAsCentroids(ST_Clip(p.rast, r.annulus_geom, true), 1)).x as pixel_x,
          (ST_PixelAsCentroids(ST_Clip(p.rast, r.annulus_geom, true), 1)).y as pixel_y
        FROM worldpop_2025 p, ring_geom r
        WHERE ST_Intersects(r.annulus_geom, p.rast)
          AND ST_Clip(p.rast, r.annulus_geom, true) IS NOT NULL
      ),

      -- Get water pixels (align with population grid)
      water_pixels AS (
        SELECT
          pp.pixel_center,
          pp.population,
          pp.pixel_x,
          pp.pixel_y,
          COALESCE(ST_Value(w.rast, pp.pixel_center), 0) as water_pct
        FROM pop_pixels pp, ring_geom r
        LEFT JOIN water_pct w ON ST_Intersects(pp.pixel_center, w.rast)
        WHERE pp.population > 0 OR COALESCE(ST_Value(w.rast, pp.pixel_center), 0) > 0
      ),

      -- Calculate area fractions (simplified to 1.0 for now)
      final_pixels AS (
        SELECT
          pixel_center,
          COALESCE(population, 0) as band1_pop,
          COALESCE(water_pct, 0) as band2_water,
          1.0 as band3_area_frac,
          pixel_x,
          pixel_y
        FROM water_pixels
      ),

      -- Create 3-band raster from collected pixels
      raster_creation AS (
        SELECT
          ST_AddBand(
            ST_AddBand(
              ST_AsRaster(
                ST_Collect(pixel_center),
                100.0, 100.0,
                ARRAY['32BF']::text[],
                ARRAY[AVG(band1_pop)]::double precision[],
                ARRAY[0.0]::double precision[]
              ),
              ST_AsRaster(
                ST_Collect(pixel_center),
                100.0, 100.0,
                ARRAY['32BF']::text[],
                ARRAY[AVG(band2_water)]::double precision[],
                ARRAY[0.0]::double precision[]
              )
            ),
            ST_AsRaster(
              ST_Collect(pixel_center),
              100.0, 100.0,
              ARRAY['32BF']::text[],
              ARRAY[AVG(band3_area_frac)]::double precision[],
              ARRAY[0.0]::double precision[]
            )
          ) as final_raster,
          COUNT(*) as total_pixels
        FROM final_pixels
        WHERE pixel_center IS NOT NULL
      )

      INSERT INTO sydney_3band_rasters (geoname_id, annulus_number, annulus_raster, pixel_count)
      SELECT
        $1::BIGINT,
        $2,
        final_raster,
        total_pixels::integer
      FROM raster_creation
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
      UPDATE sydney_3band_rasters
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
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sydney_3band_rasters;")[[1]])
table_size <- dbGetQuery(con, "SELECT pg_size_pretty(pg_total_relation_size('sydney_3band_rasters')) as size;")[[1]]

# Summary statistics
avg_time_per_ring <- total_ring_time / max_rings
avg_time_per_created_ring <- if(rings_processed > 0) total_ring_time / rings_processed else 0

cat("\n=== SYDNEY 3-BAND RESULTS ===\n")
cat(sprintf("✓ Total time: %.2f minutes (%.1fs)\n", (sydney_total_time$toc - sydney_total_time$tic)/60, sydney_total_time$toc - sydney_total_time$tic))
cat(sprintf("✓ Rings processed: %d/%d\n", rings_processed, max_rings))
cat(sprintf("✓ Rings created: %d\n", final_count))
cat(sprintf("✓ Avg time per ring checked: %.3fs\n", avg_time_per_ring))
cat(sprintf("✓ Avg time per ring created: %.3fs\n", avg_time_per_created_ring))
cat(sprintf("✓ Storage: %s\n", table_size))

# Sample raster info - check band structure
if (final_count > 0) {
  sample_info <- dbGetQuery(con, "
    SELECT
      annulus_number,
      ST_Width(annulus_raster) as width,
      ST_Height(annulus_raster) as height,
      ST_NumBands(annulus_raster) as bands,
      pixel_count,
      ROUND(creation_time_seconds::numeric, 3) as time_sec
    FROM sydney_3band_rasters
    WHERE annulus_number <= 5
    ORDER BY annulus_number;
  ")

  cat("\nFirst 5 rings - 3-band structure:\n")
  print(sample_info)

  # Check individual band statistics
  band_stats <- dbGetQuery(con, "
    SELECT
      annulus_number,
      (ST_SummaryStats(annulus_raster, 1)).mean as band1_pop_mean,
      (ST_SummaryStats(annulus_raster, 2)).mean as band2_water_mean,
      (ST_SummaryStats(annulus_raster, 3)).mean as band3_area_mean
    FROM sydney_3band_rasters
    WHERE annulus_number <= 3
    ORDER BY annulus_number;
  ")

  cat("\nBand value samples (first 3 rings):\n")
  print(band_stats)
}

# Add raster constraints for QGIS
cat("Adding raster constraints for QGIS...\n")
dbExecute(con, "SELECT AddRasterConstraints('sydney_3band_rasters'::name, 'annulus_raster'::name);")

dbDisconnect(con)

cat("\n=== SYDNEY 3-BAND RASTERS COMPLETE ===\n")
cat("Table: sydney_3band_rasters ready for QGIS viewing!\n")