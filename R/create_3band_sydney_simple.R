# CREATE 3-BAND SYDNEY RASTERS - SIMPLIFIED APPROACH
# Use ST_AddBand to combine separate single-band rasters

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
test_rings <- 5  # Start with just 5 rings for testing

cat("=== CREATING 3-BAND SYDNEY RASTERS (SIMPLIFIED) ===\n")
cat("Band 1: Population, Band 2: Water %, Band 3: Area fraction\n")
cat(sprintf("Testing with first %d rings\n\n", test_rings))

tic("sydney_3band_simple")

# Create the table
dbExecute(con, "DROP TABLE IF EXISTS sydney_3band_simple;")
dbExecute(con, "
  CREATE TABLE sydney_3band_simple (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    annulus_number INTEGER,
    annulus_raster RASTER,
    pixel_count INTEGER,
    creation_time_seconds DOUBLE PRECISION
  );
")

# Process each ring
for (ring_num in 1:test_rings) {
  cat(sprintf("Processing ring %d...\n", ring_num))
  tic(paste0("ring_", ring_num))

  # Create 3-band raster using direct approach
  dbExecute(con, "
    WITH ring_geom AS (
      SELECT annulus_geom
      FROM city_annulus
      WHERE geoname_id = $1 AND ring_number = $2
    ),

    -- Band 1: Population raster (clip WorldPop to ring)
    pop_raster AS (
      SELECT ST_Union(ST_Clip(p.rast, r.annulus_geom, true)) as pop_band
      FROM worldpop_2025 p, ring_geom r
      WHERE ST_Intersects(r.annulus_geom, p.rast)
        AND ST_Clip(p.rast, r.annulus_geom, true) IS NOT NULL
    ),

    -- Band 2: Water raster (clip water_pct to same extent)
    water_raster AS (
      SELECT
        CASE
          WHEN COUNT(*) > 0 THEN ST_Union(ST_Clip(w.rast, r.annulus_geom, true))
          ELSE ST_MakeEmptyRaster(
            ST_Width(pr.pop_band),
            ST_Height(pr.pop_band),
            ST_UpperLeftX(pr.pop_band),
            ST_UpperLeftY(pr.pop_band),
            ST_PixelWidth(pr.pop_band),
            ST_PixelHeight(pr.pop_band),
            ST_SkewX(pr.pop_band),
            ST_SkewY(pr.pop_band),
            ST_SRID(pr.pop_band)
          )
        END as water_band
      FROM water_pct w, ring_geom r, pop_raster pr
      WHERE ST_Intersects(r.annulus_geom, w.rast)
    ),

    -- Band 3: Area fraction (simplified to 1.0 for now)
    area_raster AS (
      SELECT
        ST_MapAlgebra(
          pr.pop_band,
          1,
          NULL,
          '[rast] * 0 + 1'::text,
          '32BF'::text
        ) as area_band
      FROM pop_raster pr
    ),

    -- Combine into 3-band raster
    combined_raster AS (
      SELECT
        ST_AddBand(
          ST_AddBand(
            pr.pop_band,
            wr.water_band
          ),
          ar.area_band
        ) as final_raster,
        (ST_SummaryStats(pr.pop_band)).count as pixel_count
      FROM pop_raster pr, water_raster wr, area_raster ar
      WHERE pr.pop_band IS NOT NULL
    )

    INSERT INTO sydney_3band_simple (geoname_id, annulus_number, annulus_raster, pixel_count)
    SELECT
      $1::BIGINT,
      $2,
      final_raster,
      pixel_count::integer
    FROM combined_raster
    WHERE final_raster IS NOT NULL;
  ", params = list(sydney_geoname_id, ring_num))

  ring_time <- toc(quiet = TRUE)
  ring_duration <- ring_time$toc - ring_time$tic

  # Update timing
  dbExecute(con, "
    UPDATE sydney_3band_simple
    SET creation_time_seconds = $1
    WHERE annulus_number = $2;
  ", params = list(ring_duration, ring_num))

  cat(sprintf("  Ring %d: %.3fs\n", ring_num, ring_duration))
}

total_time <- toc(quiet = TRUE)

# Get results
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sydney_3band_simple;")[[1]])
table_size <- dbGetQuery(con, "SELECT pg_size_pretty(pg_total_relation_size('sydney_3band_simple')) as size;")[[1]]

cat(sprintf("\n=== RESULTS ===\n"))
cat(sprintf("✓ Created %d rings in %.2fs\n", final_count, total_time$toc - total_time$tic))
cat(sprintf("✓ Storage: %s\n", table_size))

# Check band structure
if (final_count > 0) {
  band_check <- dbGetQuery(con, "
    SELECT
      annulus_number,
      ST_NumBands(annulus_raster) as num_bands,
      ST_Width(annulus_raster) as width,
      ST_Height(annulus_raster) as height,
      pixel_count,
      ROUND(creation_time_seconds::numeric, 3) as time_sec
    FROM sydney_3band_simple
    ORDER BY annulus_number;
  ")

  cat("\nBand structure check:\n")
  print(band_check)

  # Check band values
  if (nrow(band_check) > 0) {
    band_values <- dbGetQuery(con, "
      SELECT
        annulus_number,
        ROUND((ST_SummaryStats(annulus_raster, 1)).mean::numeric, 2) as band1_pop_mean,
        ROUND((ST_SummaryStats(annulus_raster, 2)).mean::numeric, 2) as band2_water_mean,
        ROUND((ST_SummaryStats(annulus_raster, 3)).mean::numeric, 2) as band3_area_mean
      FROM sydney_3band_simple
      ORDER BY annulus_number;
    ")

    cat("\nBand value check:\n")
    print(band_values)
  }
}

# Add raster constraints for QGIS
cat("Adding raster constraints for QGIS...\n")
dbExecute(con, "SELECT AddRasterConstraints('sydney_3band_simple'::name, 'annulus_raster'::name);")

dbDisconnect(con)

cat("\n=== 3-BAND SYDNEY TEST COMPLETE ===\n")
cat("Table: sydney_3band_simple ready for QGIS testing!\n")