# CREATE 2-BAND SYDNEY RASTERS
# Band 1: Population (from direct clipping)
# Band 2: Area fraction (% of each pixel that intersects the annulus)

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
test_rings <- 5  # Start with 5 rings for testing

cat("=== CREATING 2-BAND SYDNEY RASTERS ===\n")
cat("Band 1: Population, Band 2: Area fraction (% pixel in annulus)\n")
cat(sprintf("Testing with first %d rings\n\n", test_rings))

tic("sydney_2band_total")

# Create the table
dbExecute(con, "DROP TABLE IF EXISTS sydney_2band_area_fraction;")
dbExecute(con, "
  CREATE TABLE sydney_2band_area_fraction (
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

  # Create 2-band raster: population + area fraction
  dbExecute(con, "
    WITH ring_geom AS (
      SELECT annulus_geom
      FROM city_annulus
      WHERE geoname_id = $1 AND ring_number = $2
    ),

    -- Step 1: Create population band (same as before)
    pop_raster AS (
      SELECT ST_Union(ST_Clip(p.rast, r.annulus_geom, true)) as pop_band
      FROM worldpop_2025 p, ring_geom r
      WHERE ST_Intersects(r.annulus_geom, p.rast)
        AND ST_Clip(p.rast, r.annulus_geom, true) IS NOT NULL
    ),

    -- Step 2: Extract pixel centers from the population raster
    pixel_centers AS (
      SELECT
        (ST_PixelAsCentroids(pop_band, 1)).geom as pixel_center,
        (ST_PixelAsCentroids(pop_band, 1)).x as pixel_x,
        (ST_PixelAsCentroids(pop_band, 1)).y as pixel_y,
        (ST_PixelAsCentroids(pop_band, 1)).val as pop_value
      FROM pop_raster
      WHERE pop_band IS NOT NULL
    ),

    -- Step 3: Calculate area fraction for each pixel that's already in the raster
    pixel_area_fractions AS (
      SELECT
        pc.pixel_center,
        pc.pixel_x,
        pc.pixel_y,
        pc.pop_value,
        -- Calculate what % of each 100m x 100m pixel intersects the annulus
        CASE
          WHEN ST_Within(ST_Buffer(pc.pixel_center, 50), r.annulus_geom) THEN 1.0
          ELSE ST_Area(
            ST_Intersection(
              ST_Buffer(pc.pixel_center, 50), -- 100m x 100m pixel
              r.annulus_geom
            )
          ) / 10000.0  -- Divide by 10000 to get fraction (100m x 100m = 10000 m²)
        END as area_fraction
      FROM pixel_centers pc, ring_geom r
      WHERE ST_Intersects(ST_Buffer(pc.pixel_center, 50), r.annulus_geom)
    ),

    -- Step 4: Create area fraction raster aligned to population raster
    area_fraction_raster AS (
      SELECT
        ST_AsRaster(
          ST_Collect(pixel_center),
          (SELECT pop_band FROM pop_raster),  -- Use same template as population
          ARRAY['32BF']::text[],
          ARRAY[area_fraction]::double precision[],
          ARRAY[0.0]::double precision[]
        ) as area_band
      FROM pixel_area_fractions
      WHERE pixel_center IS NOT NULL
    ),

    -- Step 5: Combine into 2-band raster
    combined_raster AS (
      SELECT
        ST_AddBand(pr.pop_band, afr.area_band) as final_raster,
        (ST_SummaryStats(pr.pop_band)).count as pixel_count
      FROM pop_raster pr, area_fraction_raster afr
      WHERE pr.pop_band IS NOT NULL AND afr.area_band IS NOT NULL
    )

    INSERT INTO sydney_2band_area_fraction (geoname_id, annulus_number, annulus_raster, pixel_count)
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
    UPDATE sydney_2band_area_fraction
    SET creation_time_seconds = $1
    WHERE annulus_number = $2;
  ", params = list(ring_duration, ring_num))

  cat(sprintf("  Ring %d: %.3fs\n", ring_num, ring_duration))
}

total_time <- toc(quiet = TRUE)

# Get results
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sydney_2band_area_fraction;")[[1]])
table_size <- dbGetQuery(con, "SELECT pg_size_pretty(pg_total_relation_size('sydney_2band_area_fraction')) as size;")[[1]]

cat(sprintf("\n=== RESULTS ===\n"))
cat(sprintf("✓ Created %d rings in %.2fs\n", final_count, total_time$toc - total_time$tic))
cat(sprintf("✓ Storage: %s\n", table_size))

# Check 2-band structure
if (final_count > 0) {
  band_check <- dbGetQuery(con, "
    SELECT
      annulus_number,
      ST_NumBands(annulus_raster) as num_bands,
      ST_Width(annulus_raster) as width,
      ST_Height(annulus_raster) as height,
      pixel_count,
      ROUND(creation_time_seconds::numeric, 3) as time_sec
    FROM sydney_2band_area_fraction
    ORDER BY annulus_number;
  ")

  cat("\n2-Band structure check:\n")
  print(band_check)

  # Check band values - especially area fractions
  if (nrow(band_check) > 0) {
    band_values <- dbGetQuery(con, "
      SELECT
        annulus_number,
        ROUND((ST_SummaryStats(annulus_raster, 1)).mean::numeric, 2) as band1_pop_mean,
        ROUND((ST_SummaryStats(annulus_raster, 2)).mean::numeric, 3) as band2_area_mean,
        ROUND((ST_SummaryStats(annulus_raster, 2)).min::numeric, 3) as band2_area_min,
        ROUND((ST_SummaryStats(annulus_raster, 2)).max::numeric, 3) as band2_area_max
      FROM sydney_2band_area_fraction
      ORDER BY annulus_number;
    ")

    cat("\nBand values (area fraction stats show edge effects):\n")
    print(band_values)
  }
}

# Add raster constraints for QGIS
cat("Adding raster constraints for QGIS...\n")
dbExecute(con, "SELECT AddRasterConstraints('sydney_2band_area_fraction'::name, 'annulus_raster'::name);")

dbDisconnect(con)

cat("\n=== 2-BAND SYDNEY TEST COMPLETE ===\n")
cat("Table: sydney_2band_area_fraction ready for QGIS!\n")
cat("Band 1: Population density\n")
cat("Band 2: Area fraction (1.0 = full pixel, <1.0 = partial pixel at edges)\n")