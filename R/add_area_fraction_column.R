# ADD AREA FRACTION COLUMN TO EXISTING SYDNEY_FIXED_WATER TABLE
# Calculate actual area fraction for each pixel and add as raster column

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

sydney_geoname_id <- 2147714

cat("=== ADDING AREA FRACTION COLUMN TO SYDNEY_FIXED_WATER ===\n")
cat("Calculating real area fractions for each pixel\n\n")

tic("area_fraction_column_total")

# Add area fraction column to existing table
dbExecute(con, "ALTER TABLE sydney_fixed_water ADD COLUMN IF NOT EXISTS area_fraction_raster RASTER;")

# Test with just ring 3 first to verify approach
ring_num <- 3
cat(sprintf("Testing area fraction approach with ring %d...\n", ring_num))

tic("ring_3_area_fraction")

# Calculate area fractions using pixel polygon approach
dbExecute(con, "
  WITH ring_geom AS (
    SELECT annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number = $2
  ),

  -- Get the existing population raster for this ring
  existing_pop_raster AS (
    SELECT population_raster
    FROM sydney_fixed_water
    WHERE annulus_number = $2
  ),

  -- Extract each pixel as a polygon from the population raster
  pixel_polygons AS (
    SELECT
      (ST_PixelAsPolygons(population_raster, 1)).x as pixel_x,
      (ST_PixelAsPolygons(population_raster, 1)).y as pixel_y,
      (ST_PixelAsPolygons(population_raster, 1)).val as pop_value,
      (ST_PixelAsPolygons(population_raster, 1)).geom as pixel_geom
    FROM existing_pop_raster
    WHERE population_raster IS NOT NULL
  ),

  -- Calculate area fraction for each pixel
  pixel_area_fractions AS (
    SELECT
      pp.pixel_x,
      pp.pixel_y,
      -- Calculate fraction: intersection area / total pixel area
      LEAST(ST_Area(ST_Intersection(pp.pixel_geom, r.annulus_geom)) / ST_Area(pp.pixel_geom), 1.0) as area_fraction
    FROM pixel_polygons pp, ring_geom r
    WHERE ST_Intersects(pp.pixel_geom, r.annulus_geom)
  ),

  -- Create area fraction raster using pixel geometries and values
  area_fraction_raster AS (
    SELECT
      ST_AsRaster(
        ST_Collect(ST_SetSRID(ST_MakePoint(
          ST_RasterToWorldCoordX((SELECT population_raster FROM existing_pop_raster), pixel_x),
          ST_RasterToWorldCoordY((SELECT population_raster FROM existing_pop_raster), pixel_y)
        ), 4326)),
        (SELECT population_raster FROM existing_pop_raster),  -- Use as reference
        '32BF'::text,
        ARRAY(SELECT area_fraction::float4 FROM pixel_area_fractions ORDER BY pixel_x, pixel_y),
        0
      ) as area_band
    FROM pixel_area_fractions
    LIMIT 1
  )

  UPDATE sydney_fixed_water
  SET area_fraction_raster = (SELECT area_band FROM area_fraction_raster)
  WHERE annulus_number = $2;
", params = list(sydney_geoname_id, ring_num))

ring3_time <- toc(quiet = TRUE)
cat(sprintf("Ring 3 area fraction calculation: %.3fs\n", ring3_time$toc - ring3_time$tic))

# Check if it worked
area_check <- dbGetQuery(con, "
  SELECT
    annulus_number,
    CASE WHEN area_fraction_raster IS NOT NULL THEN 'YES' ELSE 'NO' END as has_area_fraction,
    ROUND((ST_SummaryStats(area_fraction_raster, 1)).mean::numeric, 3) as area_mean,
    ROUND((ST_SummaryStats(area_fraction_raster, 1)).min::numeric, 3) as area_min,
    ROUND((ST_SummaryStats(area_fraction_raster, 1)).max::numeric, 3) as area_max,
    (ST_SummaryStats(area_fraction_raster, 1)).count as pixel_count
  FROM sydney_fixed_water
  WHERE annulus_number = 3;
")

cat("\nRing 3 area fraction results:\n")
print(area_check)

if (area_check$has_area_fraction == "YES" && area_check$area_min < 1.0) {
  cat("\n✓ Area fraction calculation successful! Found edge effects.\n")
  cat("Proceeding to process all remaining rings...\n\n")

  # Get all rings except ring 3 (already done)
  existing_rings <- dbGetQuery(con, "
    SELECT DISTINCT annulus_number
    FROM sydney_fixed_water
    WHERE annulus_number != 3
    ORDER BY annulus_number;
  ")

  for (i in 1:nrow(existing_rings)) {
    ring_num <- existing_rings$annulus_number[i]
    if (ring_num %% 20 == 0) {
      cat(sprintf("Processing ring %d area fraction...\n", ring_num))
    }
    tic(paste0("ring_", ring_num, "_area"))

    # Same area fraction calculation for each ring
    dbExecute(con, "
      WITH ring_geom AS (
        SELECT annulus_geom
        FROM city_annulus
        WHERE geoname_id = $1 AND ring_number = $2
      ),

      existing_pop_raster AS (
        SELECT population_raster
        FROM sydney_fixed_water
        WHERE annulus_number = $2
      ),

      pixel_polygons AS (
        SELECT
          (ST_PixelAsPolygons(population_raster, 1)).x as pixel_x,
          (ST_PixelAsPolygons(population_raster, 1)).y as pixel_y,
          (ST_PixelAsPolygons(population_raster, 1)).val as pop_value,
          (ST_PixelAsPolygons(population_raster, 1)).geom as pixel_geom
        FROM existing_pop_raster
        WHERE population_raster IS NOT NULL
      ),

      pixel_area_fractions AS (
        SELECT
          pp.pixel_x,
          pp.pixel_y,
          LEAST(ST_Area(ST_Intersection(pp.pixel_geom, r.annulus_geom)) / ST_Area(pp.pixel_geom), 1.0) as area_fraction
        FROM pixel_polygons pp, ring_geom r
        WHERE ST_Intersects(pp.pixel_geom, r.annulus_geom)
      ),

      area_fraction_raster AS (
        SELECT
          ST_SetValues(
            (SELECT population_raster FROM existing_pop_raster),
            1,
            ARRAY(SELECT ROW(pixel_x, pixel_y, area_fraction::float4)::geomval FROM pixel_area_fractions)
          ) as area_band
      )

      UPDATE sydney_fixed_water
      SET area_fraction_raster = (SELECT area_band FROM area_fraction_raster)
      WHERE annulus_number = $2;
    ", params = list(sydney_geoname_id, ring_num))

    ring_time <- toc(quiet = TRUE)
    ring_duration <- ring_time$toc - ring_time$tic

    if (ring_num %% 20 == 0) {
      cat(sprintf("  Ring %d: %.3fs\n", ring_num, ring_duration))
    }
  }

} else {
  cat("\n❌ Area fraction calculation failed for ring 3. Check approach.\n")
}

total_time <- toc(quiet = TRUE)

# Check final results
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sydney_fixed_water WHERE area_fraction_raster IS NOT NULL;")[[1]])

cat(sprintf("\n=== AREA FRACTION COLUMN RESULTS ===\n"))
cat(sprintf("✓ Added area fraction to %d rings in %.2fs\n", final_count, total_time$toc - total_time$tic))

# Sample a few rings to verify edge effects
if (final_count > 0) {
  cat("\nSampling area fraction statistics from different rings:\n")
  sample_stats <- dbGetQuery(con, "
    SELECT
      annulus_number,
      ROUND((ST_SummaryStats(area_fraction_raster, 1)).mean::numeric, 3) as area_mean,
      ROUND((ST_SummaryStats(area_fraction_raster, 1)).min::numeric, 3) as area_min,
      ROUND((ST_SummaryStats(area_fraction_raster, 1)).max::numeric, 3) as area_max,
      (ST_SummaryStats(area_fraction_raster, 1)).count as pixel_count
    FROM sydney_fixed_water
    WHERE area_fraction_raster IS NOT NULL
      AND annulus_number IN (3, 10, 25, 50, 100, 150)
    ORDER BY annulus_number;
  ")

  print(sample_stats)

  # Add raster constraints for QGIS
  cat("\nAdding raster constraints for area fraction...\n")
  dbExecute(con, "SELECT AddRasterConstraints('sydney_fixed_water'::name, 'area_fraction_raster'::name);")
}

dbDisconnect(con)

cat("\n=== AREA FRACTION COLUMN COMPLETE ===\n")
cat("Table: sydney_fixed_water now has area_fraction_raster column\n")
cat("Shows actual area fraction (0-1) for each pixel within the annulus\n")
cat("Perfect alignment with population_raster and water_raster\n")