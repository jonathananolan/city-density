# CREATE PIXEL AREA FRACTION RASTER
# Calculate what fraction (0-1) of each population pixel is within the annulus

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

cat("=== CREATING PIXEL AREA FRACTION RASTER ===\n")
cat("For each population pixel, calculate what fraction is within the annulus\n\n")

# Test with ring 3 first
ring_num <- 3
cat(sprintf("Testing with ring %d...\n", ring_num))

tic("area_fraction_test")

# Create a simple test table
dbExecute(con, "DROP TABLE IF EXISTS sydney_area_fraction_test;")
dbExecute(con, "
  CREATE TABLE sydney_area_fraction_test (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    annulus_number INTEGER,
    population_raster RASTER,
    area_fraction_raster RASTER,
    pixel_count INTEGER
  );
")

# Calculate area fractions using a different approach
dbExecute(con, "
  WITH ring_geom AS (
    SELECT annulus_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number = $2
  ),

  -- Get the population raster for this ring
  pop_raster AS (
    SELECT ST_Union(ST_Clip(p.rast, r.annulus_geom, true)) as pop_band
    FROM worldpop_2025 p, ring_geom r
    WHERE ST_Intersects(r.annulus_geom, p.rast)
      AND ST_Clip(p.rast, r.annulus_geom, true) IS NOT NULL
  ),

  -- Extract each pixel as a polygon and calculate area intersection
  pixel_polygons AS (
    SELECT
      (ST_PixelAsPolygons(pop_band, 1)).x as pixel_x,
      (ST_PixelAsPolygons(pop_band, 1)).y as pixel_y,
      (ST_PixelAsPolygons(pop_band, 1)).val as pop_value,
      (ST_PixelAsPolygons(pop_band, 1)).geom as pixel_geom
    FROM pop_raster
    WHERE pop_band IS NOT NULL
  ),

  -- Calculate area fraction for each pixel
  pixel_area_fractions AS (
    SELECT
      pp.pixel_x,
      pp.pixel_y,
      pp.pop_value,
      pp.pixel_geom,
      ST_Centroid(pp.pixel_geom) as pixel_center,
      -- Calculate fraction: intersection area / total pixel area
      ST_Area(ST_Intersection(pp.pixel_geom, r.annulus_geom)) / ST_Area(pp.pixel_geom) as area_fraction
    FROM pixel_polygons pp, ring_geom r
    WHERE ST_Intersects(pp.pixel_geom, r.annulus_geom)
  ),

  -- Create area fraction raster from the calculated fractions
  area_fraction_raster AS (
    SELECT
      ST_SetValues(
        (SELECT pop_band FROM pop_raster),  -- Use population raster as template
        1,  -- Band 1
        ARRAY(SELECT ROW(pixel_x, pixel_y, area_fraction)::geomval FROM pixel_area_fractions)
      ) as area_band
    FROM pixel_area_fractions
    LIMIT 1
  )

  INSERT INTO sydney_area_fraction_test (
    geoname_id, annulus_number, population_raster, area_fraction_raster, pixel_count
  )
  SELECT
    $1::BIGINT,
    $2,
    pr.pop_band,
    afr.area_band,
    (ST_SummaryStats(pr.pop_band)).count::integer
  FROM pop_raster pr, area_fraction_raster afr
  WHERE pr.pop_band IS NOT NULL AND afr.area_band IS NOT NULL;
", params = list(sydney_geoname_id, ring_num))

test_time <- toc(quiet = TRUE)

# Check results
result_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sydney_area_fraction_test;")[[1]])

cat(sprintf("\\n=== AREA FRACTION TEST RESULTS ===\\n"))
cat(sprintf("✓ Created test in %.2fs\\n", test_time$toc - test_time$tic))
cat(sprintf("✓ Records created: %d\\n", result_count))

if (result_count > 0) {
  # Check the area fraction values
  area_stats <- dbGetQuery(con, "
    SELECT
      annulus_number,
      ROUND((ST_SummaryStats(area_fraction_raster, 1)).mean::numeric, 3) as area_mean,
      ROUND((ST_SummaryStats(area_fraction_raster, 1)).min::numeric, 3) as area_min,
      ROUND((ST_SummaryStats(area_fraction_raster, 1)).max::numeric, 3) as area_max,
      (ST_SummaryStats(area_fraction_raster, 1)).count as pixel_count
    FROM sydney_area_fraction_test;
  ")

  cat("\\nArea fraction statistics (0-1 scale):\\n")
  print(area_stats)

  # Add raster constraints for QGIS
  cat("\\nAdding raster constraints...\\n")
  dbExecute(con, "SELECT AddRasterConstraints('sydney_area_fraction_test'::name, 'population_raster'::name);")
  dbExecute(con, "SELECT AddRasterConstraints('sydney_area_fraction_test'::name, 'area_fraction_raster'::name);")
}

dbDisconnect(con)

cat("\\n=== PIXEL AREA FRACTION TEST COMPLETE ===\\n")
cat("Table: sydney_area_fraction_test ready for testing\\n")
cat("area_fraction_raster shows 0-1 fraction of each pixel within annulus\\n")
cat("Values should be mostly 1.0 (full pixels) with <1.0 at edges\\n")