# CREATE AREA FRACTION LOOKUP TABLE
# Calculate area fraction for each pixel without creating a raster

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
ring_num <- 3

cat("=== CREATING AREA FRACTION LOOKUP TABLE ===\n")
cat(sprintf("Testing with Sydney ring %d\n\n", ring_num))

tic("area_fraction_lookup")

# Create lookup table with area fractions
dbExecute(con, "DROP TABLE IF EXISTS sydney_pixel_area_fractions;")
dbExecute(con, "
  CREATE TABLE sydney_pixel_area_fractions (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    annulus_number INTEGER,
    pixel_x INTEGER,
    pixel_y INTEGER,
    population DOUBLE PRECISION,
    area_fraction DOUBLE PRECISION,
    pixel_geom GEOMETRY(POLYGON, 4326),
    pixel_center GEOMETRY(POINT, 4326)
  );
")

# Calculate area fractions for each pixel
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

  -- Extract each pixel as a polygon
  pixel_polygons AS (
    SELECT
      (ST_PixelAsPolygons(pop_band, 1)).x as pixel_x,
      (ST_PixelAsPolygons(pop_band, 1)).y as pixel_y,
      (ST_PixelAsPolygons(pop_band, 1)).val as population,
      (ST_PixelAsPolygons(pop_band, 1)).geom as pixel_geom
    FROM pop_raster
    WHERE pop_band IS NOT NULL
  )

  INSERT INTO sydney_pixel_area_fractions (
    geoname_id, annulus_number, pixel_x, pixel_y, population,
    area_fraction, pixel_geom, pixel_center
  )
  SELECT
    $1::BIGINT,
    $2,
    pp.pixel_x,
    pp.pixel_y,
    pp.population,
    -- Calculate fraction: intersection area / total pixel area
    ST_Area(ST_Intersection(pp.pixel_geom, r.annulus_geom)) / ST_Area(pp.pixel_geom) as area_fraction,
    pp.pixel_geom,
    ST_Centroid(pp.pixel_geom) as pixel_center
  FROM pixel_polygons pp, ring_geom r
  WHERE ST_Intersects(pp.pixel_geom, r.annulus_geom);
", params = list(sydney_geoname_id, ring_num))

lookup_time <- toc(quiet = TRUE)

# Check results
result_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sydney_pixel_area_fractions;")[[1]])

cat(sprintf("\\n=== AREA FRACTION LOOKUP RESULTS ===\\n"))
cat(sprintf("✓ Created lookup table in %.2fs\\n", lookup_time$toc - lookup_time$tic))
cat(sprintf("✓ Pixels processed: %d\\n", result_count))

if (result_count > 0) {
  # Check area fraction statistics
  area_stats <- dbGetQuery(con, "
    SELECT
      COUNT(*) as total_pixels,
      ROUND(MIN(area_fraction)::numeric, 3) as min_fraction,
      ROUND(MAX(area_fraction)::numeric, 3) as max_fraction,
      ROUND(AVG(area_fraction)::numeric, 3) as avg_fraction,
      COUNT(*) FILTER (WHERE area_fraction = 1.0) as full_pixels,
      COUNT(*) FILTER (WHERE area_fraction < 1.0) as partial_pixels
    FROM sydney_pixel_area_fractions;
  ")

  cat("\\nArea fraction statistics:\\n")
  print(area_stats)

  # Show some example partial pixels
  partial_examples <- dbGetQuery(con, "
    SELECT
      pixel_x, pixel_y,
      ROUND(population::numeric, 2) as population,
      ROUND(area_fraction::numeric, 3) as area_fraction
    FROM sydney_pixel_area_fractions
    WHERE area_fraction < 1.0
    ORDER BY area_fraction
    LIMIT 5;
  ")

  if (nrow(partial_examples) > 0) {
    cat("\\nExample partial pixels (at annulus edges):\\n")
    print(partial_examples)
  }
}

dbDisconnect(con)

cat("\\n=== AREA FRACTION LOOKUP COMPLETE ===\\n")
cat("Table: sydney_pixel_area_fractions created\\n")
cat("Contains area_fraction (0-1) for each population pixel\\n")
cat("Most pixels = 1.0 (fully inside), edge pixels < 1.0\\n")