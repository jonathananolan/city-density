# TEST RASTER VALIDITY - Check if raster data makes sense
library(RPostgres)
library(DBI)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "worldpop_db",
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)

geoname_id <- 2158177  # Melbourne
max_rings <- 5  # Test with just first 5 rings

cat("=== TESTING RASTER VALIDITY ===\n")
cat("Creating raster table for first 5 rings to examine...\n\n")

# Prepare source data (reuse if exists)
source_exists <- dbExistsTable(con, "temp_source_pixels_complete")
if (!source_exists) {
  cat("Creating source data...\n")
  # Create city boundary
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_boundary_test;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_city_boundary_test AS
    SELECT ST_Union(annulus_geom) as city_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2;
  ", params = list(geoname_id, max_rings))

  # Create temp rasters
  dbExecute(con, "DROP TABLE IF EXISTS temp_pop_rasters_test;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_pop_rasters_test AS
    SELECT DISTINCT p.rid, p.rast, p.country_iso3
    FROM temp_city_boundary_test cb
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(cb.city_geom, p.rast);
  ")

  # Extract and merge pixels
  dbExecute(con, "DROP TABLE IF EXISTS temp_source_pixels_test;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_source_pixels_test AS
    WITH city_rings AS (
      SELECT ring_number, annulus_geom
      FROM city_annulus
      WHERE geoname_id = $1 AND ring_number <= $2
    ),

    pop_pixel_data AS (
      SELECT
        r.ring_number,
        ST_PixelAsCentroids(p.rast, 1) as pixel_data
      FROM city_rings r
      CROSS JOIN temp_pop_rasters_test p
      WHERE ST_Intersects(r.annulus_geom, p.rast)
    ),

    pop_points AS (
      SELECT
        ring_number,
        (pixel_data).geom as pixel_center,
        (pixel_data).val as population,
        (pixel_data).x as pixel_x,
        (pixel_data).y as pixel_y
      FROM pop_pixel_data
      WHERE (pixel_data).val > 0
    )

    SELECT
      ring_number,
      pixel_center,
      population as population_raw,
      pixel_x,
      pixel_y,
      1.0 as area_fraction  -- Simplified for testing
    FROM pop_points;
  ", params = list(geoname_id, max_rings))

  source_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_source_pixels_test;")[[1]])
  cat(sprintf("✓ Created source data: %d pixels\n\n", source_count))
} else {
  cat("Using existing source data\n\n")
  source_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_source_pixels_complete WHERE ring_number <= $1;", params = list(max_rings))[[1]])
}

# Create the raster table
cat("Creating raster table...\n")
dbExecute(con, "DROP TABLE IF EXISTS test_raster_rings;")
dbExecute(con, "
  CREATE TABLE test_raster_rings (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    ring_raster RASTER,
    pixel_count INTEGER,
    ring_center GEOMETRY(Point, 4326)
  );
")

if (source_exists) {
  table_name <- "temp_source_pixels_complete"
  ring_filter <- "AND ring_number <= $2"
} else {
  table_name <- "temp_source_pixels_test"
  ring_filter <- ""
}

dbExecute(con, sprintf("
  INSERT INTO test_raster_rings (geoname_id, ring_number, ring_raster, pixel_count, ring_center)
  SELECT
    $1::BIGINT,
    ring_number,
    ST_AsRaster(
      ST_Collect(pixel_center),
      100.0,  -- scalex (100m resolution)
      100.0,  -- scaley (100m resolution)
      ARRAY['32BF']::text[],  -- pixeltype array
      ARRAY[AVG(COALESCE(population_raw, 0))]::double precision[],  -- use AVG for aggregation
      ARRAY[0.0]::double precision[]  -- nodataval array
    ) as ring_raster,
    COUNT(*) as pixel_count,
    ST_Centroid(ST_Collect(pixel_center)) as ring_center
  FROM %s
  WHERE population_raw IS NOT NULL %s
  GROUP BY ring_number
  ORDER BY ring_number;
", table_name, ring_filter), params = if(source_exists) list(geoname_id, max_rings) else list(geoname_id))

# Check what we created
ring_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM test_raster_rings;")[[1]])
cat(sprintf("✓ Created %d raster rings\n\n", ring_count))

# Examine the raster data
cat("=== EXAMINING RASTER DATA ===\n")
sample_data <- dbGetQuery(con, "
  SELECT
    ring_number,
    pixel_count,
    ST_Width(ring_raster) as raster_width,
    ST_Height(ring_raster) as raster_height,
    ST_NumBands(ring_raster) as num_bands,
    ST_BandPixelType(ring_raster, 1) as pixel_type,
    ST_BandNoDataValue(ring_raster, 1) as nodata_value
  FROM test_raster_rings
  ORDER BY ring_number;
")
print(sample_data)

cat("\n=== SAMPLE RASTER VALUES ===\n")
# Try to extract some actual values from the raster
raster_values <- dbGetQuery(con, "
  SELECT
    ring_number,
    pixel_count,
    ST_SummaryStats(ring_raster, 1) as stats
  FROM test_raster_rings
  WHERE ring_number <= 3
  ORDER BY ring_number;
")
print(raster_values)

cat("\n=== COMPARING TO SOURCE DATA ===\n")
comparison <- dbGetQuery(con, sprintf("
  SELECT
    ring_number,
    COUNT(*) as source_pixels,
    AVG(population_raw) as avg_population,
    MIN(population_raw) as min_population,
    MAX(population_raw) as max_population
  FROM %s
  WHERE population_raw IS NOT NULL %s
  GROUP BY ring_number
  ORDER BY ring_number;
", table_name, if(source_exists) "AND ring_number <= $1" else ""),
params = if(source_exists) list(max_rings) else list())
print(comparison)

# Try to extract pixel values from raster back to points
cat("\n=== EXTRACTING VALUES BACK FROM RASTER ===\n")
extraction_test <- dbGetQuery(con, "
  SELECT
    r.ring_number,
    r.pixel_count as raster_pixel_count,
    COUNT(pix.val) as extracted_pixels,
    AVG(pix.val) as avg_extracted_value
  FROM test_raster_rings r
  CROSS JOIN ST_PixelAsPoints(r.ring_raster, 1) as pix
  WHERE r.ring_number <= 3
  GROUP BY r.ring_number, r.pixel_count
  ORDER BY r.ring_number;
")
print(extraction_test)

# Clean up
dbExecute(con, "DROP TABLE IF EXISTS test_raster_rings;")

dbDisconnect(con)

cat("\n=== RASTER VALIDITY TEST COMPLETE ===\n")