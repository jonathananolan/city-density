# CREATE MELBOURNE RASTER RINGS (1-150) FOR QGIS VIEWING
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

# Parameters
geoname_id <- 2158177  # Melbourne
max_rings <- 150
city_name <- "Melbourne"

cat("=== CREATING MELBOURNE RASTER RINGS FOR QGIS ===\n")
cat(sprintf("City: %s (ID: %s) rings 1-%d\n\n", city_name, geoname_id, max_rings))

# Check if source data exists
source_exists <- dbExistsTable(con, "temp_source_pixels_complete")
if (!source_exists) {
  cat("Creating source data...\n")
  tic("source_prep")

  # Create city boundary
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_boundary_complete;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_city_boundary_complete AS
    SELECT ST_Union(annulus_geom) as city_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2;
  ", params = list(geoname_id, max_rings))

  # Create temp rasters
  dbExecute(con, "DROP TABLE IF EXISTS temp_pop_rasters_complete;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_pop_rasters_complete AS
    SELECT DISTINCT p.rid, p.rast, p.country_iso3
    FROM temp_city_boundary_complete cb
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(cb.city_geom, p.rast);
  ")

  dbExecute(con, "DROP TABLE IF EXISTS temp_water_rasters_complete;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_water_rasters_complete AS
    SELECT DISTINCT w.rid, w.rast
    FROM temp_city_boundary_complete cb
    CROSS JOIN water_pct w
    WHERE ST_Intersects(cb.city_geom, w.rast);
  ")

  # Extract and merge pixels
  dbExecute(con, "DROP TABLE IF EXISTS temp_source_pixels_complete;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_source_pixels_complete AS
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
      CROSS JOIN temp_pop_rasters_complete p
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
    ),

    water_pixel_data AS (
      SELECT
        r.ring_number,
        ST_PixelAsCentroids(w.rast, 1) as pixel_data
      FROM city_rings r
      CROSS JOIN temp_water_rasters_complete w
      WHERE ST_Intersects(r.annulus_geom, w.rast)
    ),

    water_points AS (
      SELECT
        ring_number,
        (pixel_data).geom as pixel_center,
        (pixel_data).val as water_pct,
        (pixel_data).x as pixel_x,
        (pixel_data).y as pixel_y
      FROM water_pixel_data
      WHERE (pixel_data).val > 5
    ),

    all_pixels AS (
      SELECT ring_number, pixel_center, population, NULL::double precision as water_pct, pixel_x, pixel_y
      FROM pop_points
      UNION
      SELECT ring_number, pixel_center, NULL::double precision, water_pct, pixel_x, pixel_y
      FROM water_points
    ),

    merged_pixels AS (
      SELECT
        ring_number, pixel_center,
        MAX(population) as population_raw,
        MAX(water_pct) as water_pct_raw,
        MAX(pixel_x) as pixel_x,
        MAX(pixel_y) as pixel_y
      FROM all_pixels
      GROUP BY ring_number, pixel_center
    )

    SELECT
      mp.ring_number,
      mp.pixel_center,
      mp.population_raw,
      mp.water_pct_raw,
      mp.pixel_x,
      mp.pixel_y,
      CASE
        WHEN ST_Within(mp.pixel_center, cr.annulus_geom) THEN 1.0
        ELSE 0.7
      END as area_fraction
    FROM merged_pixels mp
    JOIN city_rings cr ON mp.ring_number = cr.ring_number
    WHERE ST_Intersects(mp.pixel_center, cr.annulus_geom);
  ", params = list(geoname_id, max_rings))

  source_time <- toc(quiet = TRUE)
  source_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_source_pixels_complete;")[[1]])
  cat(sprintf("✓ Created source data: %d pixels in %.2fs\n\n", source_count, source_time$toc - source_time$tic))
} else {
  cat("✓ Using existing source data\n\n")
  source_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_source_pixels_complete;")[[1]])
  cat(sprintf("Source data: %d pixels\n\n", source_count))
}

# Create the raster by ring table
cat("Creating Melbourne raster rings table...\n")
tic("create_raster_rings")

dbExecute(con, "DROP TABLE IF EXISTS melbourne_raster_rings;")
dbExecute(con, "
  CREATE TABLE melbourne_raster_rings (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    ring_raster RASTER,
    pixel_count INTEGER,
    avg_population DOUBLE PRECISION,
    total_population DOUBLE PRECISION,
    ring_center GEOMETRY(Point, 4326),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")

dbExecute(con, "
  INSERT INTO melbourne_raster_rings (geoname_id, ring_number, ring_raster, pixel_count, avg_population, total_population, ring_center)
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
    AVG(COALESCE(population_raw, 0)) as avg_population,
    SUM(COALESCE(population_raw * area_fraction, 0)) as total_population,
    ST_Centroid(ST_Collect(pixel_center)) as ring_center
  FROM temp_source_pixels_complete
  WHERE population_raw IS NOT NULL OR water_pct_raw IS NOT NULL
  GROUP BY ring_number
  ORDER BY ring_number;
", params = list(geoname_id))

# Create indexes
dbExecute(con, "CREATE INDEX melbourne_raster_rings_geoname_ring_idx ON melbourne_raster_rings (geoname_id, ring_number);")
dbExecute(con, "CREATE INDEX melbourne_raster_rings_center_idx ON melbourne_raster_rings USING GIST (ring_center);")

create_time <- toc(quiet = TRUE)
ring_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM melbourne_raster_rings;")[[1]])
cat(sprintf("✓ Created %d raster rings in %.2fs\n\n", ring_count, create_time$toc - create_time$tic))

# Apply raster constraints fix for QGIS compatibility
cat("Applying raster constraints fix for QGIS compatibility...\n")
source("R/functions/fix_raster_constraints.R", local = TRUE)

tic("fix_constraints")
fix_raster_constraints(con, "melbourne_raster_rings", "ring_raster")
fix_time <- toc(quiet = TRUE)
cat(sprintf("✓ Applied raster constraints in %.2fs\n\n", fix_time$toc - fix_time$tic))

# Summary
cat("=== MELBOURNE RASTER RINGS SUMMARY ===\n")
summary_stats <- dbGetQuery(con, "
  SELECT
    COUNT(*) as total_rings,
    MIN(ring_number) as min_ring,
    MAX(ring_number) as max_ring,
    SUM(pixel_count) as total_pixels,
    ROUND(AVG(pixel_count)::numeric, 0) as avg_pixels_per_ring,
    ROUND(SUM(total_population)::numeric, 0) as total_population,
    ROUND(AVG(avg_population)::numeric, 2) as avg_population_per_pixel
  FROM melbourne_raster_rings;
")
print(summary_stats)

# Check table size
size_result <- dbGetQuery(con, "
  SELECT
    pg_size_pretty(pg_total_relation_size('melbourne_raster_rings')) as pretty_size,
    pg_total_relation_size('melbourne_raster_rings') as bytes
")
cat(sprintf("\nTable size: %s (%s bytes)\n", size_result$pretty_size[1], format(size_result$bytes[1], big.mark=",")))

cat("\n✓ Melbourne raster rings table ready for QGIS viewing!\n")
cat("Table name: melbourne_raster_rings\n")
cat("Raster column: ring_raster\n")

dbDisconnect(con)

cat("\n=== COMPLETE ===\n")