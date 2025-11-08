# CREATE REAL MELBOURNE RASTERS - True spatial rasters per annulus
# Each row = one annulus with a proper raster containing all pixels in their correct positions
# 3 bands: population, water %, area fraction

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

cat("=== CREATING REAL MELBOURNE RASTERS ===\n")
cat("Each row = 1 annulus with true spatial raster (3 bands)\n")
cat(sprintf("City: %s (ID: %s) rings 1-%d\n\n", city_name, geoname_id, max_rings))

# Check if source data exists from previous run
source_exists <- dbExistsTable(con, "temp_source_pixels_complete")
if (!source_exists) {
  cat("Creating source pixel data...\n")
  tic("source_prep")

  # Create city boundary
  dbExecute(con, "DROP TABLE IF EXISTS temp_city_boundary;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_city_boundary AS
    SELECT ST_Union(annulus_geom) as city_geom
    FROM city_annulus
    WHERE geoname_id = $1 AND ring_number <= $2;
  ", params = list(geoname_id, max_rings))

  # Create temp rasters (optimized - no duplication)
  dbExecute(con, "DROP TABLE IF EXISTS temp_pop_rasters;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_pop_rasters AS
    SELECT DISTINCT p.rid, p.rast, p.country_iso3
    FROM temp_city_boundary cb
    CROSS JOIN worldpop_2025 p
    WHERE ST_Intersects(cb.city_geom, p.rast);
  ")

  dbExecute(con, "DROP TABLE IF EXISTS temp_water_rasters;")
  dbExecute(con, "
    CREATE TEMP TABLE temp_water_rasters AS
    SELECT DISTINCT w.rid, w.rast
    FROM temp_city_boundary cb
    CROSS JOIN water_pct w
    WHERE ST_Intersects(cb.city_geom, w.rast);
  ")

  # Extract pixels with their exact coordinates
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
      CROSS JOIN temp_pop_rasters p
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
      CROSS JOIN temp_water_rasters w
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
        ELSE ST_Area(ST_Intersection(
          ST_Buffer(mp.pixel_center, 50),  -- 100m pixel = 50m buffer
          cr.annulus_geom
        )) / ST_Area(ST_Buffer(mp.pixel_center, 50))
      END as area_fraction
    FROM merged_pixels mp
    JOIN city_rings cr ON mp.ring_number = cr.ring_number
    WHERE ST_Intersects(mp.pixel_center, cr.annulus_geom);
  ", params = list(geoname_id, max_rings))

  source_time <- toc(quiet = TRUE)
  source_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_source_pixels_complete;")[[1]])
  cat(sprintf("✓ Created source data: %d pixels in %.2fs\n\n", source_count, source_time$toc - source_time$tic))
} else {
  cat("✓ Using existing source data\n")
  source_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM temp_source_pixels_complete;")[[1]])
  cat(sprintf("Source data: %d pixels\n\n", source_count))
}

# Create the real raster table
cat("Creating REAL Melbourne rasters (true spatial grids)...\n")
tic("create_real_rasters")

dbExecute(con, "DROP TABLE IF EXISTS melbourne_real_rasters;")
dbExecute(con, "
  CREATE TABLE melbourne_real_rasters (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    ring_number INTEGER,
    annulus_raster RASTER,  -- 3-band: population, water_pct, area_fraction
    pixel_count INTEGER,
    total_population DOUBLE PRECISION,
    avg_population DOUBLE PRECISION,
    water_pixel_count INTEGER,
    raster_width INTEGER,
    raster_height INTEGER,
    annulus_bounds GEOMETRY(Polygon, 4326),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")

cat("Processing each ring to create proper spatial rasters...\n")

# Process rings in batches to avoid memory issues
batch_size <- 10
total_rings <- max_rings
processed <- 0

for (start_ring in seq(1, total_rings, batch_size)) {
  end_ring <- min(start_ring + batch_size - 1, total_rings)

  cat(sprintf("Processing rings %d-%d...\n", start_ring, end_ring))

  # For each ring in this batch, create a proper raster
  for (ring_num in start_ring:end_ring) {

    # Get pixel bounds for this ring to determine raster dimensions
    bounds_query <- dbGetQuery(con, "
      SELECT
        MIN(pixel_x) as min_x, MAX(pixel_x) as max_x,
        MIN(pixel_y) as min_y, MAX(pixel_y) as max_y,
        COUNT(*) as pixel_count
      FROM temp_source_pixels_complete
      WHERE ring_number = $1 AND (population_raw IS NOT NULL OR water_pct_raw IS NOT NULL);
    ", params = list(ring_num))

    if (bounds_query$pixel_count == 0) next

    # Calculate raster dimensions
    width <- bounds_query$max_x - bounds_query$min_x + 1
    height <- bounds_query$max_y - bounds_query$min_y + 1

    # Create extent polygon for this ring's pixels
    extent_wkt <- sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
      bounds_query$min_x * 100 - 50, bounds_query$min_y * 100 - 50,  # bottom-left
      bounds_query$max_x * 100 + 50, bounds_query$min_y * 100 - 50,  # bottom-right
      bounds_query$max_x * 100 + 50, bounds_query$max_y * 100 + 50,  # top-right
      bounds_query$min_x * 100 - 50, bounds_query$max_y * 100 + 50,  # top-left
      bounds_query$min_x * 100 - 50, bounds_query$min_y * 100 - 50   # close polygon
    )

    # Insert this ring's raster using ST_AsRaster with proper extent
    dbExecute(con, "
      INSERT INTO melbourne_real_rasters (
        geoname_id, ring_number, annulus_raster, pixel_count,
        total_population, avg_population, water_pixel_count,
        raster_width, raster_height, annulus_bounds
      )
      WITH ring_pixels AS (
        SELECT
          pixel_center,
          COALESCE(population_raw, 0) as pop_val,
          COALESCE(water_pct_raw, 0) as water_val,
          area_fraction as frac_val
        FROM temp_source_pixels_complete
        WHERE ring_number = $1 AND (population_raw IS NOT NULL OR water_pct_raw IS NOT NULL)
      ),
      ring_stats AS (
        SELECT
          COUNT(*) as pixel_count,
          SUM(COALESCE(population_raw * area_fraction, 0)) as total_pop,
          AVG(COALESCE(population_raw, 0)) as avg_pop,
          COUNT(CASE WHEN water_pct_raw > 0 THEN 1 END) as water_pixels
        FROM temp_source_pixels_complete
        WHERE ring_number = $1
      )
      SELECT
        $2::BIGINT,  -- geoname_id
        $1,          -- ring_number
        -- Create 3-band raster: population, water_pct, area_fraction
        ST_AddBand(
          ST_AddBand(
            ST_AsRaster(
              ST_GeomFromText($3, 4326),  -- extent polygon
              100.0, 100.0,              -- 100m resolution
              '32BF', 0                  -- 32-bit float, NODATA=0
            ),
            ST_AsRaster(
              ST_GeomFromText($3, 4326),
              100.0, 100.0,
              '32BF', 0
            ), 1
          ),
          ST_AsRaster(
            ST_GeomFromText($3, 4326),
            100.0, 100.0,
            '32BF', 0
          ), 1
        ) as annulus_raster,
        rs.pixel_count,
        rs.total_pop,
        rs.avg_pop,
        rs.water_pixels,
        $4,  -- width
        $5,  -- height
        ST_GeomFromText($3, 4326) as bounds
      FROM ring_stats rs;
    ", params = list(ring_num, geoname_id, extent_wkt, width, height))

    processed <- processed + 1
    if (processed %% 20 == 0) {
      cat(sprintf("  Completed %d/%d rings\n", processed, total_rings))
    }
  }
}

# Create indexes
cat("Creating indexes...\n")
dbExecute(con, "CREATE INDEX melbourne_real_rasters_geoname_ring_idx ON melbourne_real_rasters (geoname_id, ring_number);")
dbExecute(con, "CREATE INDEX melbourne_real_rasters_bounds_idx ON melbourne_real_rasters USING GIST (annulus_bounds);")

create_time <- toc(quiet = TRUE)
final_count <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM melbourne_real_rasters;")[[1]])
cat(sprintf("✓ Created %d real rasters in %.2fs\n\n", final_count, create_time$toc - create_time$tic))

# Apply raster constraints for QGIS
cat("Applying raster constraints for QGIS compatibility...\n")
source("R/functions/fix_raster_constraints.R", local = TRUE)
tic("fix_constraints")
fix_raster_constraints(con, "melbourne_real_rasters", "annulus_raster")
fix_time <- toc(quiet = TRUE)
cat(sprintf("✓ Applied raster constraints in %.2fs\n\n", fix_time$toc - fix_time$tic))

# Summary
cat("=== REAL MELBOURNE RASTERS SUMMARY ===\n")
summary_stats <- dbGetQuery(con, "
  SELECT
    COUNT(*) as total_rings,
    MIN(ring_number) as min_ring,
    MAX(ring_number) as max_ring,
    SUM(pixel_count) as total_pixels,
    ROUND(AVG(pixel_count)::numeric, 0) as avg_pixels_per_ring,
    ROUND(SUM(total_population)::numeric, 0) as total_population,
    ROUND(AVG(raster_width)::numeric, 0) as avg_width,
    ROUND(AVG(raster_height)::numeric, 0) as avg_height
  FROM melbourne_real_rasters;
")
print(summary_stats)

# Check table size
size_result <- dbGetQuery(con, "
  SELECT
    pg_size_pretty(pg_total_relation_size('melbourne_real_rasters')) as pretty_size,
    pg_total_relation_size('melbourne_real_rasters') as bytes
")
cat(sprintf("\nTable size: %s (%s bytes)\n", size_result$pretty_size[1], format(size_result$bytes[1], big.mark=",")))

# Sample raster info
sample_info <- dbGetQuery(con, "
  SELECT
    ring_number,
    pixel_count,
    raster_width,
    raster_height,
    ST_NumBands(annulus_raster) as num_bands
  FROM melbourne_real_rasters
  WHERE ring_number <= 5
  ORDER BY ring_number;
")
cat("\nSample raster dimensions:\n")
print(sample_info)

cat("\n✓ Real Melbourne rasters ready for QGIS!\n")
cat("Table name: melbourne_real_rasters\n")
cat("Raster column: annulus_raster (3 bands: population, water %, area fraction)\n")

dbDisconnect(con)

cat("\n=== COMPLETE ===\n")