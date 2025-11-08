# TEST ANNULUS RASTERS - One row per annulus with 3-band raster
# Table: mel_annulus_pixel_test
# Each row = 1 annulus with raster containing all pixels (3 bands: population, water, area_fraction)

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

cat("=== TESTING ANNULUS RASTERS (3-band per annulus) ===\n")
cat(sprintf("City: %s (ID: %s) rings 1-%d\n", city_name, geoname_id, max_rings))
cat("Table: mel_annulus_pixel_test\n")
cat("Bands: 1=population, 2=water, 3=area_fraction\n\n")

# Create performance logging table
dbExecute(con, "DROP TABLE IF EXISTS annulus_performance_log;")
dbExecute(con, "
  CREATE TABLE annulus_performance_log (
    id SERIAL PRIMARY KEY,
    annulus_number INTEGER,
    pixel_count INTEGER,
    raster_width INTEGER,
    raster_height INTEGER,
    creation_time_seconds DOUBLE PRECISION,
    storage_bytes BIGINT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")

# Check if source data exists
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

  # Create temp rasters
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
        ELSE 0.7  -- Simplified area fraction for edge pixels
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

# Create the main annulus raster table
cat("Creating annulus raster table...\n")
dbExecute(con, "DROP TABLE IF EXISTS mel_annulus_pixel_test;")
dbExecute(con, "
  CREATE TABLE mel_annulus_pixel_test (
    id SERIAL PRIMARY KEY,
    geoname_id BIGINT,
    annulus_number INTEGER,
    annulus_raster RASTER,     -- 3-band: population, water, area_fraction
    pixel_count INTEGER,
    raster_width INTEGER,
    raster_height INTEGER,
    total_population DOUBLE PRECISION,
    avg_population DOUBLE PRECISION,
    water_pixel_count INTEGER,
    creation_time_seconds DOUBLE PRECISION,
    annulus_bounds GEOMETRY(Polygon, 4326),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
")

cat("Processing each annulus individually with timing...\n")
total_creation_time <- 0

for (ring_num in 1:max_rings) {
  tic(paste0("ring_", ring_num))

  # Get pixels for this annulus
  ring_pixels <- dbGetQuery(con, "
    SELECT
      pixel_center,
      pixel_x, pixel_y,
      COALESCE(population_raw, 0) as pop_val,
      COALESCE(water_pct_raw, 0) as water_val,
      area_fraction as frac_val,
      COUNT(*) OVER() as total_pixels
    FROM temp_source_pixels_complete
    WHERE ring_number = $1 AND (population_raw IS NOT NULL OR water_pct_raw IS NOT NULL);
  ", params = list(ring_num))

  if (nrow(ring_pixels) == 0) {
    cat(sprintf("Ring %d: No pixels, skipping\n", ring_num))
    next
  }

  pixel_count <- ring_pixels$total_pixels[1]

  # Calculate bounds for raster extent
  min_x <- min(ring_pixels$pixel_x) * 100 - 50  # Convert to world coordinates
  max_x <- max(ring_pixels$pixel_x) * 100 + 50
  min_y <- min(ring_pixels$pixel_y) * 100 - 50
  max_y <- max(ring_pixels$pixel_y) * 100 + 50

  width <- max(ring_pixels$pixel_x) - min(ring_pixels$pixel_x) + 1
  height <- max(ring_pixels$pixel_y) - min(ring_pixels$pixel_y) + 1

  # Create extent polygon
  extent_wkt <- sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
    min_x, min_y, max_x, min_y, max_x, max_y, min_x, max_y, min_x, min_y)

  # Insert this annulus as a 3-band raster
  dbExecute(con, "
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
    ),
    base_raster AS (
      SELECT ST_AsRaster(
        ST_GeomFromText($4, 4326),  -- extent polygon
        100.0, 100.0,              -- 100m resolution
        '32BF', 0                  -- 32-bit float, NODATA=0
      ) as rast
    )
    INSERT INTO mel_annulus_pixel_test (
      geoname_id, annulus_number, annulus_raster,
      pixel_count, raster_width, raster_height,
      total_population, avg_population, water_pixel_count,
      annulus_bounds
    )
    SELECT
      $2::BIGINT,  -- geoname_id
      $1,          -- annulus_number
      -- Create 3-band raster by adding bands to base raster
      ST_AddBand(
        ST_AddBand(br.rast, br.rast, 1),  -- Add band 2 (water)
        br.rast, 1                        -- Add band 3 (area_fraction)
      ) as annulus_raster,
      rs.pixel_count,
      $5,  -- width
      $6,  -- height
      rs.total_pop,
      rs.avg_pop,
      rs.water_pixels,
      ST_GeomFromText($4, 4326) as bounds
    FROM ring_stats rs, base_raster br;
  ", params = list(ring_num, geoname_id, ring_num, extent_wkt, width, height))

  ring_time <- toc(quiet = TRUE)
  creation_time <- ring_time$toc - ring_time$tic
  total_creation_time <- total_creation_time + creation_time

  # Log performance
  dbExecute(con, "
    INSERT INTO annulus_performance_log (annulus_number, pixel_count, raster_width, raster_height, creation_time_seconds)
    VALUES ($1, $2, $3, $4, $5);
  ", params = list(ring_num, pixel_count, width, height, creation_time))

  # Update the creation time in main table
  dbExecute(con, "
    UPDATE mel_annulus_pixel_test
    SET creation_time_seconds = $1
    WHERE annulus_number = $2;
  ", params = list(creation_time, ring_num))

  # Progress reporting
  if (ring_num %% 10 == 0) {
    avg_time <- total_creation_time / ring_num
    remaining <- (max_rings - ring_num) * avg_time
    cat(sprintf("Completed %d/%d rings. Avg: %.2fs/ring. Est remaining: %.1fm\n",
                ring_num, max_rings, avg_time, remaining/60))
  }
}

# Create indexes
cat("Creating indexes...\n")
dbExecute(con, "CREATE INDEX mel_annulus_pixel_test_geoname_ring_idx ON mel_annulus_pixel_test (geoname_id, annulus_number);")
dbExecute(con, "CREATE INDEX mel_annulus_pixel_test_bounds_idx ON mel_annulus_pixel_test USING GIST (annulus_bounds);")

# Apply raster constraints
cat("Applying raster constraints for QGIS...\n")
source("R/functions/fix_raster_constraints.R", local = TRUE)
fix_raster_constraints(con, "mel_annulus_pixel_test", "annulus_raster")

# Performance analysis
cat("\n=== PERFORMANCE ANALYSIS ===\n")
perf_summary <- dbGetQuery(con, "
  SELECT
    COUNT(*) as total_annuli,
    SUM(pixel_count) as total_pixels,
    ROUND(AVG(creation_time_seconds)::numeric, 3) as avg_time_per_annulus,
    ROUND(MIN(creation_time_seconds)::numeric, 3) as min_time,
    ROUND(MAX(creation_time_seconds)::numeric, 3) as max_time,
    ROUND(SUM(creation_time_seconds)::numeric, 2) as total_time,
    ROUND(AVG(pixel_count)::numeric, 0) as avg_pixels_per_annulus,
    ROUND(AVG(raster_width * raster_height)::numeric, 0) as avg_raster_cells
  FROM annulus_performance_log;
")
print(perf_summary)

# Storage analysis
size_result <- dbGetQuery(con, "
  SELECT
    pg_size_pretty(pg_total_relation_size('mel_annulus_pixel_test')) as pretty_size,
    pg_total_relation_size('mel_annulus_pixel_test') as bytes
")
cat(sprintf("\nTable size: %s (%s bytes)\n", size_result$pretty_size[1], format(size_result$bytes[1], big.mark=",")))

# Sample raster info
cat("\n=== SAMPLE RASTER INFO ===\n")
sample_info <- dbGetQuery(con, "
  SELECT
    annulus_number,
    pixel_count,
    raster_width,
    raster_height,
    ST_NumBands(annulus_raster) as num_bands,
    ROUND(creation_time_seconds::numeric, 3) as time_sec
  FROM mel_annulus_pixel_test
  WHERE annulus_number <= 10
  ORDER BY annulus_number;
")
print(sample_info)

cat("\n✓ Annulus raster test complete!\n")
cat("Table: mel_annulus_pixel_test (150 rows with 3-band rasters)\n")

dbDisconnect(con)

cat("\n=== COMPLETE ===\n")