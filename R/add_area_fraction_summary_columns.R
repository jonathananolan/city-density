# ADD AREA FRACTION SUMMARY TO EXISTING SYDNEY_FIXED_WATER TABLE
# Add summary statistics about area fractions as regular columns

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

cat("=== ADDING AREA FRACTION SUMMARY COLUMNS ===\n")
cat("Adding area fraction statistics to sydney_fixed_water table\n\n")

# Add area fraction summary columns
dbExecute(con, "
  ALTER TABLE sydney_fixed_water
  ADD COLUMN IF NOT EXISTS total_pixels INTEGER,
  ADD COLUMN IF NOT EXISTS full_pixels INTEGER,
  ADD COLUMN IF NOT EXISTS partial_pixels INTEGER,
  ADD COLUMN IF NOT EXISTS min_area_fraction NUMERIC(5,3),
  ADD COLUMN IF NOT EXISTS avg_area_fraction NUMERIC(5,3),
  ADD COLUMN IF NOT EXISTS has_edge_effects BOOLEAN;
")

cat("Processing area fractions for all Sydney rings...\n")
tic("area_fraction_summary")

# Calculate area fractions for each ring and update summary columns
for (ring_num in 1:150) {
  if (ring_num %% 20 == 0) {
    cat(sprintf("Processing ring %d/150...\n", ring_num))
  }

  # Calculate area fractions for this ring using the proven method
  area_stats <- dbGetQuery(con, "
    WITH ring_geom AS (
      SELECT annulus_geom
      FROM city_annulus
      WHERE geoname_id = $1 AND ring_number = $2
    ),

    -- Get the population raster for this ring from sydney_fixed_water
    pop_raster AS (
      SELECT population_raster
      FROM sydney_fixed_water
      WHERE annulus_number = $2
    ),

    -- Extract each pixel as a polygon
    pixel_polygons AS (
      SELECT
        (ST_PixelAsPolygons(population_raster, 1)).x as pixel_x,
        (ST_PixelAsPolygons(population_raster, 1)).y as pixel_y,
        (ST_PixelAsPolygons(population_raster, 1)).val as population,
        (ST_PixelAsPolygons(population_raster, 1)).geom as pixel_geom
      FROM pop_raster
      WHERE population_raster IS NOT NULL
    ),

    -- Calculate area fraction for each pixel
    pixel_area_fractions AS (
      SELECT
        pp.pixel_x,
        pp.pixel_y,
        pp.population,
        -- Calculate fraction: intersection area / total pixel area
        ST_Area(ST_Intersection(pp.pixel_geom, r.annulus_geom)) / ST_Area(pp.pixel_geom) as area_fraction
      FROM pixel_polygons pp, ring_geom r
      WHERE ST_Intersects(pp.pixel_geom, r.annulus_geom)
    )

    -- Calculate summary statistics
    SELECT
      COUNT(*) as total_pixels,
      COUNT(*) FILTER (WHERE area_fraction >= 0.999) as full_pixels,
      COUNT(*) FILTER (WHERE area_fraction < 0.999) as partial_pixels,
      ROUND(MIN(area_fraction)::numeric, 3) as min_area_fraction,
      ROUND(AVG(area_fraction)::numeric, 3) as avg_area_fraction,
      (COUNT(*) FILTER (WHERE area_fraction < 0.999) > 0) as has_edge_effects
    FROM pixel_area_fractions;
  ", params = list(sydney_geoname_id, ring_num))

  # Update the sydney_fixed_water table with these statistics
  if (nrow(area_stats) > 0 && !is.na(area_stats$total_pixels)) {
    dbExecute(con, "
      UPDATE sydney_fixed_water
      SET
        total_pixels = $1,
        full_pixels = $2,
        partial_pixels = $3,
        min_area_fraction = $4,
        avg_area_fraction = $5,
        has_edge_effects = $6
      WHERE annulus_number = $7;
    ", params = list(
      area_stats$total_pixels,
      area_stats$full_pixels,
      area_stats$partial_pixels,
      area_stats$min_area_fraction,
      area_stats$avg_area_fraction,
      area_stats$has_edge_effects,
      ring_num
    ))
  }
}

total_time <- toc(quiet = TRUE)

# Check results
summary_results <- dbGetQuery(con, "
  SELECT
    COUNT(*) as rings_with_data,
    SUM(total_pixels) as total_pixels_all_rings,
    SUM(partial_pixels) as total_partial_pixels,
    COUNT(*) FILTER (WHERE has_edge_effects = true) as rings_with_edge_effects,
    ROUND(AVG(avg_area_fraction)::numeric, 3) as overall_avg_area_fraction
  FROM sydney_fixed_water
  WHERE total_pixels IS NOT NULL;
")

cat(sprintf("\n=== AREA FRACTION SUMMARY RESULTS ===\n"))
cat(sprintf("✓ Processing completed in %.2fs\n", total_time$toc - total_time$tic))
cat(sprintf("✓ Rings processed: %d\n", summary_results$rings_with_data))
cat(sprintf("✓ Total pixels across all rings: %d\n", summary_results$total_pixels_all_rings))
cat(sprintf("✓ Partial pixels (edge effects): %d\n", summary_results$total_partial_pixels))
cat(sprintf("✓ Rings with edge effects: %d\n", summary_results$rings_with_edge_effects))
cat(sprintf("✓ Overall average area fraction: %.3f\n", summary_results$overall_avg_area_fraction))

# Show sample of rings with edge effects
edge_examples <- dbGetQuery(con, "
  SELECT
    annulus_number,
    total_pixels,
    partial_pixels,
    min_area_fraction,
    avg_area_fraction
  FROM sydney_fixed_water
  WHERE has_edge_effects = true
  ORDER BY partial_pixels DESC
  LIMIT 10;
")

if (nrow(edge_examples) > 0) {
  cat("\nTop 10 rings with most edge effects:\n")
  print(edge_examples)
}

dbDisconnect(con)

cat("\n=== AREA FRACTION SUMMARY COMPLETE ===\n")
cat("Table: sydney_fixed_water now has area fraction summary columns:\n")
cat("- total_pixels: total number of pixels in ring\n")
cat("- full_pixels: pixels 100% within annulus\n")
cat("- partial_pixels: pixels with edge effects\n")
cat("- min_area_fraction: minimum area fraction (shows edge effects)\n")
cat("- avg_area_fraction: average area fraction across ring\n")
cat("- has_edge_effects: boolean flag for rings with partial pixels\n")