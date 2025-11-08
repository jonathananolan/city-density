library(RPostgres)
library(DBI)
library(sf)
library(dplyr)

# Create optimized pixel-annulus dataset with overlap handling
create_pixel_annulus_dataset <- function(
  con,
  worldpop_table = "worldpop_2025",
  annulus_table = "city_annulus", 
  output_table = "pixel_annulus_intersections",
  min_overlap_pct = 1,  # Minimum 1% overlap to include
  max_cities_per_batch = 50,  # Process cities in batches
  recreate_table = FALSE,
  verbose = TRUE
) {
  
  if (verbose) cat("=== Creating Optimized Pixel-Annulus Dataset ===\n")
  
  # Create the output table with optimal structure
  if (recreate_table || !dbExistsTable(con, output_table)) {
    if (verbose) cat("Creating pixel-annulus intersection table...\n")
    
    if (dbExistsTable(con, output_table)) {
      dbExecute(con, sprintf("DROP TABLE %s CASCADE", output_table))
    }
    
    create_sql <- sprintf("
      CREATE TABLE %s (
        -- Unique identifier for each pixel-annulus intersection
        intersection_id BIGSERIAL PRIMARY KEY,
        
        -- Pixel identification
        pixel_rid INTEGER NOT NULL,           -- Raster tile ID
        pixel_x INTEGER NOT NULL,             -- Pixel X coordinate within tile  
        pixel_y INTEGER NOT NULL,             -- Pixel Y coordinate within tile
        pixel_geom GEOMETRY(POLYGON, 4326),   -- Actual pixel boundary
        pixel_center GEOMETRY(POINT, 4326),   -- Pixel centroid
        
        -- WorldPop data
        population_density FLOAT,             -- Population value for this pixel
        
        -- City and ring identification  
        geoname_id BIGINT NOT NULL,           -- City identifier
        city_name VARCHAR(255),               -- City name for easy reading
        ring_number INTEGER NOT NULL,         -- Which ring (1-150)
        ring_radius_km INTEGER,               -- Ring radius
        
        -- Overlap calculations
        overlap_area_m2 DOUBLE PRECISION,     -- Area of pixel inside this annulus
        overlap_percentage FLOAT,             -- %% of pixel inside this annulus (0-100)
        pixel_total_area_m2 DOUBLE PRECISION, -- Total pixel area
        
        -- Population attribution
        attributed_population FLOAT,          -- Population × overlap_percentage
        
        -- Multi-city flags
        is_multi_city_pixel BOOLEAN,          -- True if pixel overlaps multiple cities
        city_count INTEGER,                   -- How many cities this pixel touches
        primary_city_geoname_id BIGINT,       -- City with largest overlap for this pixel
        
        -- Spatial indexes
        CONSTRAINT pixel_annulus_unique UNIQUE (pixel_rid, pixel_x, pixel_y, geoname_id, ring_number)
      )
    ", output_table)
    
    dbExecute(con, create_sql)
    
    # Create optimized indexes
    indexes <- c(
      # Fast city-based queries
      sprintf("CREATE INDEX %s_city_ring_idx ON %s (geoname_id, ring_number)", output_table, output_table),
      # Fast pixel-based queries  
      sprintf("CREATE INDEX %s_pixel_idx ON %s (pixel_rid, pixel_x, pixel_y)", output_table, output_table),
      # Spatial queries
      sprintf("CREATE INDEX %s_pixel_geom_idx ON %s USING GIST (pixel_geom)", output_table, output_table),
      sprintf("CREATE INDEX %s_pixel_center_idx ON %s USING GIST (pixel_center)", output_table, output_table),
      # Multi-city analysis
      sprintf("CREATE INDEX %s_multi_city_idx ON %s (is_multi_city_pixel, city_count)", output_table, output_table),
      # Population queries
      sprintf("CREATE INDEX %s_population_idx ON %s (population_density) WHERE population_density > 0", output_table, output_table)
    )
    
    for (idx in indexes) {
      dbExecute(con, idx)
    }
    
    if (verbose) cat("✓ Table and indexes created\n")
  }
  
  # Get list of cities to process
  cities_query <- sprintf("
    SELECT DISTINCT geoname_id, city_name 
    FROM %s 
    ORDER BY geoname_id
  ", annulus_table)
  
  cities <- dbGetQuery(con, cities_query)
  total_cities <- nrow(cities)
  
  if (verbose) cat("Processing", total_cities, "cities in batches of", max_cities_per_batch, "...\n")
  
  # Process cities in batches
  for (batch_start in seq(1, total_cities, max_cities_per_batch)) {
    batch_end <- min(batch_start + max_cities_per_batch - 1, total_cities)
    batch_cities <- cities[batch_start:batch_end, ]
    
    if (verbose) cat("Batch", ceiling(batch_start/max_cities_per_batch), ": cities", batch_start, "-", batch_end, "\n")
    
    # Create batch-specific insertion query
    city_ids <- paste(batch_cities$geoname_id, collapse = ",")
    
    insert_sql <- sprintf("
      INSERT INTO %s (
        pixel_rid, pixel_x, pixel_y, pixel_geom, pixel_center,
        population_density, geoname_id, city_name, ring_number, ring_radius_km,
        overlap_area_m2, overlap_percentage, pixel_total_area_m2, attributed_population
      )
      WITH pixel_polygons AS (
        -- Extract individual pixels as polygons
        SELECT 
          w.rid as pixel_rid,
          (ST_PixelAsPolygons(w.rast)).x as pixel_x,
          (ST_PixelAsPolygons(w.rast)).y as pixel_y,
          (ST_PixelAsPolygons(w.rast)).geom as pixel_geom,
          (ST_PixelAsPolygons(w.rast)).val as population_density
        FROM %s w
        WHERE 
          -- Only process pixels that intersect with our batch cities
          EXISTS (
            SELECT 1 FROM %s a 
            WHERE a.geoname_id IN (%s)
            AND ST_Intersects(w.rast, a.annulus_geom)
          )
          AND (ST_PixelAsPolygons(w.rast)).val IS NOT NULL
          AND (ST_PixelAsPolygons(w.rast)).val > 0
      ),
      pixel_intersections AS (
        -- Calculate overlaps between pixels and annulus
        SELECT 
          p.pixel_rid,
          p.pixel_x, 
          p.pixel_y,
          p.pixel_geom,
          ST_Centroid(p.pixel_geom) as pixel_center,
          p.population_density,
          a.geoname_id,
          a.city_name,
          a.ring_number,
          a.ring_radius_km,
          -- Calculate actual intersection
          ST_Area(ST_Intersection(p.pixel_geom, a.annulus_geom)::geography) as overlap_area_m2,
          ST_Area(p.pixel_geom::geography) as pixel_total_area_m2
        FROM pixel_polygons p
        JOIN %s a ON ST_Intersects(p.pixel_geom, a.annulus_geom)
        WHERE a.geoname_id IN (%s)
      )
      SELECT 
        pixel_rid, pixel_x, pixel_y, pixel_geom, pixel_center,
        population_density, geoname_id, city_name, ring_number, ring_radius_km,
        overlap_area_m2,
        (overlap_area_m2 / pixel_total_area_m2 * 100) as overlap_percentage,
        pixel_total_area_m2,
        (population_density * overlap_area_m2 / pixel_total_area_m2) as attributed_population
      FROM pixel_intersections
      WHERE (overlap_area_m2 / pixel_total_area_m2 * 100) >= %f  -- Minimum overlap threshold
      ORDER BY pixel_rid, pixel_x, pixel_y, geoname_id, ring_number
    ", output_table, worldpop_table, annulus_table, city_ids, annulus_table, city_ids, min_overlap_pct)
    
    # Execute batch insertion
    tryCatch({
      rows_inserted <- dbExecute(con, insert_sql)
      if (verbose) cat("  → Inserted", rows_inserted, "pixel-annulus intersections\n")
    }, error = function(e) {
      cat("  ✗ Error processing batch:", e$message, "\n")
    })
    
    # Small pause between batches
    Sys.sleep(1)
  }
  
  if (verbose) cat("\nPost-processing: calculating multi-city flags...\n")
  
  # Update multi-city flags
  multi_city_sql <- sprintf("
    WITH pixel_city_counts AS (
      SELECT 
        pixel_rid, pixel_x, pixel_y,
        COUNT(DISTINCT geoname_id) as city_count,
        -- Get primary city (highest total attributed population for this pixel)
        (array_agg(geoname_id ORDER BY attributed_population DESC))[1] as primary_city_geoname_id
      FROM %s
      GROUP BY pixel_rid, pixel_x, pixel_y
    )
    UPDATE %s 
    SET 
      is_multi_city_pixel = (c.city_count > 1),
      city_count = c.city_count,
      primary_city_geoname_id = c.primary_city_geoname_id
    FROM pixel_city_counts c
    WHERE %s.pixel_rid = c.pixel_rid 
      AND %s.pixel_x = c.pixel_x 
      AND %s.pixel_y = c.pixel_y
  ", output_table, output_table, output_table, output_table, output_table)
  
  dbExecute(con, multi_city_sql)
  
  # Generate summary statistics
  if (verbose) {
    cat("\n=== Dataset Summary ===\n")
    
    summary_sql <- sprintf("
      SELECT 
        COUNT(*) as total_intersections,
        COUNT(DISTINCT CONCAT(pixel_rid, '-', pixel_x, '-', pixel_y)) as unique_pixels,
        COUNT(DISTINCT geoname_id) as cities_covered,
        COUNT(DISTINCT ring_number) as rings_per_city,
        AVG(overlap_percentage) as avg_overlap_pct,
        SUM(CASE WHEN is_multi_city_pixel THEN 1 ELSE 0 END) as multi_city_intersections,
        SUM(attributed_population) as total_attributed_population,
        pg_size_pretty(pg_total_relation_size('%s')) as table_size
      FROM %s
    ", output_table, output_table)
    
    summary <- dbGetQuery(con, summary_sql)
    
    cat("Total intersections:", format(summary$total_intersections, big.mark = ","), "\n")
    cat("Unique pixels:", format(summary$unique_pixels, big.mark = ","), "\n") 
    cat("Cities covered:", summary$cities_covered, "\n")
    cat("Rings per city:", summary$rings_per_city, "\n")
    cat("Average overlap:", round(summary$avg_overlap_pct, 1), "%\n")
    cat("Multi-city intersections:", format(summary$multi_city_intersections, big.mark = ","), "\n")
    cat("Total attributed population:", format(round(summary$total_attributed_population), big.mark = ","), "\n")
    cat("Table size:", summary$table_size, "\n")
    
    # Multi-city analysis
    multi_city_stats <- dbGetQuery(con, sprintf("
      SELECT 
        city_count,
        COUNT(DISTINCT CONCAT(pixel_rid, '-', pixel_x, '-', pixel_y)) as pixel_count,
        AVG(overlap_percentage) as avg_overlap_pct
      FROM %s
      WHERE is_multi_city_pixel = true
      GROUP BY city_count
      ORDER BY city_count
    ", output_table))
    
    if (nrow(multi_city_stats) > 0) {
      cat("\nMulti-city pixel distribution:\n")
      print(multi_city_stats)
    }
  }
  
  return(TRUE)
}

# Convenience wrapper with optimal defaults
create_pixel_dataset <- function(
  db_name = "worldpop_db",
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  recreate = FALSE
) {
  
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = db_name,
    host = host,
    port = port,
    user = user,
    password = password
  )
  
  tryCatch({
    result <- create_pixel_annulus_dataset(
      con = con,
      recreate_table = recreate,
      verbose = TRUE
    )
    return(result)
  }, finally = {
    dbDisconnect(con)
  })
}