library(RPostgres)
library(DBI)
library(sf)
library(dplyr)
library(readr)
library(tidyr)

# Load city location function
source("R/functions/get_city_lat_lon_from_web.R")

# Upload city rings to PostGIS with equal areas regardless of latitude
upload_city_rings_to_postgis <- function(
  con,
  max_radius_km = 150,
  ring_interval_km = 1,
  table_name = "city_rings",
  recreate_table = FALSE,
  verbose = TRUE
) {
  
  if (verbose) cat("=== Uploading City Rings to PostGIS ===\n")
  
  # Step 1: Get city locations
  if (verbose) cat("Loading city locations...\n")
  cities_sf <- get_city_locations()
  
  if (nrow(cities_sf) == 0) {
    stop("No city locations found")
  }
  
  if (verbose) cat("Found", nrow(cities_sf), "cities\n")
  
  # Step 2: Create rings table if needed
  if (recreate_table || !dbExistsTable(con, table_name)) {
    if (verbose) cat("Creating rings table...\n")
    
    if (dbExistsTable(con, table_name)) {
      dbExecute(con, sprintf("DROP TABLE %s", table_name))
    }
    
    # Create table with proper schema
    create_table_sql <- sprintf("
      CREATE TABLE %s (
        city_id SERIAL,
        city_name VARCHAR(255),
        country VARCHAR(255),
        geoname_id BIGINT,
        center_lon DOUBLE PRECISION,
        center_lat DOUBLE PRECISION,
        ring_km INTEGER,
        ring_radius_m INTEGER,
        ring_area_m2 DOUBLE PRECISION,
        ring_geom GEOMETRY(POLYGON, 4326),
        PRIMARY KEY (city_id, ring_km)
      )
    ", table_name)
    
    dbExecute(con, create_table_sql)
    
    # Create spatial index
    dbExecute(con, sprintf("CREATE INDEX %s_geom_idx ON %s USING GIST (ring_geom)", table_name, table_name))
    dbExecute(con, sprintf("CREATE INDEX %s_city_idx ON %s (city_id)", table_name, table_name))
    dbExecute(con, sprintf("CREATE INDEX %s_ring_idx ON %s (ring_km)", table_name, table_name))
    
    if (verbose) cat("✓ Table created with spatial indexes\n")
  }
  
  # Step 3: Generate rings for each city with EQUAL AREAS
  if (verbose) cat("Generating equal-area rings for all cities...\n")
  
  # Create ring distances (in meters for equal area calculations)
  ring_distances_m <- seq(ring_interval_km * 1000, max_radius_km * 1000, by = ring_interval_km * 1000)
  ring_distances_km <- ring_distances_m / 1000
  
  if (verbose) cat("Creating", length(ring_distances_m), "rings per city from", 
                   min(ring_distances_km), "km to", max(ring_distances_km), "km\n")
  
  # Process cities in batches to manage memory
  batch_size <- 50  # Process 50 cities at a time
  total_cities <- nrow(cities_sf)
  
  for (batch_start in seq(1, total_cities, batch_size)) {
    batch_end <- min(batch_start + batch_size - 1, total_cities)
    
    if (verbose) cat("Processing cities", batch_start, "to", batch_end, "of", total_cities, "...\n")
    
    # Get batch of cities
    cities_batch <- cities_sf[batch_start:batch_end, ]
    
    # Extract coordinates for this batch
    city_coords <- st_coordinates(cities_batch)
    cities_batch$center_lon <- city_coords[, 1]
    cities_batch$center_lat <- city_coords[, 2]
    
    # Create city_id for this batch
    cities_batch$city_id <- batch_start:batch_end
    
    # Prepare city data without geometry for joining
    cities_data <- cities_batch %>% 
      st_drop_geometry() %>% 
      select(city_id, city_name = name, country, geoname_id, population, 
             center_lon, center_lat)
    
    # Generate all rings for this batch using GEOGRAPHY for equal areas
    rings_batch <- expand.grid(
      city_id = cities_batch$city_id,
      ring_km = ring_distances_km,
      stringsAsFactors = FALSE
    ) %>%
      left_join(cities_data, by = "city_id") %>%
      mutate(
        ring_radius_m = ring_km * 1000,
        # Calculate theoretical area (cumulative circle for now)
        ring_area_m2 = pi * (ring_radius_m^2)
      )
    
    # Create equal-area rings using geography calculations
    if (verbose) cat("  → Creating", nrow(rings_batch), "equal-area rings...\n")
    
    # Convert back to sf for ring creation with GEOGRAPHY calculations
    # Preserve coordinates before st_as_sf consumes them
    rings_sf <- rings_batch %>%
      mutate(
        # Store coordinates separately before st_as_sf consumes them
        center_lon_preserve = center_lon,
        center_lat_preserve = center_lat
      ) %>%
      st_as_sf(coords = c("center_lon", "center_lat"), crs = 4326) %>%
      # CRITICAL: Upload cumulative circles, create annulus in PostGIS later
      # This avoids complex geometry operations that can fail in R
      mutate(
        ring_geom = st_buffer(geometry, dist = ring_radius_m),
        # Calculate actual area (cumulative circle for now)
        ring_area_m2 = as.numeric(st_area(st_buffer(geometry, dist = ring_radius_m))),
        # Restore coordinate columns for database storage
        center_lon = center_lon_preserve,
        center_lat = center_lat_preserve
      ) %>%
      # Clean up and prepare for database
      st_drop_geometry() %>%
      st_set_geometry("ring_geom") %>%
      select(city_id, city_name, country, geoname_id, center_lon, center_lat, 
             ring_km, ring_radius_m, ring_area_m2, ring_geom)
    
    # Verify equal areas (should be very close regardless of latitude)
    if (verbose) {
      area_check <- rings_sf %>%
        st_drop_geometry() %>%
        filter(ring_km == 10) %>%  # Check 10km rings
        summarise(
          min_area = min(ring_area_m2),
          max_area = max(ring_area_m2),
          area_variation = (max(ring_area_m2) - min(ring_area_m2)) / mean(ring_area_m2) * 100
        )
      
      cat("  ✓ Area verification for 10km rings: variation =", 
          round(area_check$area_variation, 4), "% (should be <0.01%)\n")
    }
    
    # Upload batch to database
    if (verbose) cat("  → Uploading batch to database...\n")
    
    tryCatch({
      st_write(rings_sf, con, table_name, append = TRUE, quiet = !verbose)
      if (verbose) cat("  ✓ Batch uploaded successfully\n")
    }, error = function(e) {
      cat("  ✗ Error uploading batch:\", e$message, \"\n")
      stop("Database upload failed")
    })
    
    # Small pause between batches
    if (batch_end < total_cities) {
      Sys.sleep(1)
    }
  }
  
  # Step 4: Analyze and verify results
  if (verbose) {
    cat("\n=== Final Verification ===\n")
    
    # Get final counts
    counts <- dbGetQuery(con, sprintf("
      SELECT 
        COUNT(DISTINCT city_id) as total_cities,
        COUNT(DISTINCT ring_km) as rings_per_city,
        COUNT(*) as total_rings,
        MIN(ring_area_m2) as min_area_m2,
        MAX(ring_area_m2) as max_area_m2,
        AVG(ring_area_m2) as avg_area_m2
      FROM %s
    ", table_name))
    
    cat("Total cities:", counts$total_cities, "\n")
    cat("Rings per city:", counts$rings_per_city, "\n") 
    cat("Total rings:", counts$total_rings, "\n")
    cat("Area range:", round(counts$min_area_m2 / 1e6, 2), "to", 
        round(counts$max_area_m2 / 1e6, 2), "km²\n")
    
    # Check area consistency for specific ring sizes
    area_consistency <- dbGetQuery(con, sprintf("
      SELECT 
        ring_km,
        COUNT(*) as city_count,
        MIN(ring_area_m2) as min_area,
        MAX(ring_area_m2) as max_area,
        (MAX(ring_area_m2) - MIN(ring_area_m2)) / AVG(ring_area_m2) * 100 as area_variation_pct
      FROM %s 
      WHERE ring_km IN (1, 10, 50, 100, 150)
      GROUP BY ring_km
      ORDER BY ring_km
    ", table_name))
    
    cat("\nArea consistency check:\n")
    print(area_consistency)
    
    max_variation <- max(area_consistency$area_variation_pct)
    if (max_variation < 0.01) {
      cat("✓ Perfect equal-area rings achieved! Max variation:", round(max_variation, 6), "%\n")
    } else {
      cat("⚠ Area variation detected:", round(max_variation, 4), "%\n")
    }
  }
  
  if (verbose) cat("\n=== Upload Complete ===\n")
  return(TRUE)
}

# Convenience function for quick setup
create_city_rings_table <- function(
  db_name = "worldpop_db",
  table_name = "city_rings",
  max_radius_km = 150,
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey"),
  password = Sys.getenv("POSTGRES_PASSWORD")
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
    result <- upload_city_rings_to_postgis(
      con = con,
      max_radius_km = max_radius_km,
      table_name = table_name,
      recreate_table = TRUE
    )
    return(result)
  }, finally = {
    dbDisconnect(con)
  })
}