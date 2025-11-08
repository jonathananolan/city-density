library(RPostgres)
library(DBI)
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
# Load city location function
source("R/functions/get_city_lat_lon_from_web.R")

# STEP 1: Upload city centers with 150 rows per city
upload_city_centers <- function(
  con,
  geoname_id = NULL,  # Specific geoname_id to process, or NULL for all cities
  max_rings = 150,
  table_name = "city_centers",
  recreate_table = FALSE,
  verbose = TRUE
) {
  
  if (verbose) cat("=== STEP 1: Uploading City Centers ===\n")
  
  # Get all cities
  cities_sf <- get_city_locations()
  
  # Filter to specific city if geoname_id provided
  if (!is.null(geoname_id)) {
    city_match <- cities_sf[cities_sf$geoname_id == geoname_id, ]
    if (nrow(city_match) == 0) {
      stop("geoname_id ", geoname_id, " not found in cities data")
    }
    cities_sf <- city_match
    if (verbose) cat("Processing geoname_id", geoname_id, ":", cities_sf$name[1], ",", cities_sf$country[1], "\n")
  }
  
  # Create table if needed
  if (recreate_table || !dbExistsTable(con, table_name)) {
    if (verbose) cat("Creating city centers table...\n")
    
    if (dbExistsTable(con, table_name)) {
      dbExecute(con, sprintf("DROP TABLE %s CASCADE", table_name))
    }
    
    create_sql <- sprintf("
      CREATE TABLE %s (
        geoname_id BIGINT,
        city_name VARCHAR(255),
        country VARCHAR(255),
        population BIGINT,
        ring_number INTEGER,
        center_point GEOMETRY(POINT, 4326),
        PRIMARY KEY (geoname_id, ring_number)
      )
    ", table_name)
    
    dbExecute(con, create_sql)
    dbExecute(con, sprintf("CREATE INDEX %s_center_idx ON %s USING GIST (center_point)", table_name, table_name))
    dbExecute(con, sprintf("CREATE INDEX %s_city_idx ON %s (geoname_id)", table_name, table_name))
  }
  
  # Process each city
  for (i in 1:nrow(cities_sf)) {
    city_row <- cities_sf[i, ]
    city_coords <- st_coordinates(city_row)
    
    if (verbose) cat("Uploading geoname_id", city_row$geoname_id, ":", city_row$name, "with", max_rings, "ring rows\n")
    
    # Create 150 rows for this city (one per ring)
    city_data <- data.frame(
      geoname_id = city_row$geoname_id,
      city_name = city_row$name,
      country = city_row$country,
      population = city_row$population,
      ring_number = 1:max_rings,
      center_lon = city_coords[1],
      center_lat = city_coords[2]
    )
    
    # Convert to sf with point geometry
    city_sf <- city_data %>%
      st_as_sf(coords = c("center_lon", "center_lat"), crs = 4326) %>%
      rename(center_point = geometry)
    
    # Upload to database
    st_write(city_sf, con, table_name, append = TRUE, quiet = !verbose)
  }
  
  if (verbose) {
    count <- dbGetQuery(con, sprintf("SELECT COUNT(*) as total FROM %s", table_name))
    cat("✓ Uploaded", count$total, "rows to city centers table\n")
  }
  
  return(TRUE)
}

# STEP 2: Generate circles in PostGIS 
generate_circles_in_postgis <- function(
  con,
  geoname_id = NULL,  # Process specific geoname_id
  centers_table = "city_centers",
  circles_table = "city_circles", 
  recreate_table = FALSE,
  verbose = TRUE
) {
  
  if (verbose) cat("=== STEP 2: Generating Circles in PostGIS ===\n")
  
  # Create circles table if needed
  if (recreate_table || !dbExistsTable(con, circles_table)) {
    if (verbose) cat("Creating circles table...\n")
    
    if (dbExistsTable(con, circles_table)) {
      dbExecute(con, sprintf("DROP TABLE %s CASCADE", circles_table))
    }
    
    create_sql <- sprintf("
      CREATE TABLE %s (
        geoname_id BIGINT,
        city_name VARCHAR(255),
        country VARCHAR(255),
        ring_number INTEGER,
        ring_radius_km INTEGER,
        ring_radius_m INTEGER,
        center_point GEOMETRY(POINT, 4326),
        circle_geom GEOMETRY(POLYGON, 4326),
        circle_area_m2 DOUBLE PRECISION,
        PRIMARY KEY (geoname_id, ring_number)
      )
    ", circles_table)
    
    dbExecute(con, create_sql)
    dbExecute(con, sprintf("CREATE INDEX %s_circle_idx ON %s USING GIST (circle_geom)", circles_table, circles_table))
    dbExecute(con, sprintf("CREATE INDEX %s_city_idx ON %s (geoname_id)", circles_table, circles_table))
  }
  
  # Build WHERE clause for specific city if provided
  where_clause <- if (!is.null(geoname_id)) {
    sprintf("WHERE geoname_id = %d", geoname_id)
  } else {
    ""
  }
  
  if (verbose && !is.null(geoname_id)) {
    city_info <- dbGetQuery(con, sprintf("SELECT DISTINCT city_name FROM %s WHERE geoname_id = %d", centers_table, geoname_id))
    cat("Generating circles for geoname_id", geoname_id, ":", city_info$city_name, "\n")
  }
  
  # Generate circles using PostGIS ST_Buffer with proper CRS transformation
  insert_sql <- sprintf("
    INSERT INTO %s (
  geoname_id, city_name, country, ring_number, ring_radius_km, ring_radius_m,
  center_point,        -- geometry(POINT,4326)
  circle_geom,         -- geometry(POLYGON,4326)
  circle_area_m2       -- numeric
)
SELECT
  geoname_id,
  city_name,
  country,
  ring_number,
  ring_number AS ring_radius_km,
  ring_number * 1000 AS ring_radius_m,
  center_point,
  -- geodesic circle, stored as geometry in WGS84
  ST_SetSRID(
    (ST_Buffer(center_point::geography, ring_number * 1000))::geometry,
    4326
  ) AS circle_geom,
  -- true ellipsoidal area in m²
  ST_Area(ST_Buffer(center_point::geography, ring_number * 1000)) AS circle_area_m2
FROM %s
%s
ORDER BY geoname_id, ring_number;
  ", circles_table, centers_table, where_clause)
  
  rows_inserted <- dbExecute(con, insert_sql)
  
  if (verbose) {
    cat("✓ Generated", rows_inserted, "circles in PostGIS\n")
    
    # Show sample results
    sample_where <- if (!is.null(geoname_id)) {
      sprintf("WHERE geoname_id = %d", geoname_id)
    } else {
      ""
    }
    
    sample_sql <- sprintf("
      SELECT city_name, ring_number, 
             CAST(circle_area_m2 / 1000000.0 AS NUMERIC(10,2)) as area_km2
      FROM %s 
      %s
      ORDER BY geoname_id, ring_number
      LIMIT 10
    ", circles_table, sample_where)
    
    sample <- dbGetQuery(con, sample_sql)
    cat("Sample circles:\n")
    print(sample)
  }
  
  return(TRUE)
}

# STEP 3: Generate annulus (donut rings) in PostGIS
generate_annulus_in_postgis <- function(
  con,
  geoname_id = NULL,  # Process specific geoname_id
  circles_table = "city_circles",
  annulus_table = "city_annulus",
  recreate_table = FALSE,
  verbose = TRUE
) {
  
  if (verbose) cat("=== STEP 3: Generating Annulus in PostGIS ===\n")
  
  # Create annulus table if needed
  if (recreate_table || !dbExistsTable(con, annulus_table)) {
    if (verbose) cat("Creating annulus table...\n")
    
    if (dbExistsTable(con, annulus_table)) {
      dbExecute(con, sprintf("DROP TABLE %s CASCADE", annulus_table))
    }
    
    create_sql <- sprintf("
      CREATE TABLE %s (
        geoname_id BIGINT,
        city_name VARCHAR(255),
        country VARCHAR(255),
        ring_number INTEGER,
        ring_radius_km INTEGER,
        inner_radius_km INTEGER,
        outer_radius_km INTEGER,
        center_point GEOMETRY(POINT, 4326),
        annulus_geom GEOMETRY(POLYGON, 4326),
        annulus_area_m2 DOUBLE PRECISION,
        PRIMARY KEY (geoname_id, ring_number)
      )
    ", annulus_table)
    
    dbExecute(con, create_sql)
    dbExecute(con, sprintf("CREATE INDEX %s_annulus_idx ON %s USING GIST (annulus_geom)", annulus_table, annulus_table))
    dbExecute(con, sprintf("CREATE INDEX %s_city_idx ON %s (geoname_id)", annulus_table, annulus_table))
  }
  
  # Build WHERE clause for specific city if provided
  where_clause <- if (!is.null(geoname_id)) {
    sprintf("WHERE outer_circle.geoname_id = %d", geoname_id)
  } else {
    ""
  }
  
  if (verbose && !is.null(geoname_id)) {
    city_info <- dbGetQuery(con, sprintf("SELECT DISTINCT city_name FROM %s WHERE geoname_id = %d", circles_table, geoname_id))
    cat("Generating annulus for geoname_id", geoname_id, ":", city_info$city_name, "\n")
  }
  
  # Generate annulus using PostGIS ST_Difference with properly transformed circles
  insert_sql <- sprintf("
    INSERT INTO %s (
      geoname_id, city_name, country, ring_number, ring_radius_km,
      inner_radius_km, outer_radius_km, center_point, annulus_geom, annulus_area_m2
    )
    SELECT 
      outer_circle.geoname_id,
      outer_circle.city_name,
      outer_circle.country,
      outer_circle.ring_number,
      outer_circle.ring_radius_km,
      CASE 
        WHEN outer_circle.ring_number = 1 THEN 0 
        ELSE outer_circle.ring_number - 1 
      END as inner_radius_km,
      outer_circle.ring_radius_km as outer_radius_km,
      outer_circle.center_point,
      CASE 
        WHEN outer_circle.ring_number = 1 THEN outer_circle.circle_geom  -- First ring is full circle
        ELSE ST_Difference(outer_circle.circle_geom, inner_circle.circle_geom)  -- Subtract inner from outer
      END as annulus_geom,
      CASE 
        WHEN outer_circle.ring_number = 1 THEN ST_Area(outer_circle.circle_geom::geography)
        ELSE ST_Area(ST_Difference(outer_circle.circle_geom, inner_circle.circle_geom)::geography)
      END as annulus_area_m2
    FROM %s outer_circle
    LEFT JOIN %s inner_circle ON 
      outer_circle.geoname_id = inner_circle.geoname_id AND
      inner_circle.ring_number = outer_circle.ring_number - 1
    %s
    ORDER BY outer_circle.geoname_id, outer_circle.ring_number
  ", annulus_table, circles_table, circles_table, where_clause)
  
  rows_inserted <- dbExecute(con, insert_sql)
  
  if (verbose) {
    cat("✓ Generated", rows_inserted, "annulus rings in PostGIS\n")
    
    # Show sample results  
    sample_where <- if (!is.null(geoname_id)) {
      sprintf("WHERE geoname_id = %d", geoname_id)
    } else {
      ""
    }
    
    sample_sql <- sprintf("
      SELECT city_name, ring_number, inner_radius_km, outer_radius_km,
             CAST(annulus_area_m2 / 1000000.0 AS NUMERIC(10,2)) as area_km2
      FROM %s 
      %s
      ORDER BY geoname_id, ring_number
      LIMIT 10
    ", annulus_table, sample_where)
    
    sample <- dbGetQuery(con, sample_sql)
    cat("Sample annulus rings:\n")
    print(sample)
  }
  
  return(TRUE)
}

# WRAPPER FUNCTION: Run all steps sequentially
process_city_rings_complete <- function(
  geoname_id = NULL,  # Process specific geoname_id, or NULL for all cities
  steps_to_run = c(1, 2, 3),  # Which steps to execute
  db_name = "worldpop_db",
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  max_rings = 150,
  recreate_tables = FALSE,
  verbose = TRUE
) {
  
  if (verbose) {
    cat("=== Processing City Rings: Complete Workflow ===\n")
    if (!is.null(geoname_id)) {
      cat("Processing geoname_id:", geoname_id, "\n")
    } else {
      cat("Processing ALL cities\n")
    }
    cat("Steps to run:", paste(steps_to_run, collapse = ", "), "\n\n")
  }
  
  # Connect to database
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = db_name,
    host = host,
    port = port,
    user = user,
    password = password
  )
  
  tryCatch({
    
    # STEP 1: Upload city centers
    if (1 %in% steps_to_run) {
      upload_city_centers(
        con = con,
        geoname_id = geoname_id,
        max_rings = max_rings,
        recreate_table = recreate_tables,
        verbose = verbose
      )
    }
    
    # STEP 2: Generate circles
    if (2 %in% steps_to_run) {
      generate_circles_in_postgis(
        con = con,
        geoname_id = geoname_id,
        recreate_table = recreate_tables,
        verbose = verbose
      )
    }
    
    # STEP 3: Generate annulus
    if (3 %in% steps_to_run) {
      generate_annulus_in_postgis(
        con = con,
        geoname_id = geoname_id,
        recreate_table = recreate_tables,
        verbose = verbose
      )
    }
    
    if (verbose) cat("\n=== Workflow Complete ===\n")
    return(TRUE)
    
  }, finally = {
    dbDisconnect(con)
  })
}

# CONVENIENCE FUNCTIONS for testing individual cities
test_single_city <- function(geoname_id, steps = c(1, 2, 3)) {
  process_city_rings_complete(
    geoname_id = geoname_id,
    steps_to_run = steps,
    recreate_tables = TRUE,  # Clean slate for testing
    verbose = TRUE
  )
}


cities_sf <- get_city_locations()

walk(cities_sf$geoname_id,process_city_rings_complete)
