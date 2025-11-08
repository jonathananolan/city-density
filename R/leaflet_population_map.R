# LEAFLET MAP - Show every pixel of Sydney Ring 3 population raster
library(RPostgres)
library(DBI)
library(leaflet)
library(sf)

# Connect to database
con <- dbConnect(RPostgres::Postgres(),
                dbname = "worldpop_db", host = "localhost", port = 5432,
                user = Sys.getenv("POSTGRES_USER", "abrey"),
                password = Sys.getenv("POSTGRES_PASSWORD"))

cat("=== CREATING POPULATION PIXEL-LEVEL LEAFLET MAP ===\n")

# Get ring 3 boundary
ring3_boundary <- st_read(con, query = "
  SELECT annulus_geom as geom
  FROM city_annulus
  WHERE geoname_id = 2147714 AND ring_number = 3;
")

# Extract individual pixels from ring 3 population raster
cat("Extracting pixels from ring 3 population raster...\n")

pixels <- dbGetQuery(con, "
  SELECT
    (ST_PixelAsCentroids(population_raster, 1)).x as pixel_x,
    (ST_PixelAsCentroids(population_raster, 1)).y as pixel_y,
    (ST_PixelAsCentroids(population_raster, 1)).val as population,
    ST_X((ST_PixelAsCentroids(population_raster, 1)).geom) as longitude,
    ST_Y((ST_PixelAsCentroids(population_raster, 1)).geom) as latitude
  FROM sydney_fixed_water
  WHERE annulus_number = 3
    AND population_raster IS NOT NULL;
")

dbDisconnect(con)

cat(sprintf("Extracted %d pixels\n", nrow(pixels)))
cat(sprintf("Population values range: %.1f to %.1f people/pixel\n", min(pixels$population), max(pixels$population)))

# Create color palette for population density
pop_colors <- colorNumeric(palette = "Reds", domain = c(0, max(pixels$population)))

# Create leaflet map with ring boundary and individual pixels
map <- leaflet() %>%
  addTiles() %>%

  # Add ring 3 boundary as light red background
  addPolygons(
    data = ring3_boundary,
    color = "red",
    weight = 1,
    fillColor = "red",
    fillOpacity = 0.1,
    popup = "Sydney Ring 3 Boundary"
  ) %>%

  # Add each pixel as a circle (on top of ring)
  addCircleMarkers(
    data = pixels,
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    color = ~pop_colors(population),
    fillColor = ~pop_colors(population),
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~paste0("Pixel (", pixel_x, ",", pixel_y, ")<br>",
                   "Population: ", round(population, 1), " people")
  ) %>%

  # Add color legend
  addLegend(
    pal = pop_colors,
    values = pixels$population,
    title = "Population<br>per pixel",
    position = "bottomright"
  ) %>%

  # Center on the pixels
  setView(
    lng = mean(pixels$longitude),
    lat = mean(pixels$latitude),
    zoom = 14
  )

cat("Population pixel map created successfully\n")
cat("Each dot represents one 100m x 100m pixel\n")
cat("Color intensity shows population density\n")

# Display the map
map