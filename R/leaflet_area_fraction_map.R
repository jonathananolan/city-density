# LEAFLET MAP - Show area fraction for Sydney Ring 3
# Display each pixel colored by its area fraction (0-1)

library(RPostgres)
library(DBI)
library(leaflet)
library(sf)

# Connect to database
con <- dbConnect(RPostgres::Postgres(),
                dbname = "worldpop_db", host = "localhost", port = 5432,
                user = Sys.getenv("POSTGRES_USER", "abrey"),
                password = Sys.getenv("POSTGRES_PASSWORD"))

cat("=== CREATING AREA FRACTION LEAFLET MAP ===\n")

# Get ring 3 boundary
ring3_boundary <- st_read(con, query = "
  SELECT annulus_geom as geom
  FROM city_annulus
  WHERE geoname_id = 2147714 AND ring_number = 3;
")

# Get area fraction data from the lookup table
cat("Extracting area fraction data for Sydney ring 3...\n")

area_data <- dbGetQuery(con, "
  SELECT
    pixel_x,
    pixel_y,
    ROUND(population::numeric, 1) as population,
    ROUND(area_fraction::numeric, 3) as area_fraction,
    ST_X(pixel_center) as longitude,
    ST_Y(pixel_center) as latitude
  FROM sydney_pixel_area_fractions
  WHERE geoname_id = 2147714 AND annulus_number = 3
  ORDER BY area_fraction;
")

dbDisconnect(con)

cat(sprintf("Extracted %d pixels with area fractions\n", nrow(area_data)))
cat(sprintf("Area fractions range: %.3f to %.3f\n", min(area_data$area_fraction), max(area_data$area_fraction)))

# Create color palette for area fraction (0-1)
# Use a gradient from red (partial) to green (full)
area_colors <- colorNumeric(palette = c("#FF4444", "#FFAA00", "#88DD44", "#44AA88"),
                           domain = c(0, 1))

# Separate full vs partial pixels for different visualization
full_pixels <- area_data[area_data$area_fraction == 1.0, ]
partial_pixels <- area_data[area_data$area_fraction < 1.0, ]

cat(sprintf("Full pixels (area_fraction = 1.0): %d\n", nrow(full_pixels)))
cat(sprintf("Partial pixels (area_fraction < 1.0): %d\n", nrow(partial_pixels)))

# Create leaflet map
map <- leaflet() %>%
  addTiles() %>%

  # Add ring 3 boundary as light gray background
  addPolygons(
    data = ring3_boundary,
    color = "gray",
    weight = 2,
    fillColor = "lightgray",
    fillOpacity = 0.1,
    popup = "Sydney Ring 3 Boundary"
  ) %>%

  # Add full pixels as small green dots
  addCircleMarkers(
    data = full_pixels,
    lng = ~longitude,
    lat = ~latitude,
    radius = 2,
    color = "#44AA88",
    fillColor = "#44AA88",
    fillOpacity = 0.6,
    stroke = FALSE,
    popup = ~paste0("Full Pixel (", pixel_x, ",", pixel_y, ")<br>",
                   "Population: ", population, " people<br>",
                   "Area fraction: ", area_fraction, " (100%)")
  ) %>%

  # Add partial pixels as larger colored dots
  addCircleMarkers(
    data = partial_pixels,
    lng = ~longitude,
    lat = ~latitude,
    radius = 4,
    color = ~area_colors(area_fraction),
    fillColor = ~area_colors(area_fraction),
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0("Partial Pixel (", pixel_x, ",", pixel_y, ")<br>",
                   "Population: ", population, " people<br>",
                   "Area fraction: ", area_fraction, " (", round(area_fraction*100, 1), "%)")
  ) %>%

  # Add color legend for area fractions
  addLegend(
    pal = area_colors,
    values = area_data$area_fraction,
    title = "Area Fraction<br>(% of pixel in ring)",
    position = "bottomright",
    labFormat = labelFormat(suffix = "", transform = function(x) x * 100)
  ) %>%

  # Center on the pixels
  setView(
    lng = mean(area_data$longitude),
    lat = mean(area_data$latitude),
    zoom = 13
  )

cat("Area fraction map created successfully\n")
cat("Green dots = full pixels (100% in annulus)\n")
cat("Colored dots = partial pixels (edge effects)\n")
cat("Color intensity shows area fraction percentage\n")

# Display the map
map