# LEAFLET MAP - Sydney Ring 5 Water Raster
# Display water percentage data on OpenStreetMap

library(RPostgres)
library(DBI)
library(leaflet)
library(sf)

# Connect to database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "worldpop_db",
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)

cat("=== CREATING LEAFLET MAP - SYDNEY RING 5 WATER ===\n")

# Get Sydney center coordinates from geometry
sydney_coords <- dbGetQuery(con, "
  SELECT
    ST_Y(center_point) as latitude,
    ST_X(center_point) as longitude
  FROM city_centers
  WHERE geoname_id = 2147714;
")

# Get ring 5 boundary for overlay
ring_boundary <- st_read(con, query = "
  SELECT
    ring_number,
    annulus_geom as geom
  FROM city_annulus
  WHERE geoname_id = 2147714 AND ring_number = 5;
")

# Get water raster extent for reference
water_extent <- dbGetQuery(con, "
  SELECT
    annulus_number,
    ST_Width(water_raster) as width,
    ST_Height(water_raster) as height
  FROM sydney_fixed_water
  WHERE annulus_number = 5;
")

dbDisconnect(con)

cat("Data loaded successfully\n")
cat(sprintf("Sydney center: %.4f, %.4f\n", sydney_coords$latitude, sydney_coords$longitude))
cat(sprintf("Ring 5 boundary loaded with %d features\n", nrow(ring_boundary)))
cat(sprintf("Water raster size: %d x %d pixels\n", water_extent$width, water_extent$height))

# Create leaflet map
map <- leaflet() %>%
  addTiles() %>%  # OpenStreetMap base layer
  setView(
    lng = sydney_coords$longitude,
    lat = sydney_coords$latitude,
    zoom = 11
  ) %>%

  # Add Sydney center point
  addMarkers(
    lng = sydney_coords$longitude,
    lat = sydney_coords$latitude,
    popup = "Sydney City Center"
  ) %>%

  # Add ring 5 boundary
  addPolygons(
    data = ring_boundary,
    color = "red",
    weight = 2,
    opacity = 0.8,
    fillOpacity = 0.1,
    popup = paste("Ring", ring_boundary$ring_number)
  ) %>%

  # Add layer control
  addLayersControl(
    overlayGroups = c("Ring Boundary"),
    options = layersControlOptions(collapsed = FALSE)
  )

cat("Map created successfully\n")
cat("Opening map in browser...\n")

# Display the map
map

cat("\n=== LEAFLET MAP COMPLETE ===\n")
cat("Map shows:\n")
cat("- OpenStreetMap base layer\n")
cat("- Sydney city center marker\n")
cat("- Ring 5 boundary (red outline)\n")
cat("- Centered on Sydney coordinates\n")