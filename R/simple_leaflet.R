# SIMPLE LEAFLET - Sydney Ring 5 with Water Raster Overlay
library(RPostgres)
library(DBI)
library(leaflet)
library(sf)

# Connect to database
con <- dbConnect(RPostgres::Postgres(),
                dbname = "worldpop_db", host = "localhost", port = 5432,
                user = Sys.getenv("POSTGRES_USER", "abrey"),
                password = Sys.getenv("POSTGRES_PASSWORD"))

# Get ring 3 boundary and water percentage from sydney_fixed_water
ring3 <- st_read(con, query = "
  SELECT annulus_geom as geom
  FROM city_annulus
  WHERE geoname_id = 2147714 AND ring_number = 3;
")

# Get actual water percentage for ring 3
water_stats <- dbGetQuery(con, "
  SELECT ROUND((ST_SummaryStats(water_raster, 1)).mean::numeric, 2) as water_mean
  FROM sydney_fixed_water
  WHERE annulus_number = 3;
")

dbDisconnect(con)

# Convert water percentage to opacity (divide by 100 to get 0-1 range)
water_opacity <- water_stats$water_mean / 100

leaflet() %>%
  addTiles() %>%
  addPolygons(data = ring3,
              color = "red",
              weight = 2,
              fillColor = "blue",
              fillOpacity = water_opacity,
              popup = paste0("Sydney Ring 3<br>Water coverage = ", round(water_stats$water_mean, 1), "%")) %>%
  setView(lng = 151.2093, lat = -33.8688, zoom = 12)