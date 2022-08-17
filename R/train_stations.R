library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

map_data <- list.files("data", ".RDS", full.names = T)

rds_importer <- function(x) {
  test <- readRDS(x) %>% 
    rename_with( .fn = ~paste0("population"), .cols = contains("pd_2020")) 
  
}

map_data_for_cities <- map_df(map_data,rds_importer)

library(tidyverse)
library(ggmap) 
library(geosphere)
library(rasterVis)
library(raster)
library(stars)
library(osmdata)
library(sf)
library(leaflet)


#Get water bodies to filter the out from pop density calculation
# http://gis.ess.washington.edu/data/vector/worldshore/index.html

water_bodies_global <- st_read("data/wash_water_bodies/shore_ne/shore_ne.shp") %>% 
  bind_rows(st_read("data/wash_water_bodies/shore_se/shore_se.shp") )%>% 
  bind_rows(st_read("data/wash_water_bodies/shore_ne/shore_ne.shp") )%>% 
  bind_rows(st_read("data/wash_water_bodies/shore_nw/shore_nw.shp") )


#get cities of interest 

city_names <- tribble( ~ location, ~ country, 
                       "Melbourne","Australia",
                       "Sydney","Australia",
                       "Berlin","Germany",
                       "London","UK",
                       "New York", "USA",
                       "Los Angeles", "USA",
                       "Chicago", "USA",
                       "San Francisco","USA",
                       "Tokyo","Japan")

#population density in 1km/grid raster from https://hub.worldpop.org/geodata/listing?id=76

filenames <- tribble(~country,~filename,
                     "Australia", "data/aus_pd_2020_1km.tif",
                     "USA", "data/usa_pd_2020_1km.tif",
                     "UK","data/gbr_pd_2020_1km.tif",
                     "Germany","data/deu_pd_2020_1km.tif",
                     "Japan","data/jpn_pd_2020_1km.tif")

register_google("AIzaSyAWqTGrB3D4xjKzdSzh-izgNfkRLYp4JD4")

city_locations_sf <- geocode(city_names$location) %>% bind_cols(city_names) %>% 
  left_join(filenames) %>% 
  sf::st_as_sf(coords = c("lon","lat"), crs = st_crs(poly))


bb_around_point <- function(x,xsize,ysize){
  bbox <- st_bbox(x)
  bbox <- bbox +c(xsize/2,ysize/2,-xsize/2,-ysize/2)
  return((bbox))
}

city = "London"

bb_around <- bb_around_point(city_locations_sf %>% filter(location == city), -2.1,-1.1)


q <-bb_around %>%
  opq() %>%
  add_osm_feature("public_transport", "station") %>% 
  osmdata_sf () 

q$osm_points %>% 
  ggplot() +
  geom_sf()



# calculate difference between centroids
offset <- st_geometry(city_locations_sf %>% filter(location == "Melbourne")) - st_geometry(city_locations_sf %>% filter(location == "London"))

# intiate resultset
berlin_landmarks <- q$osm_points

# perform affine transformation on geometry column; this will mess up CRS
st_geometry(berlin_landmarks) <- st_geometry(berlin_landmarks) + offset 

# repair CRS - it is the same as London landmarks
berlin_landmarks <- st_set_crs(berlin_landmarks, st_crs(q$osm_points))


library(readxl)
library(leaflet)

passenger_numbers <- read_csv("data/0312-1920-Station Usage Data.csv") %>% 
  group_by(name = `Rail Station Name`,`Calendar Week`) %>% 
  summarise(passengers = sum(`Count of Taps`),
            n = n()) %>% 
  group_by(name) %>% 
  summarise(passengers = mean(passengers)) %>% 
  mutate(name = str_remove_all(name, " DLR")) %>% 
  mutate(name = str_remove_all(name, " LU")) %>% 
  mutate(name = str_remove_all(name, " (Tube)")) %>% 
  mutate(name = str_remove_all(name, "'")) %>% 
  mutate(name = str_remove_all(name, "Pancras")) 




berlin_landmarks_joined <- berlin_landmarks %>%
  mutate(name = str_remove_all(name, "'")) %>% 
  mutate(name = str_remove_all(name, "[.]")) %>% 
  dplyr::select(name,network) %>% 
  left_join(passenger_numbers) %>% 
  filter(str_detect(network,"London Underground|berlin_landmarks_joined|London Overground")) 

view(berlin_landmarks_joined)


berlin_landmarks_joined %>% 
  ggplot() +
  geom_sf(aes(alpha = passengers))+
  geom_point(data = srl_stations, aes(x = lat, y = lon),
             colour = "white", size = 5)
  
