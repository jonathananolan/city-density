library(tidyverse)
library(ggmap) 
library(geosphere)
library(rasterVis)
library(sf)
library(raster)
library(leaflet)

#geodata from https://hub.worldpop.org/geodata/listing?id=76
raster_files <- list.files(path = "data",full.names = TRUE, pattern = ".tif")



joiner <- function(x) {
 raster(x) %>% 
  as.data.frame(xy = TRUE) %>% 
    rename( population = 3)
  }

km_density_df <- map_df(raster_files,joiner)

city_names <- c("Brunswick, Melbourne",
                "Kreuzberg, Berlin",
                "Dalston, London",
                "Williamsberg New York City")

register_google("AIzaSyAWqTGrB3D4xjKzdSzh-izgNfkRLYp4JD4")
city_locations <- geocode(city_names) %>% bind_cols(tibble(locations = city_names))

city_locations %>% 
  leaflet::leaflet() %>% 
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(locations), label = ~as.character(locations))
  

city_filter <- function(city){
  
  print(city)
  lat_lon <- city_locations %>% filter(locations == city)
  
  size_of_get = .3
  km_density_df %>% filter(
    y > lat_lon$lat - size_of_get,
    y < lat_lon$lat + size_of_get,
    x > lat_lon$lon - size_of_get,
    x < lat_lon$lon + size_of_get) %>% 
    mutate(dist = distHaversine(cbind(x, y), c(lat_lon$lon,lat_lon$lat)),
           city = city) 
  
}

all_areas <- map_df(city_names,city_filter) 


all_cities <- all_areas %>% 
  mutate(dist_1000 = dist/1000,
         dist_round = round(dist_1000)) %>% 
  filter(dist_round <10) %>% 
  group_by(dist_round,city) %>%
  summarise(population = mean(population,na.rm = TRUE))


all_cities %>% 
  ggplot(aes(x = dist_round, 
             y = population,
             colour = city,
             fill = city))+
  geom_point(size = 4)+
  geom_smooth()+
  labs(title = "Brunswick's population density is lower than other great places",
       subtitle = "Average population density of 1km square areas grouped by distance from location",
       x = "Distance from location",
       y = "Average of population of 1km^2 areas at a given distance",
       caption = "Note: areas that are parks, industrial etc included. Areas completely over water excluded.\nAreas partially over water not yet accounted for in this graph.")+
  theme_minimal()+
  scale_y_continuous(labels = scales::comma_format())
