library(tidyverse)
library(sf)
library(leaflet)

all_cities <- list.files("data/city_summaries/1km_without_water",pattern = "*")

all_circles <- list.files("data/city_summaries/ghsl_radii_circle_rds/",pattern = "*",full.names = T)

source("R/functions/get_city_lat_lon_from_web.R")
cities_list_for_shiny <- get_city_locations() %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])  %>% 
  rename(geonames_pop = population) %>%
  st_drop_geometry() %>% 
  mutate(city_name = paste0(name," (",country,")"))



circles <- map_df(all_circles,read_rds) %>% 
  left_join(cities_list_for_shiny, 
            by = "geoname_id") %>% 
  group_by(dist_km_round) %>% 
  filter(dist_km_round<101) %>% 
  select(-tiles_km,-tiles_cum,
         -area_with_water,
         -area_without_water,
         -area_cum_without_water,
         -area_cum_with_water
         ) %>% 
  mutate(pwd_with_water = pwd_with_water *1e6,
         pwd_cum_with_water = pwd_cum_with_water *1e6) %>% 
  rename(city = name)


#Check that populations make sense: 

geonames_list <- read_delim("data/geonames-all-cities-with-a-population-1000.csv",delim = ";") %>% 
  select(`Feature Class`,
         geoname_id = `Geoname ID`,
         Coordinates)

#Cities that are small suburbs of existing cities or where we don't have an accurate geocode of their city we can filter out
unnecesary_cities <- 
  circles %>% filter(dist_km_round == 20) %>% 
  st_drop_geometry() %>% 
  mutate(population_ratio = population_cum/geonames_pop) %>% 
  mutate(google = paste0("https://www.google.com/maps/@",lat,",",lon,",11.46z")) %>% 
  select(city,country,geoname_id,population_cum,geonames_pop,population_ratio,source_of_lat_lon,google) %>% 
  filter(population_ratio > 14| 
         population_ratio < .4) %>% 
  pull(geoname_id)


library(qs) 
circles %>% filter(!(geoname_id %in% unnecesary_cities)) %>% st_drop_geometry() %>% qsave("output/qs_files/shiny.qs")

city_names <- unique(circles$city_name)

save_qs <- function(x){
  circles %>% 
    filter(city_name == x) %>%
    select(city_name,geometry,dist_km_round) %>%
    qsave(paste0("output/qs_files/",x,".qs"),)
}
walk(city_names,save_qs)

circles %>% filter(dist_km_round == 99) %>% 
  left_join(cities_list_for_reconciliation) %>% 
  st_drop_geometry() %>% 
  write_csv("data/cities_list_clean.csv")


