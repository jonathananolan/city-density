library(tidyverse)
library(sf)

all_cities <- list.files("data/city_summaries/1km",pattern = "*")

output <- map_df(all_cities,~read_rds(paste0("data/city_summaries/1km/",.x)))


source("R/functions/get_city_lat_lon_from_web.R")
cities_list <- get_google_city_locations() %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  distinct(city_ascii,.keep_all = T)
library(leaflet)


output  %>% 
  filter(dist_km_round <100) %>% 
  filter(city_ascii == "United States New York") %>% 
  st_transform("wgs84") %>% 
  leaflet() %>% 
  addPolygons() 



output %>% 
  filter(dist_km_round<100) %>% 
  st_drop_geometry() %>% 
  group_by(dist_km_round) %>% 
  summarise(area = max(area_with_water)) %>% 
  ggplot(aes(x = dist_km_round, y = area))+
                 geom_line(stat = "identity")

output_long <- output %>% 
  left_join(cities_list, 
            by = "city_ascii") %>% 
  group_by(dist_km_round) %>% 
  filter(dist_km_round<100) %>% 
  mutate(density_without_water = population / area_without_water * 1e-6,
         density_with_water = population / max(area_with_water) * 1e-6) %>% 
  ungroup() %>% 
  select(-pwd_without_water,-pwd_cum_without_water,-area_cum_with_water) %>% 
  pivot_longer(c(pwd_cum_with_water,
                 area_without_water,
                 area_with_water,
                 pwd_with_water,
                 population,
                 population_cum,
                 density_cum_with_water,
                 density_cum_without_water,
                 density_with_water,
                 density_without_water,
                 area_cum_without_water),
               names_to = "metric",
               values_to = "value") %>% 
  mutate(population_weighted = case_when(str_detect(metric, "pwd")             ~ "Pwd",
                                         str_detect(metric, "density")         ~ "Den",
                                         str_detect(metric, "population|area") ~ NA_character_),
         cumulative = case_when(str_detect(metric, "cum") ~ T,
                                                        T ~ F),
         metric_type = case_when(str_detect(metric, "density|pwd") ~ "D",
                                 str_detect(metric, "area")        ~ "A",
                                 str_detect(metric, "population")  ~ "P"),
         include_water = case_when(str_detect(metric, "without")  ~ F,
                                   str_detect(metric, "with")     ~  T)
         ) %>% 
  dplyr::select(country,
         city,
         `Distance from the city centre in KM` = dist_km_round,
         metric_type,
         population_weighted,
         cumulative,
         include_water,
         value) 
  

output_long %>% st_drop_geometry() %>%   write_csv("shiny_csv.csv")
