library(tidyverse)
library(sf)

all_cities <- list.files("data/city_summaries/1km",pattern = "*")

output <- map_df(all_cities,~read_rds(paste0("data/city_summaries/1km/",.x)))


source("R/functions/get_city_lat_lon_from_web.R")
cities_list <- get_google_city_locations() %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

output_long <- output %>% 
  left_join(cities_list, 
            by = "city_ascii") %>% 
  mutate(density_without_water = population / area_without_water * 1e-6,
         density_with_water = population / area_with_water * 1e-6) %>% 
  pivot_longer(c(pwd_cum_with_water,
                 pwd_cum_without_water,
                 area_without_water,
                 area_with_water,
                 pwd_with_water,
                 pwd_without_water,
                 population,
                 population_cum,
                 density_cum_with_water,
                 density_cum_without_water,
                 density_with_water,
                 density_without_water,
                 area_cum_with_water,
                 area_cum_without_water),
               names_to = "metric",
               values_to = "value") %>% 
  st_drop_geometry() %>% 
  mutate(population_weighted = case_when(str_detect(metric, "pwd")             ~ "Population weighted density",
                                         str_detect(metric, "density")         ~ "Density",
                                         str_detect(metric, "population|area") ~ NA_character_),
         cumulative = case_when(str_detect(metric, "cum") ~ "Cumulative",
                                                        T ~ "Not cumulative"),
         metric_type = case_when(str_detect(metric, "density|pwd") ~ "Density",
                                 str_detect(metric, "area")        ~ "Area",
                                 str_detect(metric, "population")  ~ "Population"),
         include_water = case_when(str_detect(metric, "without")  ~ "Not counting water bodies in area calculation",
                                   str_detect(metric, "with")     ~     "Counting water bodies in area calculation")
         ) %>% 
  dplyr::select(country,
         city,
         lat,
         lon,
         `Distance from the city centre in KM` = dist_km_round,
         metric_type,
         population_weighted,
         cumulative,
         include_water,
         value) %>% 
  write_csv("city_density.csv")
