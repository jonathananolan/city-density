#For each city, find out how many people live within 10km. 
#Then if that city is within 50km of a more populous city by that metric, 
#add it to the list of 'small cities' that can be excluded from analysis

library(sf)
library(tidyverse)

find_files <- list.files("data/ghsl_sf/",full.names = T,pattern = "1km")

city_data <- map_df(.f = readRDS,
                    .x = find_files)


source("R/get_city_lat_lon_from_web.R")


#Ascii problems made it easy to exclude these cities

manual_cities_to_exclude <- tibble(city_ascii =c("Philippines Quezon City",
                                                 "Philippines San Juan")) 



cities_list <- get_google_city_locations() %>%
  st_transform(st_crs(city_data)) %>% 
  filter(!(city_ascii %in% manual_cities_to_exclude$city_ascii))

#Get population within 10km
pop_x_km <- city_data %>%
  filter(!(city_ascii %in% manual_cities_to_exclude$city_ascii)) %>%
  group_by(city_ascii) %>% 
  filter(dist_km_round <= 10) %>% 
  st_drop_geometry() %>% 
  summarise(population = sum(population)) %>% 
  arrange(desc(population))

#add back in the geoemtry
cities_list_with_pop <- cities_list %>% 
  distinct(geometry,.keep_all = T) %>% 
  inner_join(pop_x_km) %>% 
  mutate(i =row_number())

#Create a big dataset wtih the distance between cities
n = nrow(cities_list_with_pop)
dm = st_distance(cities_list_with_pop)

ijd = data.frame(expand.grid(i=1:n, j=1:n))
ijd$distance = as.numeric(c(dm))

#Find where the distance between two cities is less than 5km
close_cities <- ijd %>% filter(distance < 50000,distance !=0) %>% 
  left_join(cities_list_with_pop %>% st_drop_geometry())

#find the min population of the group
smaller_cities <- close_cities %>% 
  group_by(distance) %>% 
  filter(population == min(population)) %>% 
  ungroup() %>%
  dplyr::select(city_ascii) %>%
  bind_rows(manual_cities_to_exclude)

#export to csv
smaller_cities %>% write_csv("data/smaller_cities.csv")
  