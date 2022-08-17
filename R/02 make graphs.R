#This is the graph making portion - just an interum step currently until a better UI can be created

library(sf)
library(leaflet)
library(tidyverse)
library(curl)
library(ggmap)

source("R/get_geo_files_from_web.R")

city_locations_sf <- get_city_locations(cities_to_import=2) 
map_data_for_cities <- map_df(city_locations_sf$rds_file_name,readRDS)

sf_use_s2(FALSE)

#Group each 30 arc box in the city into a radii of 1km distances 
city_by_1km_radii <- map_data_for_cities %>% 
  group_by(dist_km_round,city) %>%
  summarise(population = sum(population,na.rm = TRUE),
            area = sum(area, na.rm = TRUE)
  ) %>% 
  mutate(density = population/area)

#Create a graph that shows each city compared to Melbourne
create_mel_line <- function(new_city) {
  city_by_1km_radii %>% 
    filter(city == "Melbourne") %>%
    mutate(city = new_city,
           city_graph = "Melbourne")
  
}
all_mel_cities <-map_df(city_locations_sf$city_name,create_mel_line)

city_by_1km_radii %>% 
  mutate(city_graph = "other") %>% 
  bind_rows(all_mel_cities) %>% 
  filter(city!="Melbourne",
         dist_km_round<50) %>% 
  ggplot(aes(x = dist_km_round, 
             y = as.numeric(density),
             colour = city_graph))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~city,scales = "free_y")


srl_stations <- 
  tribble(~name,~lon,~lat, 
          "southland",-37.95814389091898, 145.0488637133262,
          "Clayton",-37.924386085737005, 145.1205863538048,
          "Monash",-37.91485520273969, 145.1327532200224,
          "Glen Waverley",-37.87954006753537, 145.16186175558119,
          "Burwood",-37.84987130765103, 145.113955991112,
          "Box Hill",-37.81711527337461, 145.12273944628174,
          "Doncaster",-37.78628008631726, 145.1264186984583,
          "Heidelberg",-37.757246850366236, 145.0611670648962,
          "Reservoir",-37.7168373116437, 145.00701711277185,
          "Latrobe", -37.72459394582586, 145.05015385087205,
          "Broadmeadows",-37.6827601919551, 144.9197552728396,
          "Fawkner",-37.714404888024156, 144.96005965804608)
          

city_by_1km_radii %>% 
  filter(dist_km_round<50) %>%
  group_by(dist_km_round) %>% 
  mutate(change_required = density[city == "London"]/density) %>% 
  filter(city %in% c("Melbourne")) %>% 
  filter(dist_km_round<30,
         !is.na(population)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  ggplot() +
  geom_sf(aes(fill = as.numeric(change_required)))+
  scale_fill_continuous(labels = scales::percent_format()) +
  ggthemes::theme_map()+
  theme(panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank())+
  geom_point(data = srl_stations, aes(x = lat, y = lon), colour = "white")+
  labs(fill = "Change in population density required\nfor London to be as dense as London",
       title = "Suburban rail loop is being built on the outskirts of where our population should be rising if we want a 'London' style density",
       subtitle = "SRL stations in white")
