

library(sf)
library(leaflet)
library(tidyverse)

source("R/inputs.R")

map_data <- list.files("data", ".RDS", full.names = T)

rds_importer <- function(x) {
test <- readRDS(x) %>% 
  rename_with( .fn = ~paste0("population"), .cols = contains("pd_2020")) 

}

map_data_for_cities <- map_df(map_data,rds_importer)


map_data_for_cities %>% 
  filter(city == "London") %>% 
  leaflet::leaflet() %>%
  addTiles() %>% 
  addPolygons()



sf_use_s2(FALSE)


all_cities <- map_data_for_cities %>% 
  group_by(dist_km_round,city) %>%
  summarise(population = sum(population,na.rm = TRUE),
            area = sum(area, na.rm = TRUE)
  ) %>% 
  mutate(desity = population/area)

create_mel_line <- function(new_city) {
  all_cities %>% 
    filter(city == "Melbourne") %>%
    mutate(city = new_city,
           city_graph = "Melbourne")
  
}


all_mel_cities <-map_df(city_names$location,create_mel_line)

all_cities %>% 
  mutate(city_graph = "other") %>% 
  bind_rows(all_mel_cities) %>% 
  filter(city!="Melbourne",
         dist_km_round<50) %>% 
  ggplot(aes(x = dist_km_round, 
             y = as.numeric(desity),
             colour = city_graph))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~city,scales = "free_y")

test <- 
  all_cities %>% filter(city %in% c("melbourne", "London"),
                       !is.na(population))

max_scale <- max(all_cities$desity) 


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
          

all_cities %>% 
  filter(dist_km_round<50) %>%
  group_by(dist_km_round) %>% 
  mutate(change_required = desity[city == "Tokyo"]/desity) %>% 
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
  labs(fill = "Change in population density required\nfor Melbourne to be as dense as Tokyo",
       title = "Suburban rail loop is being built on the outskirts of where our population should be rising if we want a 'Tokyo' style density",
       subtitle = "SRL stations in white")
