#Find rank of cities by population n km from centre

library(tidyverse)
library(sf)

all_cities <- list.files(path = "data/ghsl_sf",pattern = "1km*",full.names = T)


all_files <- function(x){
  
  readRDS(x)
}

output <- map_df(all_cities,all_files)

smaller_cities <- read_csv("data/smaller_cities.csv")

city_sizes <- output %>% 
  st_drop_geometry() %>% 
  filter(!(city_ascii %in% smaller_cities$city_ascii)) %>% 
  filter(dist_km_round<30) %>% 
  group_by(city_ascii) %>% 
  summarise(population = sum(population)) %>% 
  arrange(desc(population)) %>%
  rownames_to_column()


output %>% filter(str_detect(city_ascii,"Melbourne|Rotterdam")) %>% 
  ggplot(aes(x = dist_km_round, y = population, colour = city_ascii))+
  geom_line()+
  theme_minimal()
