#Find rank of cities by population n km from centre

library(tidyverse)
library(sf)


all_cities <- list.files(path = "data/ghsl_sf",pattern = "1km*",full.names = T) %>% 
  map_df(readRDS) %>% 
  select(city_ascii,dist_km_round,population,area,pop_cum) %>% 
  st_drop_geometry()

smaller_cities <- read_csv("data/smaller_cities.csv")

stat_maker <- function(km_measure) {

#headline stat
city_sizes_headline <- all_cities %>% 
  #filter(!(city_ascii %in% smaller_cities$city_ascii)) %>% 
  filter(dist_km_round<km_measure) %>% 
  group_by(city_ascii) %>% 
  summarise(population = sum(population)) %>% 
  distinct(population, .keep_all =  T) %>% 
  arrange(desc(population)) %>%
  rownames_to_column()%>% 
  select(-population) %>% 
  mutate(type = "Headline")


#Exclude small cities
city_sizes_excluding_twin <- all_cities %>% 
  filter(!(city_ascii %in% smaller_cities$city_ascii)) %>% 
  filter(dist_km_round<km_measure) %>% 
  group_by(city_ascii) %>% 
  summarise(population = sum(population)) %>% 
  distinct(population, .keep_all =  T) %>% 
  arrange(desc(population)) %>%
  rownames_to_column() %>% 
  select(-population) %>% 
  mutate(type = "Excluding small 'sister' cities near big ones")


#Look at population density instead

city_sizes_pop_density <- all_cities %>% 
  #filter(!(city_ascii %in% smaller_cities$city_ascii)) %>% 
  filter(dist_km_round<km_measure) %>% 
  group_by(city_ascii) %>% 
  summarise(population_density = sum(population)/sum(area)) %>% 
  distinct(population_density, .keep_all =  T) %>% 
  arrange(desc(population_density)) %>%
  rownames_to_column()%>% 
  select(-population_density) %>% 
  mutate(type = "By density (excluding water bodies)")

city_sizes_pop_density_excluding_twins <- all_cities %>% 
  filter(!(city_ascii %in% smaller_cities$city_ascii)) %>% 
  filter(dist_km_round<km_measure) %>% 
  group_by(city_ascii) %>% 
  summarise(population_density = sum(population)/sum(area)) %>% 
  distinct(population_density, .keep_all =  T) %>% 
  arrange(desc(population_density)) %>%
  rownames_to_column()%>% 
  select(-population_density) %>% 
  mutate(type = "By density and excluding sister cities")

output <- bind_rows(city_sizes_pop_density_excluding_twins,
          city_sizes_pop_density,
          city_sizes_excluding_twin,
          city_sizes_headline) %>% 
  mutate(distance_from_centre = km_measure) %>% 
  rename(rank = rowname)
print(km_measure)
return(output)
}

ranks <- map_df(seq(1,100),stat_maker)

ranks %>% 
  filter(city_ascii== "Australia Melbourne") %>% 
  filter(distance_from_centre < 75) %>% 
  mutate(rank = parse_number(rank),
         type = fct_relevel(type,"Headline",
                            "By density (excluding water bodies)",
                            "Excluding small 'sister' cities near big ones")) %>% 
  ggplot(aes(x = distance_from_centre, y = rank, colour = type))+
  geom_line(stat = "identity",
            size = 2)+
  theme_minimal()+
  scale_y_reverse(limits = c(600,1),
                  breaks = c(1,200,400,600)) +
  labs(title = "The area around Melbourne is not very populous by global standards",
       colour = "Different ways to measure population",
       x = "Radius of area used to rank cities (km)",
       y = "Melbourne's global population rank",
       caption = "Sister cities are defined as those where another city less than 50km away has more people within it's 10km centre.")
  
  ranks %>% 
    mutate(rank = parse_number(rank)) %>% 
    filter(city_ascii== "Australia Melbourne") %>% 
    filter(distance_from_centre < 75,
           type == "Excluding small 'sister' cities near big ones") %>% 
    ggplot(aes(x = distance_from_centre, y = rank))+
    geom_line(stat = "identity",
              size = 2)+
    theme_minimal()+
    scale_y_reverse(limits = c(600,1),
                    breaks = c(1,200,400,600)) +
    labs(title = "The area around Melbourne is not very populous by global standards",
         x = "Radius of area used to rank cities (km)",
         y = "Melbourne's global population rank",
         caption = "Source: https://github.com/jonathananolan/population-density")+
    geom_text(data = data.frame(x = 4.41247074650499, y = 469.571378362391, label = "The area 5km from Melbourne's centre\nis ranked 428th in the world for population"),
              mapping = aes(x = x, y = y, label = label),
              hjust = 0L, inherit.aes = FALSE)
  
  
  
  city_rank = 195
  
  ranks %>% 
    rename(dist_km_round = distance_from_centre) %>% 
    filter(type == "Excluding small 'sister' cities near big ones") %>% 
    left_join(all_cities) %>% 
    group_by(dist_km_round) %>% 
    summarise(difference = pop_cum[rank == 192] - pop_cum[city_ascii == "Australia Melbourne"]) %>% view()
  
  
  