#Find rank of cities by population n km from centre

library(tidyverse)
library(sf)


all_cities_raw <- list.files(path = "data/ghsl_sf",pattern = "1km*",full.names = T) %>% 
  map_df(readRDS) %>% 
  select(city_ascii,dist_km_round,population,area,pop_cum)


melbourne_port <- st_read("data/port of melbourne.shp") 

mel_without_port_geom <- all_cities_raw %>%
  filter(city_ascii== "Australia Melbourne") %>% 
  st_difference(melbourne_port) %>% 
  mutate(area = as.numeric(st_area(geometry))) 

mel_without_port <- mel_without_port_geom %>% 
  st_drop_geometry() %>% 
  mutate(city_ascii = "Australia Melbourne (without port)")


melbourne_port_parks <- st_read("data/port_and_parks.shp") %>% st_simplify()

mel_without_port_parks_geom <- all_cities_raw %>%
  filter(city_ascii== "Australia Melbourne") %>% 
  st_difference(melbourne_port_parks) %>% 
  mutate(area = as.numeric(st_area(geometry))) 

mel_without_port_parks <- mel_without_port_parks_geom %>% 
  st_drop_geometry() %>% 
  mutate(city_ascii = "Australia Melbourne (without port and many parks)")


## Calculate plan melbourne increases in population
council_areas_regions <- tribble(~lga,~region,
                                 "Melbourne",   "inner",
                                 "Port Phillip",   "inner",
                                 "Yarra",   "inner",
                                 "Stonnington",   "inner south east",
                                 "Bayside (Vic.)",   "inner south east",
                                 "Boroondara",   "inner south east",
                                 "Glen Eira",   "inner south east",
                                 "Melton","western",
                                 "Brimbank","western",
                                 "Hobsons Bay","western",
                                 "Wyndham","western",
                                 "Moonee Valley","western",
                                 "Maribyrnong","western",
                                 "Banyule","northern",
                                 "Whittlesea","northern",
                                 "Nillumbik","northern",
                                 "Hume","northern",
                                 "Moreland","northern",
                                 "Darebin","northern",
                                 "Manningham","eastern",
                                 "Whitehorse","eastern",
                                 "Knox","eastern",
                                 "Yarra Ranges","eastern",
                                 "Maroondah","eastern",
                                 "Monash","eastern",
                                 "Kingston (Vic.)","southern",
                                 "Frankston","southern",
                                 "Cardinia","southern",
                                 "Casey","southern",
                                 "Greater Dandenong","southern",
                                 "Mornington Peninsula","southern") 

population_density_lga <- readxl::read_excel("data/32180DS0002_2001-21.xlsx", sheet = 2, skip = 6) %>% 
  select(lga = ...2, 
         population = `2015`) %>% 
  mutate(population = parse_number(population))

dwelling_estimates <- 
  tribble(~region,~dwelling_change,~people_per_dwelling,
          "inner",230000, 2.1,
          "western" ,365000 ,2.4,
          "northern" ,340000 ,2.7,
          "inner south east", 125000 ,2.4,
          "eastern", 190000, 2.7,
          "southern", 300000,2.7) %>% 
  mutate(population_increase = dwelling_change*people_per_dwelling)

council_areas <- council_areas_regions %>% 
  left_join(population_density_lga) %>% 
  left_join(dwelling_estimates) %>% 
  group_by(region) %>% 
  mutate(share_of_lga_in_region = population/sum(population)) %>% 
  mutate(increase = share_of_lga_in_region * population_increase)

library(absmapsdata)

council_areas_map <- lga2021 %>% 
  right_join(council_areas, by = c("lga_name_2021" = "lga")) %>% 
  st_transform(st_crs(mel_maps))



mel_maps <- all_cities_raw %>%
  filter(city_ascii== "Australia Melbourne")

mel_additional_pop_by_1km <- sf::st_intersection(mel_maps, council_areas_map) %>% 
  group_by(lga_name_2021) %>% 
  mutate(area_of_intersect = as.numeric(st_area(geometry)),
         share_of_area_in_km = area_of_intersect /sum(area_of_intersect),
         pop_increase = increase*share_of_area_in_km) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  group_by(dist_km_round) %>% 
  summarise(pop_increase = sum(pop_increase))


plan_melbourne <- all_cities_raw %>% st_drop_geometry() %>% 
  filter(city_ascii== "Australia Melbourne") %>% 
  left_join(mel_additional_pop_by_1km) %>% 
  mutate(population = population + pop_increase) %>% 
  mutate(pop_cum = cumsum(population)) %>% 
  mutate(city_ascii = "Australia Melbourne plan Melbourne")




all_cities <- all_cities_raw %>% st_drop_geometry() %>% 
  bind_rows(mel_without_port) %>% 
  bind_rows(mel_without_port_parks) %>% 
  bind_rows(plan_melbourne)

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

ranks %>% write_csv("ranks.csv")

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
         caption = "Source: https://github.com/jonathananolan/population-density") +
    geom_text(data = data.frame(x = 4.41247074650499, y = 469.571378362391, label = "The area 5km from Melbourne's centre\nis ranked 428th in the world for population"),
              mapping = aes(x = x, y = y, label = label),
              hjust = 0L, inherit.aes = FALSE)
  


  ranks %>% 
    filter(str_detect(city_ascii,"Australia")) %>% 
    filter(distance_from_centre < 75,
           type %in% c("By density and excluding sister cities",
                       "Excluding small 'sister' cities near big ones")) %>% 
    mutate(type = case_when(type == "By density and excluding sister cities" ~ "Density (Excludes water where people can't live)",
                            T ~ "Population")) %>% 
    mutate(rank = parse_number(rank)) %>% 
    mutate(city_ascii = str_remove(city_ascii,"Australia ")) %>% 
    ggplot(aes(x = distance_from_centre, y = rank, colour = city_ascii))+
    geom_line(stat = "identity",
              size = 1)+
    theme_minimal()+
    scale_colour_manual(values = pals::brewer.dark2(7))+
    scale_y_reverse(limits = c(700,1),
                    breaks = c(1,200,400,600)) +
    labs(title =   "Australian cities are not very dense by global standards",
         colour =  "Australian cities",
         x =       "Radius of area used to rank cities (km)",
         y =       "Australian city's global population rank",
         caption = "Population measured in 'density' for the land within a given distance of centre, so excluding bodies of water.")+
      facet_wrap(type~.)
  
  
  ranks %>% 
    filter(str_detect(city_ascii,"Melbourne|Los Angeles|New York|London|Berlin"),
           !str_detect(city_ascii,"port")) %>% 
    filter(distance_from_centre < 75,
           type %in% c("By density and excluding sister cities")) %>% 
    mutate(type = case_when(type == "By density and excluding sister cities" ~ "Density (Excludes water where people can't live)",
                            T ~ "Population")) %>% 
    mutate(rank = parse_number(rank)) %>% 
    mutate(city_ascii = str_remove(city_ascii,"Australia ")) %>% 
    ggplot(aes(x = distance_from_centre, y = rank, colour = city_ascii))+
    geom_line(stat = "identity",
              size = 1)+
    theme_minimal()+
    scale_colour_manual(values = pals::brewer.dark2(8))+
    scale_y_reverse(limits = c(700,1),
                    breaks = c(1,200,400,600)) +
    labs(title =   "Plan Melbourne won't fix the city's low density in our inner ring suburbs",
         colour =  "Australian cities",
         x =       "Radius of area used to rank cities (km)",
         y =       "Australian city's global population rank",
         caption = "Population measured in 'density' for the land within a given distance of centre, so excluding bodies of water.")+
    facet_wrap(type~.)
  
  
  city_rank = 150
  
  ranks %>% 
    rename(dist_km_round = distance_from_centre) %>% 
    filter(type == "Excluding small 'sister' cities near big ones",) %>% 
    left_join(all_cities) %>% 
    group_by(dist_km_round) %>% 
    summarise(difference = pop_cum[rank == city_rank]/pop_cum[city_ascii == "Australia Melbourne"],
              city = city_ascii[rank == city_rank]) %>% view()
  
  
  ranks %>% 
    filter(type == "Excluding small 'sister' cities near big ones",) %>% 
    filter(distance_from_centre ==30) %>% 
    mutate(rank = parse_number(rank)) %>% 
    view()
  
  library(ggmap)
  
mel_map <-   mel_without_port_geom %>% 
    filter(dist_km_round<7) %>% 
    summarise(area = sum(area))
  
  bbox_for_data <- st_bbox(mel_map %>% st_transform("wgs84")) 
  bbox_for_data_ggmap <- make_bbox(lon = c(bbox_for_data[[1]],bbox_for_data[[3]]), 
                              lat = c(bbox_for_data[[2]],bbox_for_data[[4]]))
  
  nc_map <- get_map(location = bbox_for_data_ggmap,maptype = "toner-lite",scale = 5)
  
  
 
  geom_for_sf <- mel_without_port_geom %>% 
    filter(dist_km_round<6) %>% 
    st_transform("wgs84")
 
    ggmap(nc_map) +
    geom_sf(data = geom_for_sf ,
            aes(fill= as.factor(dist_km_round)),
            inherit.aes = FALSE,
            alpha = .5)+
    labs(fill = "Distance from the CBD")+
    ggthemes::theme_map()
  
  all_cities %>% 
    filter(city_ascii == "Australia Melbourne",
           dist_km_round<6) %>% 
    summarise(area = sum(area))
  
  melbourne_port %>% 
    summarise(area = st_area(geometry))
    
  
  
  

