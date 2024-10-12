library(tidyverse)
library(qs)
library(data.table)
library(dtplyr)
library(sf)
cities <- read.csv("data/city_locations_cleaned.csv") %>% 
  select(geoname_id,name,country)

folder <-"data/city_summaries/detailed_with_water/"
all_qs <- list.files(folder,include.dirs = T,full.names = T)

reader <- function(x) {
  
 qread(x) %>%  filter(population_single_tile > 1500)

}


all_places <- map_df(all_qs,reader, .progress = T)
print("yay")
dense_places <- all_places %>% 
  slice_max(density_with_water_single_tile, n = 200000) %>% 
  left_join(cities)

dense_places %>% 
  filter(population_single_tile>40000)%>% 
  group_by(name,country) %>% st_drop_geometry() %>% summarise(n=n()) %>% view()

places <- c(#"Seoul","New York City","Singapore",
            "Barcelona",#"Hong Kong",
            #"London",
            "Paris")

#dense_places 
plot_data <- dense_places %>% filter(name %in% places) %>% 
  filter(population_single_tile>20000) %>% 
  group_by(name) %>% 
  filter(row_number()<50) %>% 
  st_drop_geometry() %>% 
  mutate(population = cut(population_single_tile,seq(0,200000,1000),include.lowest = TRUE,labels = seq(0,199000,1000))) %>%
  mutate(population = parse_number(as.character(population))+500) %>% 
  group_by(population) %>%
  arrange(desc(name)) %>% 
  filter(population_single_tile>0) %>% 
  arrange((name)) %>% 
  mutate(height = row_number()) 

source("R/functions/ggplot_theme.R")
plot_data %>% 
  ungroup() %>% 
  ggplot(aes(x = population, y = height,fill = name )) +
  geom_tile(colour = "white",width = 1000) +
  scale_x_continuous(
    breaks = seq(0, 200000, by = 10000),  # Set breaks every 10000
    labels = prettyNum(seq(0, 200000, by = 10000),big.mark = ",")
  ) +
  labs(       title = HTML(paste0("<span style='color:", jn_colours$complementary[1], ";'><strong>Barcelona</strong></span>"," is not as dense as ",
                                  "<span style='color:", jn_colours$complementary[2], ";'><strong>Hong Kong</strong></span>",", ",
                                  "<span style='color:", jn_colours$complementary[3], ";'><strong>NYC</strong></span>",", ",
                                  "<span style='color:", jn_colours$complementary[4], ";'><strong>Seoul</strong></span>", ", or ",
                                  "<span style='color:", jn_colours$complementary[5], ";'><strong>Singapore</strong></span>
                                  ")),
              subtitle = "50 most dense 1km squares in a selection of cities ",
              x = formatted_label,
              y = "Number of square km",
              caption = "Notes: Data from Global Human Settlement Layer (2020)\nSource: CityDensity.com",
              fill = element_blank())+
  theme_jn_caption()+
  theme(plot.title = element_markdown(),
        legend.position = "none",
        axis.title.x  = element_markdown())+
  scale_fill_manual(values = c(jn_colours$complementary[1],jn_colours$complementary[2],jn_colours$complementary[3],jn_colours$complementary[4],jn_colours$complementary[5]))



dense_places %>% 
  filter(name %in% places) %>% 
  filter(population_single_tile > 46810.29) %>% nrow()
