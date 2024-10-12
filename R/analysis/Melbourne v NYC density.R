library(tidyverse)
library(qs)
library(sf)
library(leaflet)
library(htmltools)
source("R/functions/ggplot_theme.R")
mel <- qread("data/city_summaries/detailed_without_water/2158177.qs") %>% mutate(city = "Melbourne")
nyc <- qread("data/city_summaries/detailed_without_water/5128581.qs")%>% mutate(city = "NYC")
london <- qread("data/city_summaries/detailed_without_water/2643743.qs")%>% mutate(city = "London")
paris <- qread("data/city_summaries/detailed_without_water/2988507.qs")%>% mutate(city = "Paris")



londn <- qread("data/s")
boroughs <- read_sf("data/NYC/nybb.shp") %>% 
  filter(BoroName == "Manhattan") %>% 
  st_transform(st_crs(nyc))
manhattan <- nyc %>% 
  st_filter(boroughs) %>% 
  filter(population_single_tile>0)

manhattan %>% 
  st_transform("wgs84") %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons()

mel_close <- mel %>% 
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
  arrange(dist_km) %>%
  filter(population_single_tile>0) %>% 
  filter(row_number()<= nrow(manhattan)) %>% 
  mutate(tile_no = row_number()) %>% 
  mutate(city = case_when(tile_no %in% c(34,48,53) ~ "Fitzroy North",
                          T ~ city))

test <- nyc %>%   
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
  arrange(desc(population_single_tile)) %>%
  filter(population_single_tile>0) %>% 
  filter(row_number()<=nrow(manhattan))

london_close <- london %>% 
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
  #arrange(desc(population_single_tile)) %>% 
  arrange(dist_km) %>%
  filter(population_single_tile>0) %>% 
  filter(row_number()<=nrow(manhattan))

paris_close <- paris %>% 
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
  #arrange(desc(population_single_tile)) %>% 
  arrange(dist_km) %>%
  filter(population_single_tile>0) %>% 
  filter(row_number()<=nrow(manhattan))

 map_data <- mel_close %>% 
   filter(city == "Fitzroy North" ) %>% 
  st_as_sf() %>% 
  st_transform("wgs84") 
 
 map_data %>%
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(fill = jn_colours$complementary[3],
              color = jn_colours$complementary[3],
              label = map_data$tile_no)




manhattan %>% 
  bind_rows(mel_close) %>% 
  ggplot(aes(x = population_single_tile,fill = city))+
  geom_histogram()



plot_data <- manhattan %>% 
  bind_rows(mel_close) %>% 
  st_drop_geometry() %>% 
  mutate(density = density_with_water_single_tile *1e6,
         density_without_water = density_without_water_single_tile*1e6) %>% 
  mutate(population = cut(population_single_tile,seq(0,200000,1000),include.lowest = TRUE,labels = seq(0,199000,1000))) %>%
  mutate(population = parse_number(as.character(population))+500) %>% 
  group_by(population) %>%
  arrange(desc(city)) %>% 
  filter(population_single_tile>0) %>% 
  mutate(height = row_number()) 

# Create the non-breaking spaces string
space_string <- paste0(rep('&nbsp;', 10), collapse = '')

# Create the formatted label with parts in bold and non-breaking spaces
formatted_label <- paste0(
  "<b>← Less dense areas</b>", 
  space_string, 
  "People in each square kilometer", 
  space_string, 
  "<b>More dense areas →</b>"
)


formatted_label =  paste0(
      "<span style='font-size:12pt'><b>← Less dense areas</b></span>",
      
      # Transparent dots as spacer
      "<span style='color:transparent;'>..............</span>",
      
      "People in each square kilometer",
      
      # Transparent dots as spacer
      "<span style='color:transparent;'>..............</span>",
      
      "<span style='font-size:12pt'><b>More dense areas →</b></span>"
    )
   

plot_data %>% 
  ggplot(aes(x = population, y = height,fill = city ))+
  geom_tile(colour = "white",width = 1000)+
  scale_x_continuous(
    breaks = seq(0, 200000, by = 10000),  # Set breaks every 10000
    labels = prettyNum(seq(0, 200000, by = 10000),big.mark = ","),
    limits = c(0, NA)  # Set the limit to start at 0
  ) +
  labs(       title = HTML(paste0("<span style='color:", jn_colours$complementary[1], ";'><strong>Melbourne</strong></span>"," is not as dense as ",
                                  "<span style='color:", jn_colours$complementary[2], ";'><strong>Manhattan</strong></span>")),
       subtitle = "Manhattan divided into 106 1km squares, compared to the same area closest to Melbourne.\nFitzroy North shown in green.",
       x = formatted_label,
       y = "Number of square km",
       caption = "Notes: Excludes areas where poulation is 0.\nMany km squares in Manhattan contain water, and so this graph underestimates just how dense the borough is.\nData from Global Human Settlement Layer (2020) and ABS (2022) for Australian data.\nSource: CityDensity.com",
       fill = element_blank())+
  theme_jn_caption()+
  theme(plot.title = element_markdown(),
        legend.position = "none",
        axis.title.x  = element_markdown())+
  scale_fill_manual(values = c(jn_colours$complementary[3],jn_colours$complementary[1],jn_colours$complementary[2]))

ggsave("data/NYC/1km_squares_waffle.png")
plot_data <- paris_close %>% 
  bind_rows(mel_close) %>% 
  group_by(city) %>% 
  filter(row_number()<=100) %>% 
  st_drop_geometry() %>% 
  mutate(density = density_with_water_single_tile *1e6,
         density_without_water = density_without_water_single_tile*1e6) %>% 
  mutate(population = cut(population_single_tile,seq(0,200000,1000),include.lowest = TRUE,labels = seq(0,199000,1000))) %>%
  mutate(population = parse_number(as.character(population))+500) %>% 
  group_by(population) %>%
  arrange(desc(city)) %>% 
  filter(population_single_tile>0) %>% 
  mutate(height = row_number()) 




plot_data %>% 
  ggplot(aes(x = population, y = height,fill = city ))+
  geom_tile(colour = "white",width = 1000)+
  scale_x_continuous(
    breaks = seq(0, 200000, by = 10000),  # Set breaks every 10000
    labels = prettyNum(seq(0, 200000, by = 10000),big.mark = ","),
    limits = c(0, NA)  # Set the limit to start at 0
  ) +
  labs(       title = HTML(paste0("<span style='color:", jn_colours$complementary[1], ";'><strong>Melbourne</strong></span>"," is not as dense as ",
                                  "<span style='color:", jn_colours$complementary[2], ";'><strong>Paris</strong></span>")),
              subtitle = "The closest 100 square km around Paris and Melbourne.",
              x = formatted_label,
              y = "Number of square km",
              caption = "Notes: Excludes areas where poulation is 0.\nData from Global Human Settlement Layer (2020) and ABS (2022) for Australian data.\nSource: CityDensity.com",
              fill = element_blank())+
  theme_jn_caption()+
  theme(plot.title = element_markdown(),
        legend.position = "none",
        axis.title.x  = element_markdown())


ggsave("data/NYC/1km_squares_waffle_paris.png",height = 5.8)







all_cities <- read_csv("data/s3_uploads/city-density.csv") 

all_cities %>% filter(geoname_id %in% c("2158177","6058560")) %>% 
  group_by(dist_km_round) %>% 
  arrange(city_name) %>% 
  summarise(across(is.numeric,~last(.x)/first(.x))) %>% 
  ggplot(aes(x = dist_km_round, y =density_cum_without_water))+
  geom_line(colour = "black")


library(scales)
library(ggtext)
library(htmltools)
 all_cities %>% filter(geoname_id %in% c("2158177","2643743")) %>% 
   filter(dist_km_round<30) %>% 
  mutate(city_name = str_replace(city_name," \\(","\n(")) %>% 
  mutate(city_name = fct_rev(city_name)) %>% 
  ggplot(
    aes(x = dist_km_round, 
        y = density_without_water, 
        color = city_name)
  ) +
  geom_line(linewidth = 1.5) +
  labs(x = "Distance from city centre (km)", 
       y = "Density",
       title = HTML(paste0("Density near <span style='color:", jn_colours$complementary[2], ";'><strong>London</strong></span>"," and ",
                                        "<span style='color:", jn_colours$complementary[1], ";'><strong>Melbourne</strong></span>")),
       subtitle =  "Excluding large bodies of water",
       colour = element_blank(),
       caption = "Source: CityDensity.com\nData from Global Human Settlement Layer (2020) and ABS (2022) for Australian data.") +
  theme_jn_caption(plot_type = "line")+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+
   theme(plot.title = element_markdown(),
         legend.position = "none")
 ggsave("data/NYC/London Melbourne.png")
 