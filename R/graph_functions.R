#This is the graph making portion - just an interum step currently until a better UI can be created

library(sf)
library(leaflet)
library(tidyverse)
library(curl)
library(ggmap)
library(ggiraph)
library(units)

print("at point 2 yay! ")

source("R/get_geo_files_from_web.R")
print("at point 3 yay! ")

city_locations_sf <- get_city_locations(cities_to_import=7)
circle_map <- map_df(city_locations_sf$rds_1km_circle_name,readRDS) %>% 
  mutate(density = density*1e6)
km_2_map <- map_df(city_locations_sf$rds_file_name,readRDS) %>% 
  mutate(density = population/(area/1e6))





leaflet_city_circle <- function(x,city_to_map,second_city) {
metric_map_two <- circle_map %>% 
  filter(city %in% c(city_to_map,second_city)) %>% 
  filter(dist_km_round<40) %>%
  mutate(metric = {{x}},
         ) %>% 
  filter(!is.na(population)) %>% 
  rename(geometry = x) %>% 
  st_as_sf()

metric_map <- metric_map_two %>% filter(city == city_to_map)

if(str_detect(deparse(substitute(x)),"density")) {
labels <- sprintf(
  "<strong>%skm from centre</strong><br/>%s people per square km",
  metric_map$dist_km_round, 
  round(metric_map$metric)
) %>% lapply(htmltools::HTML)

} else {
  labels <- sprintf(
    "<strong>%skm from centre</strong><br/> %s people",
    metric_map$dist_km_round, 
    round(metric_map$metric)
  ) %>% lapply(htmltools::HTML)

}

bins = round(seq(0,max(as.numeric(metric_map_two$metric)),length.out = 9))
pal <- colorBin("RdBu", 
                domain = metric_map$metric, 
                bins = bins,
                reverse = T)

hundred <- function(x){x*100}
leaflet_object<- 
leaflet(metric_map)%>% 
  addTiles() %>% 
  addPolygons(
  fillColor = ~pal(metric),
  weight = 1,
  opacity = 1,
  color = "white",
  fillOpacity = 0.7,
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

if(str_detect(deparse(substitute(x)),"Density")) {
     leaflet_object <- leaflet_object %>% 
  addLegend(pal = pal, values = ~density,
            opacity = 0.7, 
            position = "bottomright",
            labFormat = labelFormat(suffix = "%",
                                    transform = hundred),
            title = paste0("Density (people per square km)"))
} else{
  leaflet_object <- leaflet_object %>% 
  addLegend(pal = pal, values = ~density,
            opacity = 0.7, 
            position = "bottomright",
            title = paste0("Population"))
}


return(leaflet_object)

}

leaflet_city_comparer <- function(x,main_city,compared_city) {
  metric_map <- circle_map %>% 
    filter(dist_km_round<40) %>%
    group_by(dist_km_round) %>% 
    mutate(population_percent = (population[city == city] / population) * area[city == main_city]/area,
           population_change  = round((population[city == main_city] - population) * area[city == main_city]/area), 
           density_percent    = density[city == main_city] / density,
           density_change     = density[city == main_city] - density,
           metric = {{x}},
           #metric = density_change
    ) %>% 
    filter(city %in% c(compared_city)) %>% 
    filter(dist_km_round<40,
           !is.na(population)) %>% 
    rename(geometry = x) %>% 
    st_as_sf()
  
  if(str_detect(deparse(substitute(x)),"percent")) {
    labels <- sprintf(
      "<strong>%skm from centre</strong><br/>%s as much density in %s as %s",
      metric_map$dist_km_round, 
      paste0(round(100*metric_map$metric),"%"),
      main_city,
      compared_city
    ) %>% lapply(htmltools::HTML)
    
    step = (ceiling(as.numeric(max(metric_map$metric)))-1)/5
    
    bins <- c(0, .2,.4,.6,.8,1,1+step, 1+2*step,1+3*step,1+4*step,1+5*step)
    pal <- colorBin("RdBu", 
                    domain = metric_map$metric, 
                    bins = bins,
                    reverse = F)
    
  } else {
    labels <- sprintf(
      "<strong>%skm from centre</strong><br/> %s more people per square km in %s than %s",
      metric_map$dist_km_round, 
      round(metric_map$metric),
      main_city,
      compared_city
    ) %>% lapply(htmltools::HTML)
    
    step = (ceiling(as.numeric(max(abs(metric_map$metric)))))/5
    
    bins <- round(c(-5*step,-4*step,-3*step,-2*step,-1*step,0, step,2*step,3*step,4*step,5*step))
    pal <- colorBin("RdBu", 
                    domain = metric_map$metric, 
                    bins = bins,
                    reverse = F)
    
  }
  
  hundred <- function(x){x*100}
  leaflet_object<- 
    leaflet(metric_map)%>% 
    addTiles() %>% 
    addPolygons(
      fillColor = ~pal(metric),
      weight = 1,
      opacity = 1,
      color = "white",
      fillOpacity = 0.7,
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
  
  if(str_detect(deparse(substitute(x)),"percent")) {
    leaflet_object <- leaflet_object %>% 
      addLegend(pal = pal, values = ~density,
                opacity = 0.7, 
                position = "bottomright",
                labFormat = labelFormat(suffix = "%",
                                        transform = hundred),
                title = paste0("Difference in population density<br>between ",main_city," and ",compared_city))
  } else{
    leaflet_object <- leaflet_object %>% 
      addLegend(pal = pal, values = ~density,
                opacity = 0.7, 
                position = "bottomright",
                title = paste0("Difference in population density<br>between ",main_city," and ",compared_city))
  }
  
  
  return(leaflet_object)
  
}


km_2_map_creator <- function(city_to_map,second_city) {

km_2_binner <- km_2_map %>% filter(city %in% c(city_to_map,second_city))
bins <- round(seq(from = 0,
            to = max(km_2_binner$population),
            length.out = 9))
  
pal <- colorBin("YlOrRd", 
                domain = km_2_map$population, 
                bins = bins)

km_2_map %>% 
  filter(city == city_to_map,
         dist_km_round<30) %>%
  leaflet()%>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(population),
    weight = 0,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7) %>% 
  addLegend(pal = pal, values = ~population,
            opacity = 0.7, 
            position = "bottomright",
            title = "Population")
}

dist_line_graph <- function(cities) {

gg_point <- circle_map %>% 
  filter(city %in% cities) %>% 
  ggplot(aes(x = dist_km_round, 
             y = as.numeric(density), 
             color = city,
             fill = city)) +
  geom_smooth()+
  geom_point_interactive(aes(tooltip = paste0(city," at ",
                                              dist_km_round,
                                              "km: ", 
                                              prettyNum(round(as.numeric(density)),big.mark = ",")," people per square km"), 
                             data_id = density)) + 
  theme_minimal()+
  labs(title = "Density for different cities compared",
       x = "Distance from centre of city",
       y = "Population density (people per square km)",
       colour = "City",
       fill = "City")+
  scale_y_continuous(labels = scales::number_format(big.mark = ","))
  


girafe(ggobj = gg_point,
       options = list(
         opts_sizing(width = .7),
         opts_zoom(max = 5)))

}

