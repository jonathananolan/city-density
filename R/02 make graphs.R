#This is the graph making portion - just an interum step currently until a better UI can be created

library(sf)
library(leaflet)
library(tidyverse)
library(curl)
library(ggmap)
library(ggiraph)
library(units)


sf_use_s2(FALSE)

#Create a graph that shows each city compared to Melbourne
create_mel_line <- function(new_city) {
  circle_map %>% 
    filter(city == "Melbourne") %>%
    mutate(city = new_city,
           city_graph = "Melbourne")
  
}
all_mel_cities <-map_df(city_locations_sf$city_name,create_mel_line)

circle_map %>% 
  mutate(city_graph = "other") %>% 
  bind_rows(all_mel_cities) %>% 
  filter(city!="Melbourne",
         dist_km_round<40) %>% 
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
          



leaflet_city_comparor <- function(x,compared_city,main_city) {
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
                reverse = T)

} else {
  labels <- sprintf(
    "<strong>%skm from centre</strong><br/> %s more people in %s as %s",
    metric_map$dist_km_round, 
    round(metric_map$metric),
    main_city,
    compared_city
  ) %>% lapply(htmltools::HTML)
  
  step = (ceiling(as.numeric(max(abs(metric_map$metric)))))/5
  
  bins <- c(-5*step,-4*step,-3*step,-2*step,-1*step,0, step,2*step,3*step,4*step,5*step)
  pal <- colorBin("RdBu", 
                  domain = metric_map$metric, 
                  bins = bins,
                  reverse = T)
  
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
            title = paste0("Difference in population density<br>between ",city_one," and ",city_two))
} else{
  leaflet_object <- leaflet_object %>% 
  addLegend(pal = pal, values = ~density,
            opacity = 0.7, 
            position = "bottomright",
            title = paste0("Difference in population density<br>between ",city_one," and ",city_two))
}


return(leaflet_object)

}


km_2_map_creator <- function(city) {

htmlwidgets::saveWidget(leaflet_object, file="output/leaflet_map.html")

bins <- seq(from = 0,
            to = max(km_2_map$population),
            length.out = 15)
  
pal <- colorBin("YlOrRd", 
                domain = metric_map$metric, 
                bins = bins)


km_2_map %>% 
  filter(city == city,
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

