options(shiny.autoload.r = FALSE)
library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(glue)
library(qs)
library(data.table)
library(shinyWidgets)
library(plotly)
library(scales)
library(shinythemes)
library(DT)
library(ggflags)
library(tidyverse)

cities_data <-   qread("output/qs_files/shiny.qs") %>% 
  data.table::setDT() 

#Data for rankings

pop_cum <- qread("output/qs_files/pop_rankings.qs")
density_cum <- qread("output/qs_files/density_cum_rankings.qs")

cities_lookup <- cities_data %>% st_drop_geometry() %>% distinct(city_name,geoname_id)
cities <- cities_lookup$city_name
options(scipen = 50)

choices_list <- cities_data %>% 
  distinct(country,city_name)  %>%
  group_by(country) %>%
  summarise(city_name = list(sort(city_name)), 
            .groups = 'drop') %>%
  deframe()  # Converts to a named list


metrics <- qread("output/qs_files/names_of_colums.qs") %>%  
  filter(col_name %in% names(cities_data))


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

city_lat_lons <- qread("output/qs_files/city_lat_lons.qs")


source("R/functions/ggplot_theme.R")
source("R/input_modules.R")