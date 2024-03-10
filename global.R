options(shiny.autoload.r = FALSE)
library(shiny)
library(ggplot2)
library(tidyverse)
library(sf)
library(leaflet)
library(glue)
library(qs)
library(data.table)
library(shinyWidgets)
library(plotly)
library(scales)
library(shinythemes)

cities_data <-   qread("output/qs_files/shiny.qs") %>% data.table::setDT() 

cities_lookup <- cities_data %>% st_drop_geometry() %>% distinct(city_name,geoname_id)

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

cities <- cities_lookup$city_name
options(scipen = 50)

choices_list <- cities_data %>% 
  distinct(country,city_name)  %>%
  group_by(country) %>%
  summarise(city_name = list(sort(city_name)), 
            .groups = 'drop') %>%
  deframe()  # Converts to a named list

print(str(cities_data))

metrics <- tribble(~col_name,~metric_type,~water,~cumulative,
                   "area_with_water",          "Area",                       "","",
                   "area_without_water",       "Area",                       "excluding large bodies of water","",
                   "population",               "Population",                 "","",
                   "pwd_with_water",           "Density",                    "population weighted","",
                   "density_without_water",    "Density",                    "excluding large bodies of water","",
                   "density_with_water" ,      "Density",                    "","",
                   "population_cum",           "Population",                 "","cumulative",
                   "area_cum_without_water",   "Area",                       "excluding large bodies of water", "cumulative",
                   "pwd_cum_with_water" ,      "Density",                    "population weighted","cumulative",
                   "density_cum_with_water",   "Density",                    "","cumulative",
                   "density_cum_without_water","Density",                    "excluding large bodies of water","cumulative")%>%
  mutate(description = pmap_chr(list(metric_type, water, cumulative), function(...) {
    combined_values <- c(...)
    combined_values <- combined_values[combined_values != ""] # Remove empty values
    paste(combined_values, collapse = ", ")   # Concatenate with comma
  })) %>% 
  mutate(subtitle = pmap_chr(list(water, cumulative), function(...) {
    combined_values <- c(...)
    combined_values <- combined_values[combined_values != ""] # Remove empty values
    paste(combined_values, collapse = ", ")   # Concatenate with comma
  })) %>% 
  mutate(subtitle = stringr::str_to_sentence(subtitle)) %>% 
  mutate(units = case_when(metric_type == "Density"~"Residents per square kilometer",
                           metric_type == "Area"~"Square kilometers",
                           metric_type == "Population"~"Residents")) %>% 
  filter(col_name %in% names(cities_data)) %>% 
  mutate(title = if_else(metric_type == "Population","Population living in ","Density of"))

source("R/functions/ggplot_theme.R")
source("R/input_modules.R")