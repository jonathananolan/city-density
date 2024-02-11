#This script creates a file for each populous city with km square grids showing population density in each square

library(tidyverse)
library(ggmap) #Google API key is required to find the centre of each city. This is free for limited use
library(geosphere) #find distance between two points
library(raster) #import raster data
library(sf) # You also need install.packages("rgdal") for this to work, which isn't installed automatically when installing sf/raster
library(countrycode) #get country codes to find url to download pop densities
library(curl) # More reliable file download than R's default

#You need a google API key in order to get the lat/lon of cities
if(!has_google_key()){register_google(readline(prompt="Enter your google API key: "),
                                      write = TRUE) }


source("R/functions/get_ghsl_files.R")
global_raster_pop <- get_ghsl_files()

source("R/functions/get_city_lat_lon_from_web.R")
cities_list <- get_google_city_locations() %>%
               st_transform(st_crs(global_raster_pop))%>% 
              distinct(city_ascii,.keep_all = T)

source("R/functions/get_water_body_map.R")
water_bodies_global_moll <- get_global_water_bodies(crs_transform = st_crs(global_raster_pop))  

source("R/functions/create_summary_files_for_each_city.R")

walk(.x = cities_list$city_ascii,
     .f = create_summary_files_for_each_city)

