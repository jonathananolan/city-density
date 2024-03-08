#This script creates a file for each populous city with km square grids showing population density in each square

source("R/00renv.R")
#You need a google API key in order to get the lat/lon of cities
if(!has_google_key()){register_google(readline(prompt="Enter your google API key: "),
                                      write = TRUE) }


source("R/functions/get_ghsl_files.R")
global_raster_pop <- get_ghsl_files()


#I found that the GHSL is out of date for Australia. 
#They've projected forward temporary poppulation growth from the 2011 census to 2020 for greenfield suburbs in australia's major cities 
#in a way that makes them seem like they're now more dense than Australian suburbs. 
#To fix this, I'm subbing in data from Australia's statistical agency. 

aus_raster_pop <- raster("data/Australian_Population_Grid_2022_in_GEOTIFF_format(1)/apg22r_1_0_0.tif")


source("R/functions/get_city_lat_lon_from_web.R")
cities_list <- get_city_locations() %>%
               st_transform(st_crs(global_raster_pop))

water_bodies_global_moll <- read_sf("input_data//water_bodies/water_mol_simplified.shp")

source("R/functions/create_summary_files_for_each_city.R")

create_summary_files_for_each_city(2147714) 
create_summary_files_for_each_city(2147714) 

library(furrr)
plan(multisession, workers = 4)


future_walk(.x = cities_list$geoname_id,
            .f = create_summary_files_for_each_city,
            .progress = T)

walk(.x = cities_list$city_ascii,
            .f = create_summary_files_for_each_city,
            .progress = T)


  test <- read_rds("data/city_summaries/detailed_without_water/Japan Tokyo.RDS")
test <- read_rds("data/city_summaries/1km_without_water/Japan Tokyo.RDS")

test %>% rename(dcww = density_cum_with_water,
                dcwow = density_cum_without_water,
                dww = density_with_water,
                dwow = density_without_water,
                aww = area_with_water,
                awow = area_without_water,
                acww = area_cum_with_water,
                acwow = area_cum_with_water
                ) %>%  write_sf("test.shp")

test%>%  write_sf("test.shp")

