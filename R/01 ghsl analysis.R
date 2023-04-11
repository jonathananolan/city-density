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

source("R/get_city_lat_lon_from_web.R")
source("R/get_water_body_map.R")

# Download this file.... a bit lazy not to do it for you sorry but it's so big it can be a hassle
#https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2022A/GHS_POP_E2015_GLOBE_R2022A_54009_1000/V1-0/GHS_POP_E2015_GLOBE_R2022A_54009_1000_V1_0.zip
global_raster_pop <- raster("data/GHS_POP_E2015_GLOBE_R2022A_54009_1000_V1_0.tif")

#Make sure everything is projected into Mollweide

#Download shape file of water bodies from the net
water_bodies_global_moll <- get_global_water_bodies()  %>% 
  st_transform(st_crs(global_raster_pop)) %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  filter(area > 30000) %>% #Bit of a random decision - but very small bodies of water area annoying
  st_set_precision(500000) %>%
  st_make_valid() %>% 
  st_simplify()


cities_list <- get_google_city_locations() %>%
  st_transform(st_crs(global_raster_pop))



create_city_map <- function(input_city) {
  
  
  tryCatch({

  print(input_city)
  
 
  time = Sys.time()

 # input_city = "United States Washington" #- helpful to run for your first test of this function.
  city_sf <- cities_list %>% filter(city_ascii == input_city) 
  city_lat_lon <- city_sf %>% st_coordinates()
  
  if(!file.exists(paste0("data/ghsl_sf/",input_city,".RDS"))) {
    
    print(paste0("Running for ",input_city))
    
  

    #Create an 'extent' bounding box around the city's centre.
    #this is made to be roughly 100km - but doesn't need to be exact. 
    
    bb_around_point <- function(dist_from_cbd_num = 100){
      
   output <-  city_sf %>% 
        st_buffer((dist_from_cbd_num*1000)+500) %>%
        st_bbox() %>% 
        extent()
   
   return(output)
    }
    city_raster_map <- raster::crop(global_raster_pop,bb_around_point()) 
    
    city_sf_map<- city_raster_map%>%
      raster::rasterToPolygons() %>% 
      st_as_sf() %>%
      st_set_precision(1000000) %>%
      st_make_valid() %>% 
      st_simplify()
      
    sf_use_s2(T)
    
    city_water_sf_map <- sf::st_crop(water_bodies_global_moll ,bb_around_point()) 
    
    if (nrow(city_water_sf_map) >0) {
    city_sf_without_water <-  st_difference(city_sf_map , st_union(city_water_sf_map))
    
      }else{
    city_sf_without_water <- city_sf_map
            }
    
    city_sf_without_water$area <- st_area(city_sf_without_water)
    
    #Find the centre of each km/2, so we can see how far from the city it is.
    #warning message here is worrying! 
    city_centroids <- city_sf_without_water  %>%  st_centroid() 
    
    city_distances <- tibble(dist = as.numeric(st_distance(city_centroids,city_sf)[,1]))
                                                               
    
    #Find distance from each km/2 square into city centre
    city_sf_without_water <- city_sf_without_water %>% 
      rename_with( .fn = ~paste0("population"), .cols = contains("GHS_POP")) %>% 
      bind_cols(city_distances) %>% 
      mutate(city_ascii = input_city,
             dist_km = dist/1000,
             dist_km_round = round(dist_km),
             area = as.numeric(area)) %>% 
      mutate(density = population/(area*1e-6)) 
    
    
    #Save city's sf object to RDS
    output_directory = "data/ghsl_sf/"
    if (!dir.exists(output_directory)){
      dir.create(output_directory) }
    saveRDS(city_sf_without_water,paste0("data/ghsl_sf/",input_city,".RDS"))
    
    city_cum_density <- city_sf_without_water %>% 
      group_by(city_ascii) %>% 
      mutate(density = population/area,
             pwd_cum = weighted.mean(density[dist_km_round<=dist_km_round], population[dist_km_round<=dist_km_round])) %>% 
      group_by(dist_km_round,city_ascii) %>%
      st_drop_geometry() %>% 
      summarise(pwd_cum = first(pwd_cum))
    
    city_by_1km_radii <- city_sf_without_water %>% 
      group_by(dist_km_round,city_ascii) %>%
      summarise(population = sum(population,na.rm = TRUE),
                area = sum(area, na.rm = TRUE),
                pwd = weighted.mean(population/(area*1e-6), population)
      ) %>% 
      group_by(city_ascii) %>%
      mutate(density  = population/(area*1e-6), 
             pop_cum  = cumsum(population[dist_km_round<=dist_km_round]),
             area_cum = cumsum(area[dist_km_round<=dist_km_round])) %>%
      left_join(city_cum_density)
    
    
    
    
    #Save city's sf object to RDS
    output_directory = "data/ghsl_sf"
    if (!dir.exists(output_directory)){
      dir.create(output_directory) }
    
    saveRDS(city_by_1km_radii,paste0("data/ghsl_sf/1km_",input_city,".RDS"))
    
    
    # #It's accurate as square km squares, but circles look nicer! Let's create circles instead....
    # 
    # distances_from_cbd <- unique(city_by_1km_radii$dist_km_round)
    # 
    # create_semi_circles <- function(dist_from_cbd_num){
    #   dist_from_cbd_number_1000 = dist_from_cbd_num*1000
    # 
    #   
    #   big_circle <- city_sf%>% 
    #     st_buffer(dist_from_cbd_number_1000+500) 
    #   
    #   little_circle <- city_sf%>% 
    #     st_buffer(dist_from_cbd_number_1000-500) 
    #   
    #   output <- st_difference(big_circle,little_circle) %>% st_as_sf() %>% 
    #     mutate(dist_km_round = dist_from_cbd_number_1000/1000) %>% 
    #     st_difference(st_union(city_water_sf_map)) %>% 
    #     select(dist_km_round)
    #   return(output)
    # }
    # 
    # semi_circle_areas <- map_df(distances_from_cbd,create_semi_circles)
    # 
    # city_by_1km_radii_circle <- city_by_1km_radii %>% st_drop_geometry() %>% left_join(semi_circle_areas)
    # 
    # #Save city's sf object to RDS
    # output_directory = "data/ghsl_radii_circle_rds"
    # if (!dir.exists(output_directory)){
    #   dir.create(output_directory) }
    # 
    # saveRDS(city_by_1km_radii_circle,paste0("data/ghsl_radii_circle_rds/",input_city,".RDS"))
    print(Sys.time() - time)
  }

  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


walk(.x = cities_list$city_ascii,
     .f = create_city_map)


