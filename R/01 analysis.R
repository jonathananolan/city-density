#This script creates a file for each populous city with 250m/2 square grids showing population density in each square

library(tidyverse)
library(ggmap) #Google API key is required to find the centre of each city. This is free for limited use (well above what we are doing here!)!
library(geosphere) #find distance between two points
library(raster) #import raster data
library(sf) # You also need install.packages("rgdal") for this to work, which isn't installed automatically when installing sf/raster
library(countrycode) #get country codes to find url to download pop densities
library(curl) # More reliable file download than R's default


#You need a google API key in order to get the lat/lon of cities
if(!has_google_key()){register_google(readline(prompt="Enter your google API key: "),
                                      write = TRUE) }

source("R/get_geo_files_from_web.R")

#Download shape file of water bodies from the net
water_bodies_global <- get_global_water_bodies()
#Download raster file of population density from the net, and use google to find the centre of the city
#You might want to set this to only import a couple of cities to save time to start with
city_locations_sf <- get_city_locations() %>% filter(city_name!="Brisbane")

create_city_map <- function(city,year = 2015,resolution = 250) {
  
  time = Sys.time()
  #city = "Melbourne";year = 2015;resolution = 250
  
  #city = "Melbourne" #- helpful to run for your first test of this function.
  city_sf <- city_locations_sf %>% filter(city_name == city) 
  city_lat_lon <- city_sf %>% st_coordinates()
  
  if(!file.exists(paste0("data/ghsl_rds/",year,"_",resolution,"_",city,".RDS"))) {
    
    print(paste0("Running for ",city))
    
    raster_raw <- grep(paste0("GPW4",year),grep(resolution,list.dirs("data/GHSL"), value = TRUE),value = TRUE) %>% 
      list.files(pattern = "*.tif$",full.names = TRUE) %>% 
      raster()
    
    custom_crs <- paste0("+proj=aeqd +lat_0=",city_lat_lon[2]," +lon_0=",city_lat_lon[1]," +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    
    #Create an 'extent' bounding box around the city's centre.
    #this is made to be roughly 100km - but doesn't need to be exact. 
    
    bb_around_point <- function(dist_from_cbd_number = 100){
      dist_from_cbd_number = dist_from_cbd_number*1000
      
      
      city_location <- st_geometry(st_sfc(st_point(cbind(city_lat_lon[1], city_lat_lon[2])),crs=custom_crs))  %>% 
        st_transform(custom_crs) 
      
      
      output <-  city_location%>% 
        st_buffer(dist_from_cbd_number+500) %>%
        st_transform(crs(raster_raw)) %>% 
        st_bbox()
      
      return(output)
    }

    
    bb<- bb_around_point()
    
    #Crop the raster to the city's bounding box
    #Then perform convert it to an SF object. 
    
    
    city_raster_map <- raster::crop(raster_raw,extent(bb)) 
    
    
    #Save city's raster object to RDS
    output_directory = "data/ghsl_raster/"
    if (!dir.exists(output_directory)){
      dir.create(output_directory) }
    raster::writeRaster(city_raster_map,paste0("data/ghsl_raster/",year,"_",resolution,"_",city,".tif"),overwrite = TRUE)
    
    
    city_sf_map<- city_raster_map%>%
      raster::rasterToPolygons() %>% 
      st_as_sf()
    

    
    #An error I don't quite understand came up when I use the new spherical SF model of the earth. Changing to the old way. 
    sf_use_s2(FALSE)
    
    bb_around_point_wbs <- function(dist_from_cbd_number = 100){
      dist_from_cbd_number = dist_from_cbd_number*1000
      
      
      city_location <- st_geometry(st_sfc(st_point(cbind(city_lat_lon[1], city_lat_lon[2])),crs=4326))  %>% 
        st_transform(custom_crs) 
      
      
      output <-  city_location%>% 
        st_buffer(dist_from_cbd_number+500) %>%
        st_transform(crs(water_bodies_global)) %>% 
        st_bbox()
      
      return(output)
    }
    
    water_bodies <- st_crop(water_bodies_global , 
                            bb_around_point_wbs(), sparse = FALSE) %>% 
      st_transform(crs(custom_crs))
    
    
    bb_around_point_final <- function(dist_from_cbd_number = 100){
      dist_from_cbd_number = dist_from_cbd_number*1000
      
      
      city_location <- st_geometry(st_sfc(st_point(cbind(city_lat_lon[1], city_lat_lon[2])),crs=4326))  %>% 
        st_transform(custom_crs) 
      
      
      output <-  city_location%>% 
        st_buffer(dist_from_cbd_number+500) %>%
        st_bbox() 
      return(output)
    }
    
    city_sf_without_water <-  st_difference(city_sf_map %>% 
                                              st_transform(custom_crs), st_union(water_bodies)) %>% 
      st_crop(bb_around_point_final(), sparse = FALSE)
    
    city_sf_without_water$area <- st_area(city_sf_without_water)
    
    #Find the centre of each km/2, so we can see how far from the city it is.
    #warning message here is worrying! 
    city_km_2_centroids <- st_coordinates(city_sf_without_water %>% st_transform(4326) %>% st_centroid())
    
    
    st_crs(city_sf) = 4326
    city_lat_lon_x_y <- city_sf  %>%  st_transform(custom_crs) %>% st_coordinates()
    

    #Find distance from each km/2 square into city centre
    city_sf_without_water <- city_sf_without_water %>% 
      rename_with( .fn = ~paste0("population"), .cols = contains("GHS_POP")) %>% 
      mutate(dist = distHaversine(cbind(city_km_2_centroids ),
                                  c(city_lat_lon[1],city_lat_lon[2] )),
             city = city,
             dist_km = dist/1000,
             dist_km_round = round(dist_km),
             area = as.numeric(area)) %>% 
      mutate(density = population/(area*1e-6)) 
    
    
    #Save city's sf object to RDS
    output_directory = "data/ghsl_rds/"
    if (!dir.exists(output_directory)){
      dir.create(output_directory) }
    saveRDS(city_sf_without_water,paste0("data/ghsl_rds/",year,"_",resolution,"_",city,".RDS"))
    
    city_by_1km_radii <- city_sf_without_water %>% 
      group_by(dist_km_round,city) %>%
      summarise(population = sum(population,na.rm = TRUE),
                area = sum(area, na.rm = TRUE)
      ) %>% 
      mutate(density = population/(area*1e-6))
    
    
    
    #Save city's sf object to RDS
    output_directory = "data/ghsl_radii_rds"
    if (!dir.exists(output_directory)){
      dir.create(output_directory) }
    
    saveRDS(city_by_1km_radii,paste0("data/ghsl_radii_rds/",year,"_",resolution,"_",city,".RDS"))
    
    
    #It's accurate as square km squares, but circles look nicer! Let's create circles instead....
    
    distances_from_cbd <- unique(city_by_1km_radii$dist_km_round)
    
    create_semi_circles <- function(dist_from_cbd_number){
      dist_from_cbd_number = dist_from_cbd_number*1000
      
      custom_crs <- paste0("+proj=aeqd +lat_0=",city_lat_lon[2]," +lon_0=",city_lat_lon[1]," +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      city_location <- st_geometry(st_sfc(st_point(cbind(city_lat_lon[1], city_lat_lon[2])),crs=4326))  %>% 
        st_transform(custom_crs) 
      
      
      big_circle <- city_location%>% 
        st_buffer(dist_from_cbd_number+500) %>%
        st_transform(crs(water_bodies))
      
      little_circle <- city_location%>% 
        st_buffer(dist_from_cbd_number-500) %>%
        st_transform(crs(water_bodies))
      
      output <- st_difference(big_circle,little_circle) %>% st_as_sf() %>% 
        mutate(dist_km_round = dist_from_cbd_number/1000) %>% 
        
        st_difference(st_union(water_bodies))
      return(output)
    }
    
    semi_circle_areas <- map_df(distances_from_cbd,create_semi_circles)
    
    city_by_1km_radii_circle <- city_by_1km_radii %>% st_drop_geometry() %>% left_join(semi_circle_areas)
    
    #Save city's sf object to RDS
    output_directory = "data/ghsl_radii_circle_rds"
    if (!dir.exists(output_directory)){
      dir.create(output_directory) }
    
    saveRDS(city_by_1km_radii_circle,paste0("data/ghsl_radii_circle_rds/",year,"_",resolution,"_",city,".RDS"))
    print(Sys.time() - time)
  }
}


walk(city_locations_sf$city_name,
     .f = create_city_map)

