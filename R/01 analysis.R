#This script creates a file for each populous city with 1km/2 square grids showing population density in each square

library(tidyverse)
library(ggmap) #Google API key is required to find the centre of each city. This is free for limited use (well above what we are doing here!)!
library(geosphere) #find distance between two points
library(raster) #import raster data
library(sf) # You also need install.packages("rgdal") for this to work, which isn't installed automatically when installing sf/raster
library(countrycode) #get country codes to find url to download pop densities
library(curl) # More reliable file download than R's default


#You need a google API key in order to get the lat/lon of cities
if(!has_google_key()){register_google(readline(prompt="Enter your google API key: "),write = TRUE) }

source("R/get_geo_files_from_web.R")

#Download shape file of water bodies from the net
water_bodies_global <- get_global_water_bodies()
#Download raster file of population density from the net, and use google to find the centre of the city
#You might want to set this to only import a couple of cities to save time to start with
city_locations_sf <- get_city_locations(cities_to_import=2) 

create_city_map <- function(city) {
  
  #city = "Melbourne" #- helpful to run for your first test of this function.
  city_sf <- city_locations_sf %>% filter(city_name == city) 
  city_lat_lon <- city_sf %>% st_coordinates()
  
  if(!file.exists(city_sf$rds_file_name)) {


#Get file path to import raster file of country for given city
  print(paste0("Running for ",city))
  tif_filepath = paste0("data/population_densities/",city_sf$tif_filename)
  raster <- raster(tif_filepath)


#Create an 'extent' bounding box around the city's centre.
#this is made to be roughly 100km - but doesn't need to be exact. 
  
ext_around_point <- function(x,xsize,ysize){
  bbox <- st_bbox(x)
  bbox <- bbox +c(xsize/2,ysize/2,-xsize/2,-ysize/2)
  return(extent(bbox))
}

ext_bb <- ext_around_point(city_sf, -2,-1)


#Crop the raster to the city's bounding box
#Then perform convert it to an SF object. 
city_sf_map <- raster::crop(raster,ext_bb) %>% 
  raster::rasterToPolygons() %>% 
  st_as_sf()

#Do the same thing for the water bodies, but use a slightly bigger bounding box
#ST crops wants a "bounding box" not an 'extent' for this SF ivhect so the function is slightly 
#different.  
bb_around_point <- function(x,xsize,ysize){
  bbox <- st_bbox(x)
  bbox <- bbox +c(xsize/2,ysize/2,-xsize/2,-ysize/2)
  return((bbox))
}


bb_around <- bb_around_point(city_sf, -2.1,-1.1)

#An error I don't quite understand came up when I use the new spherical SF model of the earth. Changing to the old way. 
sf_use_s2(FALSE)

water_bodies <- st_crop(water_bodies_global , 
                        bb_around, sparse = FALSE) %>% 
  st_transform(crs(city_sf_map))

city_sf_without_water <-  st_difference(city_sf_map, st_union(water_bodies))
city_sf_without_water$area <- st_area(city_sf_without_water)

#Find the centre of each km/2, so we can see how far from the city it is.
#warning message here is worrying! 
city_km_2_centroids <- st_coordinates(city_sf_without_water %>%   st_centroid())

#Find distance from each km/2 square into city centre
city_sf_without_water <- city_sf_without_water %>% 
  mutate(dist = distHaversine(cbind(city_km_2_centroids ),
                              c(city_lat_lon[1],city_lat_lon[2] )),
         city = city,
         dist_km = dist/1000,
         dist_km_round = round(dist_km)) %>% 
  rename_with( .fn = ~paste0("population"), .cols = contains("pd_2020")) 


#Save city's sf object to RDS
output_directory = "data/city_density_rds"
if (!dir.exists(output_directory)){
  dir.create(output_directory) }
saveRDS(city_sf_without_water,city_sf$rds_file_name)
}
}

walk(city_locations_sf$city_name,
     .f = create_city_map)




