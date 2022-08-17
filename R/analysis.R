library(tidyverse)
library(ggmap) 
library(geosphere)
library(rasterVis)
library(raster)
library(stars)
library(osmdata)
library(sf)
library(leaflet)

source("R/get_geo_files_from_web.R")

#Download shape file of water bodies from the net
water_bodies_global <- get_global_water_bodies()
#Download raster file of population density from the net
#You might want to set this to only import a couple of cities to save time. 
city_locations_sf <- get_city_locations(cities_to_import = 30) 

create_city_map <- function(city) {
  city = city_locations_sf$city_name[30]

#Get file path to import raster file of country for
  print(paste0("Running for ",city))
  tif_file = city_locations_sf %>% filter(city_name == city) %>% pull(tif_filename)
  tif_filepath = paste0("data/population_densities/",tif_file)
file.exists(tif_filepath)
raster <- raster(tif_filepath)

circle <- filter(city_name == city) %>% 
      st_buffer(city_locations_sf, 50000)  

ext_around_point <- function(x,xsize,ysize){
  bbox <- st_bbox(x)
  bbox <- bbox +c(xsize/2,ysize/2,-xsize/2,-ysize/2)
  return(extent(bbox))
}


square <- ext_around_point(city_locations_sf %>% filter(city_name == city), -2,-1)


city_sf_map <- raster::crop(raster,square) %>% 
  raster::rasterToPolygons() %>% 
  st_as_sf()

bb_around_point <- function(x,xsize,ysize){
  bbox <- st_bbox(x)
  bbox <- bbox +c(xsize/2,ysize/2,-xsize/2,-ysize/2)
  return((bbox))
}


bb_around <- bb_around_point(city_locations_sf %>% filter(city_name == city), -2.1,-1.1)


sf_use_s2(FALSE)
water_bodies <- st_crop(water_bodies_global %>% 
                          sf::st_buffer(dist = 0), 
                        bb_around, sparse = FALSE)




water_bodies_crs <- water_bodies %>% 
  st_transform(crs(city_sf_map)) %>% 
  sf::st_buffer(dist = 0)


poly_city <-  st_difference(city_sf_map, st_union(water_bodies_crs))

poly_city$area <- st_area(poly_city)

lat_lon <- city_locations_sf %>% filter(city_name == city)

poly_city <- poly_city %>% 
  mutate(dist = distHaversine(cbind(st_coordinates(poly_city %>%   st_centroid() )),
                              c(lat_lon$lon,lat_lon$lat)),
         city = city,
         dist_km = dist/1000,
         dist_km_round = round(dist_km)) 

saveRDS(poly_city,paste0("data/poly_for_",city,".RDS"))




}

walk(city_locations_sf$city_name,
     .f = create_city_map)




