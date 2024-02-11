
create_summary_files_for_each_city <- function(input_city,distance=100) {
  
  
  tryCatch({
    
    print(input_city)
    
    
    time = Sys.time()
    
    
    #input_city = "Japan Tokyo" #- helpful to run for your first test of this function.
    city_sf <- cities_list %>% filter(city_ascii == input_city) 
    city_lat_lon <- city_sf %>% st_transform("wgs84") %>% st_coordinates()
    
    if(!file.exists(paste0("data/city_summaries/detailed/",input_city,".RDS"))) {
      
      print(paste0("Running for ",input_city))
      
      
      
      #Create an 'extent' bounding box around the city's centre.
      #this is made to be roughly 100km - but doesn't need to be exact. 
      
      bb_around_point <- function(dist_from_cbd_num = distance+30){
        
        output <-  city_sf %>% 
          st_buffer((dist_from_cbd_num*1000)+500) %>%
          st_bbox() %>% 
          extent()
        
        return(output)
      }
      city_raster_map <- raster::crop(global_raster_pop,bb_around_point()) 
      
      #test map
      #leaflet() %>% addTiles() %>% addRasterImage(city_raster_map)
      
      city_sf_map<- city_raster_map%>%
        raster::rasterToPolygons() %>% 
        st_as_sf() %>%
        st_set_precision(1000000) %>%
        st_make_valid() %>% 
        st_simplify() %>% 
        mutate(area_with_water = as.numeric(st_area(.)))
      
      if(min(city_sf_map$area_with_water)!=10^6) {print("area calc wrong - check projections")}
      
     # city_sf_map %>% st_transform("wgs84") %>% leaflet() %>% addTiles() %>% addPolygons()
      
               
               
      
      
    
      # Apply the Haversine function to each row in city_centroids
      # Assuming 'city_sf' has the coordinates for NYC in 'lon' and 'lat'
      city_distances <- city_sf_map %>%
        st_centroid()  %>%
        st_transform("wgs84") %>% 
        mutate(dist = geosphere::distHaversine(st_coordinates(geometry), city_lat_lon)) %>% 
        st_drop_geometry() %>% 
        dplyr::select(dist)
      
      
     #Find distance from each km/2 square into city centre
      city_sf_map <- city_sf_map %>% 
       rename_with( .fn = ~paste0("population"), .cols = contains("GHS_POP")) %>% 
       bind_cols(city_distances) %>% 
       mutate(city_ascii = input_city,
              dist_km = dist/1000,
              dist_km_round = round(dist_km),
              id = row_number()) %>% 
       filter(dist_km_round<101)
      

    # Now figure out how much area we lose because of water....


      
      sf_use_s2(T)
      
      city_water_sf_map <- sf::st_crop(water_bodies_global_moll ,bb_around_point(500)) 
      
      if (nrow(city_water_sf_map) >0) {
        city_sf_without_water <-  st_difference(city_sf_map , st_union(city_water_sf_map)) %>% 
          mutate(area_without_water = as.numeric(st_area(.))) %>% 
          dplyr::select(id,area_without_water,dist_km_round)
        
        city_without_water_1km_map <- city_sf_without_water %>% 
          group_by(dist_km_round) %>% 
          summarise(geometry = st_union(geometry)) # Combine geometries into one)
        
        city_sf_everything <- city_sf_map %>%
          left_join(city_sf_without_water %>% 
                     st_drop_geometry(), by = c("id","dist_km_round")) %>% 
          mutate(area_without_water = replace_na(area_without_water,0))  
                         
      }else{
        city_sf_without_water <- city_sf_map %>% 
          mutate(area_without_water = area_with_water)
      }
      

      
      city_with_densities <- city_sf_everything %>% 
        mutate(density_without_water = population/(area_without_water * 1e-6),
               density_with_water    = population/(area_with_water* 1e-6) ) %>%
        st_drop_geometry()
      
      #Save city's sf object to RDS
      output_directory = "data/city_summaries/detailed"
      if (!dir.exists(output_directory)){
        dir.create(output_directory) }
      saveRDS(city_sf_without_water %>% left_join(city_sf_map %>% st_drop_geometry()),paste0("data/city_summaries/detailed/",input_city,".RDS"))
      
      cumulative_calculator <- function(dist_number) {
      city_with_densities %>%
        filter(dist_km_round <= dist_number) %>% 
        summarise(population_cum = sum(population),
                  density_cum_with_water    = sum(population) / sum(area_with_water* 1e-6),
                  density_cum_without_water = sum(population) / sum(area_without_water* 1e-6),
                  area_cum_with_water       = sum(area_with_water),
                  area_cum_without_water    = sum(area_without_water),
                  pwd_cum_with_water        = weighted.mean(density_with_water,population)) %>% 
        mutate(dist_km_round = dist_number)
        }
      
      city_cum_densities <- map_df(unique(city_with_densities$dist_km_round),cumulative_calculator)
        
      city_by_1km_radii <- city_with_densities %>% 
        group_by(dist_km_round,
                 city_ascii) %>%
        summarise(pwd_with_water     = stats::weighted.mean(density_with_water, population, na.rm = TRUE),
                  population         = sum(population,         na.rm = TRUE),
                  area_with_water    = sum(area_with_water,    na.rm = TRUE),
                  area_without_water = sum(area_without_water, na.rm = TRUE)
        ) %>% 
        left_join(city_cum_densities, by = "dist_km_round") %>% 
        filter(dist_km_round <101) %>% 
        ungroup() 
      
      #city_by_1km_radii %>% ggplot(aes(x = dist_km_round, y = area_without_water))+geom_line(stat = "identity")
      #city_by_1km_radii_with_water_geo %>% st_transform("wgs84") %>% leaflet() %>% addTiles() %>% addPolygons()

      city_by_1km_radii_with_water_geo <- city_without_water_1km_map %>% left_join(city_by_1km_radii)

      #Save city's sf object to RDS
      output_directory = "data/city_summaries/1km_without_water"
      if (!dir.exists(output_directory)){
        dir.create(output_directory) }
      
      saveRDS(city_by_1km_radii_with_water_geo,paste0("data/city_summaries/1km_without_water/",input_city,".RDS"))
      
      
      #It's accurate as square km squares, but circles look nicer! Let's create circles instead....

      distances_from_cbd <- unique(city_by_1km_radii$dist_km_round)

      create_semi_circles <- function(dist_from_cbd_num){
        dist_from_cbd_number_1000 = dist_from_cbd_num*1000


        big_circle <- city_sf%>%
          st_buffer(dist_from_cbd_number_1000+500)

        little_circle <- city_sf%>%
          st_buffer(dist_from_cbd_number_1000-500)

        output <- st_difference(big_circle,little_circle) %>% st_as_sf() %>%
          mutate(dist_km_round = dist_from_cbd_number_1000/1000) %>%
          st_difference(st_union(city_water_sf_map)) %>%
          select(dist_km_round)
        return(output)
      }

      semi_circle_areas <- map_df(distances_from_cbd,create_semi_circles)

      city_by_1km_radii_circle <- semi_circle_areas %>% left_join(city_by_1km_radii %>% st_drop_geometry())

      #Save city's sf object to RDS
      output_directory = "data/city_summaries/ghsl_radii_circle_rds"
      if (!dir.exists(output_directory)){
        dir.create(output_directory) }

      saveRDS(city_by_1km_radii_circle,paste0("data/ghsl_radii_circle_rds/",input_city,".RDS"))
      print(Sys.time() - time)
    }
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
