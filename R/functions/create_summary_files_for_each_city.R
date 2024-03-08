
create_summary_files_for_each_city <- function(input_city,distance=101) {
  
  if (!dir.exists("data/city_summaries/")){  dir.create("data/city_summaries/") }
  
  tryCatch({
    
    print(input_city)
    
    
    time = Sys.time()
    
   # input_city = "Australia Melbourne"
    #input_city = "United States New York" #- helpful to run for your first test of this function.
    city_sf <- cities_list %>% filter(geoname_id == input_city) 
    city_lat_lon <- city_sf %>% st_transform("wgs84") %>% st_coordinates()
    
    if(!file.exists(paste0("data/city_summaries/ghsl_radii_circle_rds/",input_city,".RDS"))) {
      
      print(paste0("Running for ",city_sf$name,", ",city_sf$country))
      
      
      
      #Create an 'extent' bounding box around the city's centre.
      #this is made to be roughly 100km - but doesn't need to be exact. 
      
      bb_around_point <- function(dist_from_cbd_num = distance+45){
        
        output <-  city_sf %>% 
          st_buffer((dist_from_cbd_num*1000)+500) %>%
          st_bbox() %>% 
          extent()
        
        return(output)
      }
      
      if (str_detect(city_sf$country,'Australia')){
        print("aus special!")
        bb_aus <- function(dist_from_cbd_num = distance+45){
          
          output <-  city_sf %>% 
            st_transform(st_crs(aus_raster_pop)) %>% 
            st_buffer((dist_from_cbd_num*1000)+500) %>%
            st_bbox() %>% 
            extent()
          
          return(output)
        }
        
        city_raster_map <- raster::crop(aus_raster_pop,bb_aus()) 
        city_raster_map[is.na(city_raster_map)] <- 0
        
        city_sf_map<- city_raster_map%>%
          raster::rasterToPolygons() %>% 
          st_as_sf() %>%
          st_transform(st_crs(city_sf)) %>%
          st_set_precision(1000000) %>%
          st_make_valid() %>% 
          st_simplify() 
        
        }else{
          city_raster_map <- raster::crop(global_raster_pop,bb_around_point()) 
      
          city_raster_map[is.na(city_raster_map)] <- 0
          
          #test map
          #leaflet() %>% addTiles() %>% addRasterImage(city_raster_map)
          
          city_sf_map<- city_raster_map%>%
            raster::rasterToPolygons() %>% 
            st_as_sf() %>%
            st_set_precision(1000000) %>%
            st_make_valid() %>% 
            st_simplify() 
          
        }
      
    # city_sf_map %>% st_transform("wgs84") %>% leaflet() %>% addTiles() %>% addPolygons()
      
      suppressWarnings({
      # Apply the Haversine function to each row in city_centroids
      # Assuming 'city_sf' has the coordinates for NYC in 'lon' and 'lat'
      city_distances <- city_sf_map %>%
        st_transform("wgs84") %>% 
        st_centroid()  %>%
        mutate(dist = geosphere::distHaversine(st_coordinates(geometry), city_lat_lon)) %>% 
        st_drop_geometry() %>% 
        dplyr::select(dist)
      
      })
     #Find distance from each km/2 square into city centre
      city_sf_map <- city_sf_map %>% 
       rename_with( .fn = ~paste0("population"), .cols = matches("GHS_POP|Band_1")) %>% 
      rename(population_single_tile = population) %>% 
       bind_cols(city_distances) %>% 
       mutate(geoname_id = input_city,
              dist_km = dist/1000,
              dist_km_round = floor(dist_km),
            #  dist_km_round = if_else(dist_km_round == 0,1,dist_km_round),
              id = row_number()) %>% 
       filter(dist_km_round<101) %>% 
        mutate(area_with_water_single_tile = as.numeric(st_area(.)),
               density_with_water_single_tile = population_single_tile/area_with_water_single_tile) %>% 
        group_by(dist_km_round) %>% 
        mutate(tiles = 1) %>% #So even if we filter some we still get the total water
       ungroup() %>%
      mutate(id = row_number())
       #city_sf_map %>% st_transform("wgs84") %>% leaflet() %>% addTiles() %>% addPolygons()
      
      if(!between(min(city_sf_map$area_with_water_single_tile)/1e6,.99,1.01)) {print("area calc wrong - check projections")}

      
      #Save city's sf object to RDS
      output_directory = "data/city_summaries/detailed_with_water/"
      if (!dir.exists(output_directory)){
        dir.create(output_directory) }
      saveRDS(city_sf_map,paste0(output_directory,input_city,".RDS"))
      

    # Now figure out how much area we lose because of water....

      
      sf_use_s2(T)
      suppressWarnings({
      city_water_sf_map_raw <- sf::st_crop(water_bodies_global_moll ,bb_around_point())
      })
      
      city_water_sf_map <-  city_water_sf_map_raw %>% 
        st_combine() %>% 
        st_make_valid() %>% 
        st_union() %>% 
        st_buffer(1) #There was a weird thing with reprojection, making the water bodies slightly bigger stops that error. 
      
      if (nrow(city_water_sf_map_raw) >0) {
        
        suppressWarnings({
        city_sf_without_water <-  st_difference(city_sf_map , 
                                                city_water_sf_map) 
        })
        city_sf_empty_tiles <- city_sf_map %>% 
                               st_drop_geometry() %>%  
                               anti_join(city_sf_without_water, by = join_by(population_single_tile, 
                                                                             dist, 
                                                                             geoname_id, 
                                                                             dist_km, 
                                                                             dist_km_round, 
                                                                             id, 
                                                                             area_with_water_single_tile,
                                                                             density_with_water_single_tile, 
                                                                             tiles))
        
        if(nrow(city_sf_empty_tiles) > 0){
          city_sf_without_water <-  city_sf_without_water %>% 
          bind_rows(city_sf_empty_tiles) }
        
        city_sf_map_all <- city_sf_without_water %>% 
          mutate(area_without_water_single_tile = as.numeric(st_area(.)),
                 density_without_water_single_tile = replace_na(population_single_tile/area_without_water_single_tile,0)
                 ) %>%
          ungroup() 

                         
      }else{
        city_sf_map_all <- city_sf_map %>% 
          mutate(area_without_water_single_tile = area_with_water_single_tile,
                 density_without_water_single_tile = population_single_tile/area_without_water_single_tile
          ) %>% 
          ungroup() 
      }
      
      city_without_water_1km_map <- city_sf_map_all %>% 
        group_by(dist_km_round,
                 geoname_id) %>% 
        summarise(    geometry = list(geometry[area_without_water_single_tile > 2500]), # Collect geometries conditionally
                  area_with_water    = sum(area_with_water_single_tile),
                  area_without_water = sum(area_without_water_single_tile),
                  tiles_km = sum(tiles),
                  population = sum(population_single_tile),
                  pwd_with_water     = stats::weighted.mean(density_with_water_single_tile, population_single_tile, na.rm = TRUE),
                  .groups = "drop") %>% 
        mutate(density_without_water = population/(area_without_water * 1e-6),
               density_with_water    = population/(area_with_water* 1e-6) ) %>%
        ungroup()
      
     # city_without_water_1km_map %>% st_drop_geometry() %>% ggplot(aes(x = dist_km_round, y = tiles))+geom_line(stat = "identity")
      
       #Save city's sf object to RDS
      output_directory = "data/city_summaries/detailed_without_water/"
      if (!dir.exists(output_directory)){
        dir.create(output_directory) }
      saveRDS(city_sf_map_all,paste0(output_directory,input_city,".RDS"))
      
      cumulative_calculator <- function(dist_number) {
        city_sf_map_all %>%
        ungroup() %>% 
        st_drop_geometry() %>% 
        filter(dist_km_round <= dist_number) %>% 
        summarise(population_cum = sum(population_single_tile),
                  area_cum_with_water       = sum(area_with_water_single_tile),
                  area_cum_without_water    = sum(area_without_water_single_tile),
                  pwd_cum_with_water        = weighted.mean(density_with_water_single_tile,
                                                            population_single_tile),
                  tiles_cum = sum(tiles),
                  .groups = "drop") %>% 
        mutate(dist_km_round = dist_number,
               density_cum_with_water    = population_cum / (area_cum_with_water* 1e-6),
               density_cum_without_water = population_cum / (area_cum_without_water* 1e-6)
               )
        }
      
      city_cum_densities <- map_df(unique(city_sf_map_all$dist_km_round),cumulative_calculator)
        
      city_by_1km_radii <- city_without_water_1km_map %>% 
        left_join(city_cum_densities,by = join_by(dist_km_round)) %>% 
        filter(dist_km_round <distance) %>% 
        ungroup() 
      
       #Save city's sf object to RDS
      output_directory = "data/city_summaries/1km_without_water/"
      if (!dir.exists(output_directory)){
        dir.create(output_directory) }
      
      saveRDS(city_by_1km_radii,paste0(output_directory,input_city,".RDS"))
      
      
      #It's accurate as square km squares, but circles look nicer! Let's create circles instead....

      distances_from_cbd <- unique(city_by_1km_radii$dist_km_round)
      

      create_geodesic_circle <- function(lon, lat, radius, num_points = 100) {
        # Earth's radius in meters
        R <- 6378137
        # Convert radius to radians
        rad <- radius / R
        
        # Generate points
        angles <- seq(0, 2 * pi, length.out = num_points)
        circle_points <- lapply(angles, function(angle) {
          lat_rad <- lat * pi / 180
          lon_rad <- lon * pi / 180
          lat_point <- asin(sin(lat_rad) * cos(rad) + cos(lat_rad) * sin(rad) * cos(angle))
          lon_point <- lon_rad + atan2(sin(angle) * sin(rad) * cos(lat_rad), cos(rad) - sin(lat_rad) * sin(lat_point))
          c(lon = lon_point * 180 / pi, lat = lat_point * 180 / pi)
        })
        
        # Explicitly add the first point to the end to ensure closure
        circle_points <- c(circle_points, list(circle_points[[1]]))
        
        # Convert the list of points to a matrix and then to a polygon
        points_matrix <- matrix(unlist(circle_points), ncol = 2, byrow = TRUE)
        circle_polygon <- st_polygon(list(points_matrix))
        
        # Create an sf object with the polygon and set CRS to WGS 84
        circle_points_sf <- st_sfc(circle_polygon)
        circle_sf <- st_sf(geometry = st_set_crs(circle_points_sf, 4326))
        
        return(circle_sf)
      }
      
       create_semi_circles <- function(dist_from_cbd_num){
       # print(dist_from_cbd_num)
         
         #dist_from_cbd_num = 3
         
        dist_from_cbd_number_1000 = dist_from_cbd_num*1000

        big_circle <- create_geodesic_circle(city_lat_lon[1],city_lat_lon[2], dist_from_cbd_number_1000+1000) %>% st_make_valid()
        
        little_circle <- create_geodesic_circle(city_lat_lon[1],city_lat_lon[2], dist_from_cbd_number_1000) %>% st_make_valid()
      

        if(dist_from_cbd_num>0){
        output <- st_difference(big_circle,little_circle) %>% st_as_sf() 
        } else{output <- big_circle}
        output <- output%>%
          mutate(dist_km_round = dist_from_cbd_num) %>%
          st_transform(st_crs(city_water_sf_map)) %>% 
          st_difference(city_water_sf_map) %>%
          select(dist_km_round) %>%
          st_transform("wgs84")
        
        return(output)
      }
      suppressWarnings({
      semi_circle_areas <- map_df(distances_from_cbd,create_semi_circles)
      })
      
      #Check if it roughly matches up....
      
      #semi_circle_areas %>% filter(dist_km_round == 1) %>% leaflet() %>% addTiles() %>% addPolygons() %>%
      #  addPolygons(data = city_by_1km_radii %>% filter(dist_km_round == 1) %>% st_transform("wgs84"))
      
      city_by_1km_radii_circle <- semi_circle_areas %>% left_join(city_by_1km_radii %>% st_drop_geometry(),by = join_by(dist_km_round))

      #Save city's sf object to RDS
      output_directory = "data/city_summaries/ghsl_radii_circle_rds"
      if (!dir.exists(output_directory)){
        dir.create(output_directory) }

      saveRDS(city_by_1km_radii_circle,paste0("data/city_summaries/ghsl_radii_circle_rds/",input_city,".RDS"))
      print(Sys.time() - time)
    }
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

