
source("R/00renv.R")

full_city_list <- list.files("data/city_summaries/1km_without_water") %>% str_remove(".qs")
already_complete <- list.files("data/s3_uploads/","square") %>% str_remove_all("square_|.html")
already_complete_circle <- list.files("data/s3_uploads/","circle") %>% str_remove_all("circle_|.html")

cities_to_leaflet <- full_city_list[!(full_city_list %in% already_complete)]

leaflet_city_details <- get_city_locations() %>% 
  mutate(city_name = paste0(name,",<br>",country)) 

create_map_files <- function(city_id){
  
  #city_id = 2034714
  cities_name <- leaflet_city_details$city_name[leaflet_city_details$geoname_id == city_id]
  
  
  rings <- qread(paste0("data/city_summaries/ghsl_radii_circle_qs/",city_id,".qs"))
  circle_pal <- viridis_palette_limiter(rings$pwd_with_water,2)

  rings_map <- rings %>%
    st_transform("wgs84") %>% 
    leaflet() %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~circle_pal(pwd_with_water),
      color = "white",
      weight = .1,
      opacity = .7,
      fillOpacity = 0.4
      #popup = ~paste(metric_column(), ":", data$value)  # Correct context for .data
    ) %>%
    addLegend(pal = circle_pal, values = ~pwd_with_water, opacity = 1,
              title = ~paste0("Density of rings around<br>",cities_name,"<br>(population weighted)."),
              position = "bottomright") 
  
  
  # Save the map
  
  htmlwidgets::saveWidget(rings_map,paste0("data/s3_uploads/circle_",city_id,".html"),selfcontained = T,libdir = 'library')
  
  squares <- qread(paste0("data/city_summaries/detailed_with_water/",city_id,".qs"))
  
  square_pal <- viridis_palette_limiter(squares$population_single_tile,10)
  
  
  square_map <-  squares %>%
    filter(dist_km_round<80) %>% 
    st_transform("wgs84") %>% 
    leaflet() %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~square_pal(population_single_tile),
      color = "white",
      weight = .1,
      opacity = .1,
      fillOpacity = 0.4
    ) %>%
    addLegend(pal = square_pal, values = ~population_single_tile, opacity = 1,
              title = ~paste0("Population<br>per 1km square around<br>",cities_name),
              position = "bottomright") 
  
  
  htmlwidgets::saveWidget(square_map,paste0("data/s3_uploads/square_",city_id,".html"),libdir = 'library')
  
  
  
}


plan(multisession, 
     workers = 4)



walk(cities_to_leaflet,create_map_files,.progress = T)



files_with_path <- list.files("data/s3_uploads","*.html",full.names = T) 
files <- list.files("data/s3_uploads","*.html") 

#Upload all files

library(aws.s3)
recursive_upload_to_s3 <- function(file,file_with_path) {

  # Create a relative path from the base directory to maintain the structure
  # Define the S3 object key with an optional prefix
  s3_object_key <- paste0("leaflet_maps/",file)
  
  # Use put_object to upload the file with the appropriate content type
  aws.s3::put_object(
    file = file_with_path,
    object = s3_object_key,
    bucket = "city-density",
    headers = c("Content-Type" = "text/html"),
    verbose = T)
}


walk2(files,files_with_path,recursive_upload_to_s3)

#The 'library' folder I've added to the s3 manually. 




  