
source("R/00renv.R")

full_city_list <- list.files("data/city_summaries/1km_without_water") %>% str_remove(".qs")
already_complete <- list.files("data/s3_uploads/","square") %>% str_remove_all("square_|.html")
already_complete_circle <- list.files("data/s3_uploads/","circle") %>% str_remove_all("circle_|.html")

cities_to_leaflet <- full_city_list[!(full_city_list %in% already_complete)]

leaflet_city_details <- get_city_locations() %>% 
  mutate(city_name = paste0(name,",<br>",country)) 

create_map_files <- function(city_id){
  
 # city_id = 2034714
  cities_name <- leaflet_city_details$city_name[leaflet_city_details$geoname_id == city_id]
  
  
  rings <- qread(paste0("data/city_summaries/ghsl_radii_circle_qs/",city_id,".qs")) %>% 
    mutate(pwd_with_water = pwd_with_water *1e6,
           pwd_cum_with_water = pwd_cum_with_water *1e6) 
  
  
  ring_creator <- function(col_to_run,pop_text,text_to_add){
  
  ring_pal <- viridis_palette_limiter(rings[[col_to_run]],2) 

  rings_map <- rings %>%
    st_transform("wgs84") %>% 
    leaflet() %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~ring_pal(rings[[col_to_run]]),
      color = "white",
      weight = .1,
      opacity = .7,
      fillOpacity = 0.4
      #popup = ~paste(metric_column(), ":", data$value)  # Correct context for .data
    ) %>%
    addLegend(pal = ring_pal, values = ~rings[[col_to_run]], opacity = 1,
              title = ~HTML(paste0(pop_text,"<br>",cities_name,".<br><span style='font-weight:normal;'>",text_to_add,"</span>")),
              position = "bottomright") 
  
  htmlwidgets::saveWidget(rings_map,paste0("data/s3_uploads/ring_",col_to_run,"_",city_id,".html"),selfcontained = T,libdir = 'library')
  
  }
  
  
  
  ring_creator("pwd_with_water",        "Density of rings around",   "Residents per square km,<br>population weighted.")
  ring_creator("density_with_water",    "Density of rings around",   "Residents per square km,<br>inclduing water.")
  ring_creator("density_without_water", "Density of rings around",   "Residents per square km,<br>excluding water.")
  ring_creator("population",            "Population of rings around","Residents in each ring.")
  ring_creator("population_cum",        "Population of rings around","Residents in each ring<br>or closer.")
  
  
  

  
  
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
              title = ~HTML(paste0("Population around<br>",cities_name,".<br><span style='font-weight:normal;'>1km squares.</span>")),
              position = "bottomright")

  
  htmlwidgets::saveWidget(square_map,paste0("data/s3_uploads/square_",city_id,".html"),libdir = 'library')
  
  
  
}



walk(cities_to_leaflet,create_map_files,.progress = T)



files_with_path <- list.files("data/s3_uploads","*.html",full.names = T) 
files <- list.files("data/s3_uploads","*.html") 


string <- paste0("*",paste0(cities_to_leaflet,collapse = "|"),"*")

files_with_path <- files_with_path[grepl(string, files_with_path)]

files <- files[grepl(string, files)]



#Upload all files

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




  