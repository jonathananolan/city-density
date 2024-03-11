
source("R/00renv.R")

leaflet_city_list <- list.files("data/city_summaries/1km_without_water") %>% str_remove(".qs")

leaflet_city_details <- get_city_locations() %>% 
  mutate(city_name = paste0(name,",<br>",country))

create_map_files <- function(city_id){
  
  cities_name <- leaflet_city_details$city_name[cities_details$geoname_id == city_id]
  
  
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

walk(unique(leaflet_city_list),create_map_files)





#Upload all files

library(aws.s3)
recursive_upload_to_s3 <- function(base_dir,file_path) {
  # List all files in the base directory
  file_path <-files[2]
  # Create a relative path from the base directory to maintain the structure
  relative_path <- sub(paste0("^", gsub("\\\\", "/", base_dir)), "", gsub("\\\\", "/", file_path))
  relative_path <- ifelse(substr(relative_path, 1, 1) == "/", substr(relative_path, 2, nchar(relative_path)), relative_path)
  relative_path <- paste0("leaflet_maps/",relative_path)
  # Define the S3 object key with an optional prefix
  s3_object_key <- file.path(relative_path)
  
  # Determine the MIME type based on the file extension
  
  # Determine MIME type based on file extension
  mime_types <- c(
    html = "text/html",
    js = "application/javascript",
    css = "text/css",
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
    ico = "image/x-icon"
  )
  
  file_ext <- tools::file_ext(file_path)
  print(file_ext)
  content_type <- mime_types[[file_ext]]
  
  # If the file extension is not found in the list, use 'application/octet-stream' as default
  if (is.null(content_type)) {
    content_type <- 'application/octet-stream'
  }
  print(content_type)
  
  # Use put_object to upload the file with the appropriate content type
  aws.s3::put_object(
    file = file_name,
    object = s3_object_key,
    bucket = "city-density",
    headers = c("Content-Type" = content_type),
    verbose = T)
}


base_dir = "data/s3_uploads"

files = list.files(base_dir,full.names = T,recursive = T)

walk2(base_dir,files,recursive_upload_to_s3)

recursive_upload_to_s3(base_dir = "data/s3_uploads", bucket = "city-density")

test <- aws.s3::get_bucket_df("city-density")


aws.s3::put_folder("data/s3_uploads",bucket = "city-density",multipart = T,show_progress = T,verbose = T)  






  