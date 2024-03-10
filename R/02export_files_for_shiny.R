library(tidyverse)
library(sf)
library(leaflet)
library(aws.s3)
library(htmlwidgets)
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-1")

#Sys.setenv(AWS_ACCESS_KEY_ID = rstudioapi::askForPassword())
#Sys.setenv(AWS_SECRET_ACCESS_KEY = rstudioapi::askForPassword())

upload_object <- function(file_location){
  
  put_object(file = file_location, 
             bucket = "city-density",multipart = T,show_progress = T,
             headers = list("Content-Type" = "text/html"),verbose = T)
}

viridis_palette_limiter <- function(data,outlier_number){  # Sort data to find the cutoff points
  sorted_values <- sort(data)
  min_cutoff    <- min(sorted_values)
  high_cutoff   <- sorted_values[length(sorted_values) - outlier_number]  # 10th highest value
  highest_value <- max(sorted_values)
  first_distance <- high_cutoff-min_cutoff  
  second_distance <- highest_value - high_cutoff
  #The first distance is going to have about 50 colours... so what is the second?
  second_number_of_colours <- round(50*second_distance/first_distance)
  ## Make vector of colors for values smaller than 0 (20 colors)
  rc1 <- viridis::viridis(50, option = "D")
  ## Make vector of colors for values larger than 0 (180 colors)
  rc2 <- rep(viridis::viridis(1, begin = 1, option = "D"),second_number_of_colours)
  ## Combine the two color palettes
  rampcols <- c(rc1, rc2)
  pal <- colorNumeric(palette = rampcols, domain = data)
  return(pal)
  }

all_cities <- list.files("data/city_summaries/1km_without_water",pattern = "*")

all_circles <- list.files("data/city_summaries/ghsl_radii_circle_rds/",pattern = "*",full.names = T)



source("R/functions/get_city_lat_lon_from_web.R")
cities_list_for_shiny <- get_city_locations() %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])  %>% 
  rename(geonames_pop = population) %>%
  st_drop_geometry() %>% 
  mutate(city_name = paste0(name," (",country,")"))



circles <- map_df(all_circles,read_rds) %>% 
  left_join(cities_list_for_shiny, 
            by = "geoname_id") %>% 
  group_by(dist_km_round) %>% 
  filter(dist_km_round<101) %>% 
  select(-tiles_km,-tiles_cum,
         -area_with_water,
         -area_without_water,
         -area_cum_without_water,
         -area_cum_with_water
  ) %>% 
  mutate(pwd_with_water = pwd_with_water *1e6,
         pwd_cum_with_water = pwd_cum_with_water *1e6) %>% 
  rename(city = name)


#Check that populations make sense: 

geonames_list <- read_delim("data/geonames-all-cities-with-a-population-1000.csv",delim = ";") %>% 
  select(`Feature Class`,
         geoname_id = `Geoname ID`,
         Coordinates)

#Cities that are small suburbs of existing cities or where we don't have an accurate geocode of their city we can filter out
unnecesary_cities <- 
  circles %>% 
  filter(dist_km_round == 20) %>% 
  st_drop_geometry() %>% 
  mutate(population_ratio = population_cum/geonames_pop) %>% 
  mutate(google = paste0("https://www.google.com/maps/@",lat,",",lon,",11.46z")) %>% 
  select(city,country,geoname_id,population_cum,geonames_pop,population_ratio,source_of_lat_lon,google) %>% 
  filter(population_ratio > 14| 
           population_ratio < .4) %>% 
  pull(geoname_id)

#Export full file to s3 for other people to download
circles_s3_export <- circles %>% 
  filter(!(city_name %in% unnecesary_cities)) 

circles_s3_export %>% 
  st_drop_geometry() %>% 
  write_csv("data/s3_uploads/city-density.csv")
upload_object("data/s3_uploads/city-density.csv")

#Upload to AWS - let JN know if you want access to the bucket. If not commit your changes and he can run. 
if(Sys.info()[7] == "jonathannolan") {
  
  
  aws.s3::bucketlist(add_region = T)
  walk2(list_rmds_with_path,list_rmds,upload_object)
  
}



circles_qs <- circles_s3_export %>%
  select(-c(lat,
            lon,
            country_code_iso2c,
            country_code_iso3c,
            geonames_pop))


library(qs) 
circles_qs %>% 
  st_drop_geometry() %>% 
  qsave("output/qs_files/shiny.qs")

city_names <- unique(circles$circles_s3_export)


city_id <- circles_s3_export$geoname_id[1]


create_map_files <- function(city_id){
  
  circles_map_data <- circles_s3_export %>% 
    filter(geoname_id == city_id)
  
  unique_city_name <- unique(circles_map_data$city_name) %>% str_replace(" \\(","<br>\\(")
  square_map_data <- circles %>% 
    filter(geoname_id == city_id) 
  
  

  circle_pal <- viridis_palette_limiter(circles_map_data$pwd_with_water,2)
  
  
  circles_map <- circles_map_data %>%
    st_transform("wgs84") %>% 
    leaflet() %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~circle_pal(pwd_with_water),
      color = "white",
      weight = .1,
      opacity = .5,
      fillOpacity = 0.2
      #popup = ~paste(metric_column(), ":", data$value)  # Correct context for .data
    ) %>%
    addLegend(pal = circle_pal, values = ~pwd_with_water, opacity = 1,
              title = ~paste0("Density of rings around<br>",unique_city_name,"<br>(population weighted)
              <a href='square_100077.html'><br>Click for 1km squares"
              ),
              position = "bottomright") 

  
  # Save the map

  htmlwidgets::saveWidget(circles_map,paste0("data/s3_uploads/circle_",city_id,".html"),selfcontained = T,libdir = 'library')
  
  square_map_data <- readRDS(paste0("data/city_summaries/detailed_with_water/",city_id,".RDS"))
  
  ## It's not uncommon for outlier values to stuff up the scale. Let's fix that...
  
  
  # Assuming `square_map_data$population_single_tile` contains your data
  
square_pal <- viridis_palette_limiter(square_map_data$population_single_tile,10)
  
  
  square_map <-  square_map_data %>%
    st_transform("wgs84") %>% 
    leaflet() %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~square_pal(population_single_tile),
      color = "white",
      weight = .1,
      opacity = .1,
      fillOpacity = 0.8
      #popup = ~paste(metric_column(), ":", data$value)  # Correct context for .data
    ) %>%
    addLegend(pal = square_pal, values = ~population_single_tile, opacity = 1,
              title = ~paste0("Population per square km"#,<br>"#,unique(city_name)
              ),
              position = "bottomright") 
  
  
  htmlwidgets::saveWidget(square_map,paste0("data/s3_uploads/square_",city_id,".html"),libdir = 'library')
  
  
  
}

walk(unique(circles$geoname_id),create_map_files)


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

put_object(file = file_location, 
           bucket = "city-density",multipart = T,show_progress = T,
           headers = list("Content-Type" = "text/html"),verbose = T)
create_map_files(circles$geoname_id[1])


select(city_name,geometry,dist_km_round) %>%
  qsave(paste0("output/qs_files/",x,".qs"),)

## Used for internal checks. 
save_qs <- function(x){
  circles %>% 
    filter(city_name == x) %>%
    select(city_name,geometry,dist_km_round) %>%
    qsave(paste0("output/qs_files/",x,".qs"),)
}
walk(city_names,save_qs)

circles %>% filter(dist_km_round == 99) %>% 
  left_join(cities_list_for_reconciliation) %>% 
  st_drop_geometry() %>% 
  write_csv("data/cities_list_clean.csv")



tribble(~col_name,~metric_type,~water,~cumulative,
                   "area_with_water",          "Area",                       "","",
                   "area_without_water",       "Area",                       "excluding large bodies of water","",
                   "population",               "Population",                 "","",
                   "pwd_with_water",           "Density",                    "population weighted","",
                   "density_without_water",    "Density",                    "excluding large bodies of water","",
                   "density_with_water" ,      "Density",                    "","",
                   "population_cum",           "Population",                 "","cumulative",
                   "area_cum_without_water",   "Area",                       "excluding large bodies of water", "cumulative",
                   "pwd_cum_with_water" ,      "Density",                    "population weighted","cumulative",
                   "density_cum_with_water",   "Density",                    "","cumulative",
                   "density_cum_without_water","Density",                    "excluding large bodies of water","cumulative")%>%
  mutate(description = pmap_chr(list(metric_type, water, cumulative), function(...) {
    combined_values <- c(...)
    combined_values <- combined_values[combined_values != ""] # Remove empty values
    paste(combined_values, collapse = ", ")   # Concatenate with comma
  })) %>% 
  mutate(subtitle = pmap_chr(list(water, cumulative), function(...) {
    combined_values <- c(...)
    combined_values <- combined_values[combined_values != ""] # Remove empty values
    paste(combined_values, collapse = ", ")   # Concatenate with comma
  })) %>% 
  mutate(subtitle = stringr::str_to_sentence(subtitle)) %>% 
  mutate(units = case_when(metric_type == "Density"~"Residents per square kilometer",
                           metric_type == "Area"~"Square kilometers",
                           metric_type == "Population"~"Residents")) %>% 
  mutate(title = if_else(metric_type == "Population","Population living in ","Density of")) %>% 
  qs::qsave("output/qs_files/names_of_colums.qs")


circles_s3_export %>%
  st_drop_geometry() %>% 
  ungroup() %>% 
  distinct(geoname_id,
           city_name) %>% 
  left_join(cities_list_for_shiny %>% 
              select(source_of_lat_lon,
                     geoname_id,
                     lon,
                     lat)
            ) %>% qsave("output/qs_files/city_lat_lons.qs")

