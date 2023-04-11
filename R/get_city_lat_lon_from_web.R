library(countrycode)
library(ggmap)
library(tidyverse)
library(sf)

#You need a google API key in order to get the lat/lon of cities
if(!has_google_key()){register_google(readline(prompt="Enter your google API key: "),
                                      write = TRUE) }

#create data folder

get_google_city_locations <- function(){
output_directory = "data/city_locations"
if (!dir.exists(output_directory)){
  dir.create(output_directory) }

if (!file.exists("data/city_locations/city_locations_sf.rds")) {
  
city_names <-  read_csv("data/city_locations/list_of_cities_utf.csv") %>% 
  mutate(city_raw = city) %>% 
  separate(city,into = c("city","designation"), sep = "\\(") %>% 
  mutate(designation = str_remove_all(designation,"\\)")) %>% 
  mutate(country_code_iso3c = countrycode(country,"country.name","iso3c"),
         city = str_to_title(city),
         country = countrycode(country_code_iso3c,"iso3c","country.name"),
         city_label = paste0(country,": ",city)) 


city_locations_2 <- geocode(paste("locality of",city_names$city_raw,",",city_names$country),output = "more") %>% 
  bind_cols(city_names)  %>% 
  filter(!(type %in% c("country","establishment"))) %>%
  mutate(city_text = str_remove_all(city_label,"[^[:alnum:] ]"),
         city_ascii = trimws(stringi::stri_trans_general(str = city_text, 
                                                         id = "Latin-ASCII"))) %>% 
  filter(!is.na(lat)) 
  

city_locations_2%>% 
  write_csv("data/city_locations/city_locations.csv")

city_sf <- city_locations_2  %>% 
  sf::st_as_sf(coords = c("lon","lat"), crs = st_crs("wgs84"))

city_sf %>% 
  write_rds("data/city_locations/city_locations_sf.rds") 

} else {city_sf<- readRDS("data/city_locations/city_locations_sf.rds") }

return(city_sf)

}
