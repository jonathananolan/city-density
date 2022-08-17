library(countrycode)
#get cities of interest 

#create data folder

output_directory = "data"
if (!dir.exists(output_directory)){
  dir.create(output_directory) }


#population density in 30 arc grid raster from https://hub.worldpop.org/geodata/listing?id=76
get_city_locations <- function(cities_to_import = 999) {
city_names <- tribble( ~ city_name, ~ country, 
                       "Melbourne","Australia",
                       "London","United Kingdom",
                       "Sydney","Australia",
                       "Brisbane","Australia",
                       "Adelaide","Australia",
                       "Berlin","Germany",
                       "San Francisco","USA",
                       "Tokyo","Japan",
                       "Delhi","India",
                       "Shanghai","China",
                       "Sao Paulo","Brazil",
                       "Mexico City","Mexico",
                       "Cairo","Egypt",
                       "Mumbai","India",
                       "Beijing","China",
                       "Dhaka","Bangladesh",
                       "Osaka","Japan",
                       "New York","United States",
                       "Karachi","Pakistan",
                       "Buenos Aires","Argentina",
                       "Chongqing","China",
                       "Istanbul","Turkey",
                       "Kolkata","India",
                       "Manila","Philippines",
                       "Lagos","Nigeria",
                       "Rio de Janeiro","Brazil",
                       "Tianjin","China",
                       "Kinshasa","DR Congo",
                       "Guangzhou","China",
                       "Los Angeles","United States",
                       "Moscow","Russia",
                       "Shenzhen","China",
                       "Lahore","Pakistan",
                       "Bangalore","India",
                       "Paris","France",
                       "Bogotá","Colombia",
                       "Jakarta","Indonesia",
                       "Chennai","India",
                       "Lima","Peru",
                       "Bangkok","Thailand",
                       "Seoul","South Korea",
                       "Nagoya","Japan",
                       "Hyderabad","India",
                       "Tehran","Iran",
                       "Chicago","United States",
                       "Chengdu","China",
                       "Nanjing","China",
                       "Wuhan","China",
                       "Ho Chi Minh City","Vietnam",
                       "Luanda","Angola",
                       "Ahmedabad","India",
                       "Kuala Lumpur","Malaysia",
                       "Xi'an","China",
                       "Hong Kong","China",
                       "Dongguan","China",
                       "Hangzhou","China",
                       "Foshan","China",
                       "Shenyang","China",
                       "Riyadh","Saudi Arabia",
                       "Baghdad","Iraq",
                       "Santiago","Chile",
                       "Surat","India",
                       "Madrid","Spain",
                       "Suzhou","China",
                       "Pune","India",
                       "Harbin","China",
                       "Houston","United States",
                       "Dallas","United States",
                       "Toronto","Canada",
                       "Dar es Salaam","Tanzania",
                       "Miami","United States",
                       "Belo Horizonte","Brazil",
                       "Singapore","Singapore",
                       "Philadelphia","United States",
                       "Atlanta","United States",
                       "Fukuoka","Japan",
                       "Khartoum","Sudan",
                       "Barcelona","Spain",
                       "Johannesburg","South Africa",
                       "Saint Petersburg","Russia",
                       "Qingdao","China",
                       "Dalian","China",
                       "Washington","United States",
                       "Yangon","Myanmar",
                       "Alexandria","Egypt",
                       "Jinan","China",
                       "Guadalajara","Mexico") %>% 
  mutate(country_code = countrycode(country,"country.name","iso3c"),
         tif_filename = paste0(tolower(country_code),"_pd_2020_1km.tif"),
         url = paste0("https://data.worldpop.org/GIS/Population_Density/Global_2000_2020_1km/2020/",country_code,"/",tif_filename))

city_names_filtered <- city_names %>% 
  filter(row_number() <= cities_to_import)

get_pop_density_file <- function(url,tif_filename){
  
  output_directory = "data/population_densities"
  if (!dir.exists(output_directory)){
    dir.create(output_directory) }
  
  dest_path = paste0("data/population_densities/",tif_filename)
  
  if(!file.exists(dest_path)){
    curl_download(url, dest_path)
    
  }
  
  
}


walk2(city_names_filtered$url,city_names_filtered$tif_filename,get_pop_density_file)


if(!file.exists("data/city_locations_sf.rds")) {
city_locations_sf <- geocode(paste(city_names$city_name,",",city_names$country)) %>% 
  bind_cols(city_names) %>% 
  sf::st_as_sf(coords = c("lon","lat"), crs = st_crs(poly))  %>% 
  mutate(rds_file_name = paste0("data/city_density_rds/poly_for_",city_name,".RDS"))

write_rds(city_locations_sf,"data/city_locations_sf.rds") } else {
  city_locations_sf <- read_rds("data/city_locations_sf.rds")
}

return(city_locations_sf %>% filter(row_number() <= cities_to_import )) 

}

# get water body map from:
# http://gis.ess.washington.edu/data/vector/worldshore/index.html
get_global_water_bodies <- function(){
water_body_files <-  c("shore_ne",                     
                       "shore_se", 
                       "shore_ne",
                       "shore_nw")

get_water_bodies_file <- function(file_name){
  
  output_directory = "data/water_bodies"
  if (!dir.exists(output_directory)){
    dir.create(output_directory) }

  dest_path = paste0("data/water_bodies/",file_name,".shp")
  
  if(!file.exists(dest_path)){
    
    curl_download(paste0("http://gis.ess.washington.edu/data/vector/worldshore/",
                         file_name,
                         ".zip"),
                  paste0("data/water_bodies/",file_name,".zip"))
    
    utils::unzip(paste0("data/water_bodies/",file_name,".zip"),
                 exdir = "data/water_bodies")
  }

}

walk(water_body_files,get_water_bodies_file)

water_body_filenames = paste0("data/water_bodies/",water_body_files,".shp")

if(!file.exists("data/water_bodies.rds")) {
  output <- map_df(water_body_filenames,sf::st_read)
  
  write_rds(output,"data/water_bodies.rds") } else {
    output <- read_rds("data/water_bodies.rds")
  }

return(output)

}




