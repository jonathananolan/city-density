source("R/00renv.R")


all_cities <- list.files("data/city_summaries/1km_without_water",pattern = "*")
all_circles <- list.files("data/city_summaries/ghsl_radii_circle_qs/",pattern = "*",full.names = T)

cities_list_for_shiny <- get_city_locations() %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])  %>% 
  rename(geonames_pop = population) %>%
  st_drop_geometry() %>% 
  mutate(city_name = paste0(name," (",country,")"))

cities_list_for_shiny %>% qs::qsave("data/qc/qc_of_city_list.qs")

circles_qc <- map_df(all_circles,qread) %>% 
  left_join(cities_list_for_shiny, 
            by = "geoname_id") %>% 
  group_by(dist_km_round) %>% 
  filter(dist_km_round<101) %>% 
  mutate(pwd_with_water = pwd_with_water *1e6,
         pwd_cum_with_water = pwd_cum_with_water *1e6) %>% 
  rename(city = name)

circles_qc %>% qs::qsave("data/qc/qc_of_circles.qs")

circles <- circles_qc %>% 
  select(-tiles_km,-tiles_cum,
         -area_with_water,
         -area_without_water,
         -area_cum_without_water,
         -area_cum_with_water
  ) 


#Check that populations make sense: 

geonames_list <- read_delim("data/geonames-all-cities-with-a-population-1000.csv",delim = ";") %>% 
  select(`Feature Class`,
         geoname_id = `Geoname ID`,
         Coordinates)


circles %>% 
  st_drop_geometry() %>% 
  write_csv("data/s3_uploads/city-density.csv")

put_object(file = "data/s3_uploads/city-density.csv", 
           bucket = "city-density",multipart = T,show_progress = T)

circles %>%
  select(-c(lat,
            lon,
            country_code_iso2c,
            country_code_iso3c,
            geonames_pop)) %>% 
  st_drop_geometry() %>% 
  qsave("output/qs_files/shiny.qs")


circles %>%
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
  mutate(title = if_else(metric_type == "Population","Population living near ","Density near")) %>% 
  qs::qsave("output/qs_files/names_of_colums.qs")



