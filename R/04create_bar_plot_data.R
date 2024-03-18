source("R/00renv.R")


cities_list_for_shiny <- get_city_locations()  %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])  %>% 
  rename(geonames_pop = population) %>% 
  dplyr::select(-country,-source_of_lat_lon,-name) %>% 
  st_drop_geometry()

city_data <- qread("output/qs_files/shiny.qs") %>% 
  left_join(cities_list_for_shiny) %>% 
  filter(!is.na(city_name))

#For city rankings we don't want to count the same people twice. So when the radius around a city starts to overlap
#with another city's radius, the smaller city should be excluded. 
#to Facilitate that we create a column saying where the nearest city is that is bigger than that city. 
#The function excludes cities that have already been excluded because they are **even** closer. 
#e.g. we exclude Philly when it's close to NYC, even though it's slightly closer to Brooklyn, 
#because Brooklyn gets excluded much earlier, and so there's no risk of the Brooklyn + phily circles overlpping. 

city_one <- city_data %>%
  filter(dist_km_round == 15) %>% 
  mutate(geonames_pop = coalesce(geonames_pop,population_cum)) %>% #Some cities don't have a geoname pop so we use the best guess we have - pop within 15km which is the default value on the shiny app. 
  ungroup() %>% 
  distinct(lat,lon,city_name,geonames_pop) %>% 
    filter(!is.na(city_name))

city_two <- city_one %>%
  rename_all(~paste0(.x,"_2"))

biggest_city <- city_one %>% arrange(desc(geonames_pop)) %>% 
  filter(row_number() == 1) %>% 
  select(city_name) %>% 
  mutate(dist_excluded = 10e10)

all_combinations_of_cities <- expand.grid(city_name = city_one$city_name,
                                        city_name_2 = city_two$city_name_2) %>%
  left_join(city_one) %>% 
  left_join(city_two) %>% 
  filter(geonames_pop_2 > geonames_pop ) %>% 
  mutate(dist = round(distHaversine(cbind(lon, lat), cbind(lon_2, lat_2))/1000))

km_where_city_excluded <- all_combinations_of_cities%>% 
  group_by(city_name) %>% 
  arrange(dist) %>% 
  filter(row_number() == 1) %>%
  mutate(dist_excluded = ceiling(dist/2)) %>% # /2 because that's when radius overlap 
  select(city_name,
         dist_excluded) %>% 
  bind_rows(biggest_city)

city_data_with_distances <- city_data %>% 
  left_join(km_where_city_excluded, by = "city_name") %>% 
  left_join(countrycode::codelist %>% select(country_code_iso3c = iso3c ,region), by = "country_code_iso3c") %>% 
  mutate(country_code_iso2c =  tolower(country_code_iso2c))

library(dtplyr)

filterer_pop <- function(km,pop_col){
  output <- city_data_with_distances %>% 
    filter(dist_km_round == km,
           dist_excluded >= km) %>%
    select(city_name,city, country, geoname_id, value = {{pop_col}},dist_km_round,region,country_code_iso3c,country_code_iso2c) %>%
    arrange(desc(value)) %>% 
    mutate(order = row_number(),
           city_name = fct_rev(fct_reorder(city_name,row_number())),
           city = fct_rev(fct_reorder(city,row_number()))
           ) %>% ungroup() %>% 
    as.data.table()
  
  return(output)

}


pop_cum     <- map2(seq(1,100), "population_cum",            filterer_pop, .progress = T)
density_cum <- map2(seq(1,100), "density_cum_without_water", filterer_pop, .progress = T)

pop_cum %>% qsave("output/qs_files/pop_rankings.qs")
density_cum %>% qsave("output/qs_files/density_cum_rankings.qs")
