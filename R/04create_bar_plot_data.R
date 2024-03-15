source("R/00renv.R")


cities_list_for_shiny <- get_city_locations()  %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])  %>% 
  rename(geonames_pop = population) %>% 
  dplyr::select(-country,-source_of_lat_lon,-name) %>% 
  st_drop_geometry()

city_data <- qread("output/qs_files/shiny.qs") %>% 
  left_join(cities_list_for_shiny)



#Manually check for duplicate cities
duplicate_cities <- c("New Cairo (Egypt)",
                      "Borivli (India)",
                      "Thāne (India)",
                      "Dombivli (India)",
                      "Bhiwandi (India)",
                      "Najafgarh (India)",
                      "Rohini (India)",
                      "Narela (India)",
                      "Ghāziābād (India)",
                      "Faridabad (India)",
                      "Pānihāti (India)",
                      "South Tangerang (Indonesia)",
                      "Depok (Indonesia)",
                      "Cibinong (Indonesia)",
                      "Bekasi (Indonesia)",
                      #"Mandaluyong City (Philippines)",
                      #"Quezon City (Philippines)",
                      "Antipolo (Philippines)",
                      "Taguig (Philippines)",
                      "Paranaque City (Philippines)",
                      "Las Piñas (Philippines)",
                      "Gwangmyeong (Korea, Republic of)",
                      "Anyang-si (Korea, Republic of)",
                      "Machida (Japan)",
                      "Kawaguchi (Japan)",
                      "Wakayama (Japan)",
                      "Minato (Japan)",
                      "Saitama (Japan)",
                      "Guarulhos (Brazil)",
                      "Diadema (Brazil)",
                      "São Bernardo do Campo (Brazil)",
                      "Guarulhos (Brazil)",
                      "Santo André (Brazil)",
                      "Tangerang (Indonesia)",
                      "Rajpur Sonarpur (India)",
                      "Matsudo (Japan)",
                      "Tokorozawa (Japan)",
                      "Koshigaya (Japan)",
                      "Foshan (China)",
                      "Goyang-si (Korea, Republic of)",
                      "Seongnam-si (Korea, Republic of)",
                      "Ansan-si (Korea, Republic of)",
                      "Yokohama (Japan)",
                      #"Kawasaki (Japan)",
                      "Bhayandar (India)",
                      "Kalyān (India)",
                      "San Jose del Monte (Philippines)",
                      "Mandaluyong City (Philippines)",
                      "Quezon City (Philippines)",
                      "Naucalpan de Juárez (Mexico)",
                      "Ecatepec de Morelos (Mexico)",
                      "Santa María Chimalhuacán (Mexico)",
                      "Osasco (Brazil)",
                      "Tuen Mun (Hong Kong, China)",
                      "Incheon (Korea, Republic of)",
                      "Suwon (Korea, Republic of)",
                      "Zagazig (Egypt)",
                      #"Shenzhen (China)",
                      "Masina (Congo, Democratic Republic of the)",
                      "Samut Prakan (Thailand)",
                      "Honchō (Japan)",
                      "Kawagoe (Japan)",
                      "Ciudad López Mateos (Mexico)",
                      "Samut Prakan (Thailand)",
                      "Kawagoe (Japan)",
                      "Honchō (Japan)",
                      "Xochimilco (Mexico)",
                      "Brazzaville (Congo)",
                      "Ataşehir (Turkey)",
                      "Uijeongbu-si (Korea, Republic of)",
                      "Cuautitlán Izcalli (Mexico)",
                      "Brooklyn (United States)",
                      "Queens (United States)",
                      "The Bronx (United States)",
                      "Carapicuíba (Brazil)",
                      "Ikeja (Nigeria)",
                      "Ebute Ikorodu (Nigeria)",
                      "Maltepe (Turkey)",
                      "Biên Hòa (Viet Nam)",
                      "Tsuen Wan (Hong Kong, China)",
                      "Sha Tin (Hong Kong, China)",
                      "Sha Tin Wai (Hong Kong, China)",
                      "Suita (Japan)",
                      "Hirakata (Japan)",
                      "Hachiōji (Japan)",
                      "Kashiwa (Japan)",
                      "Bogor (Indonesia)",
                      "Bhātpāra (India)",
                      "Ulhasnagar (India)",
                      "Takatsuki (Japan)",
                      "Duque de Caxias (Brazil)",
                      "Roodepoort (South Africa)",
                      "Sharjah (United Arab Emirates)",
                      "Laval (Canada)",
                      "Ōtsu (Japan)",
                      "Yiwu (China)",
                      "Omdurman (Sudan, The Republic of)",
                      "Turmero (Venezuela, Bolivarian Rep. of)",
                      "Dongguan (China)",
                      "Fujisawa (Japan)",
                      "Itaquaquecetuba (Brazil)",
                      "Kunshan (China)",
                      "Meerut (India)",
                      "New South Memphis (United States)"
)

#Now the big ones are sorted a function can sort the rest...


add_bigger_closer_city <- function(cities,dist) {
  # Calculate all pairwise distances
  distances <- geosphere::distm(cities[, c("lon", "lat")], cities[, c("lon", "lat")], fun = distHaversine) / 1000  # Convert meters to km
  
  # Initialize a column for the name of a bigger, closer city
  cities$bigger_closer_city <- NA
  
  # Loop over each city
  for (i in 1:nrow(cities)) {
    # Find cities within 30km
    close_cities <- which(distances[i, ] <= dist & distances[i, ] > 0)  # Exclude the city itself
    
    # Initialize variables to find the closest bigger city
    closest_distance <- Inf
    closest_bigger_city <- NA
    
    # For each close city, check if it has a larger population and is the closest
    for (j in close_cities) {
      if (cities$geonames_pop[j] > cities$geonames_pop[i] && distances[i, j] < closest_distance) {
        closest_distance <- distances[i, j]
        closest_bigger_city <- cities$city_name[j]
      }
    }
    
    # Update the name of the bigger, closer city
    cities$bigger_closer_city[i] <- closest_bigger_city
  }
  
  return(cities)
}


city_data <- city_data %>% filter(!is.na(city))



filterer_pop <- function(km,pop_col){
  
  cities_to_include <- city_data %>% 
    filter(!(city_name %in% duplicate_cities)) %>% 
    filter(dist_km_round == km ) %>% 
    mutate(geonames_pop = if_else(is.na(geonames_pop),100,geonames_pop)) %>%
    add_bigger_closer_city(km) %>% 
    filter(is.na(bigger_closer_city)) %>% 
    pull(city_name)
  
  
  output <-city_data %>% 
    filter(city_name %in% cities_to_include) %>% 
    filter(dist_km_round == km) %>%
    left_join(countrycode::codelist %>% select(country_code_iso3c = iso3c ,region), by = "country_code_iso3c") %>% 
    select(city_name,city, country, geoname_id, value = {{pop_col}},dist_km_round,region,country_code_iso3c,country_code_iso2c) %>%
    #select(city_name, city, country, geoname_id, value = density_cum_without_water ,dist_km_round,region) %>%
    arrange(desc(value)) %>% 
    filter(row_number() <31) %>% 
    mutate(order = row_number(),
           city_name = fct_rev(fct_reorder(city_name,row_number())),
           city = fct_rev(fct_reorder(city,row_number())),
           country = str_replace_all(string=country,pattern="\\,.*$",replacement=" "),
           country_code_iso2c =  tolower(country_code_iso2c)
           ) %>% ungroup() 

}


pop_cum     <- map2(seq(1,100), "population_cum",            filterer_pop)
density_cum <- map2(seq(1,100), "density_cum_without_water", filterer_pop)

pop_cum %>% qsave("output/qs_files/pop_rankings.qs")
density_cum %>% qsave("output/qs_files/density_cum_rankings.qs")
