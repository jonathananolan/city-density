srl_stations <- 
  tribble(~name,~lon,~lat, 
          "southland",-37.95814389091898, 145.0488637133262,
          "Clayton",-37.924386085737005, 145.1205863538048,
          "Monash",-37.91485520273969, 145.1327532200224,
          "Glen Waverley",-37.87954006753537, 145.16186175558119,
          "Burwood",-37.84987130765103, 145.113955991112,
          "Box Hill",-37.81711527337461, 145.12273944628174,
          "Doncaster",-37.78628008631726, 145.1264186984583,
          "Heidelberg",-37.757246850366236, 145.0611670648962,
          "Reservoir",-37.7168373116437, 145.00701711277185,
          "Latrobe", -37.72459394582586, 145.05015385087205,
          "Broadmeadows",-37.6827601919551, 144.9197552728396,
          "Fawkner",-37.714404888024156, 144.96005965804608)


city_one <- "Melbourne"
city_two <- "London"

sf_use_s2(FALSE)

#Create a graph that shows each city compared to Melbourne
create_mel_line <- function(new_city) {
  circle_map %>% 
    filter(city == "Melbourne") %>%
    mutate(city = new_city,
           city_graph = "Melbourne")
  
}
all_mel_cities <-map_df(city_locations_sf$city_name,create_mel_line)

circle_map %>% 
  mutate(city_graph = "other") %>% 
  bind_rows(all_mel_cities) %>% 
  filter(city!="Melbourne",
         dist_km_round<40) %>% 
  ggplot(aes(x = dist_km_round, 
             y = as.numeric(density),
             colour = city_graph))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~city,scales = "free_y")

print("weird, why am I here?!")
