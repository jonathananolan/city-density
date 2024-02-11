library(tidyverse)
mel_data <- read_rds("data/ghsl_sf/1km_Australia Melbourne.RDS") %>% 
  mutate(population = case_when(dist_km_round == 12 ~ population+(1e6/10),
                                dist_km_round == 13 ~ population+(1e6/10),
                                dist_km_round == 14 ~ population+(1e6/5),
                                dist_km_round == 16 ~ population+(1e6/10),
                                dist_km_round == 17 ~ population+(1e6/10),
                                dist_km_round == 18 ~ population+(1e6/5),
                                dist_km_round == 19 ~ population+(1e6/10),
                                dist_km_round == 20 ~ population+(1e6/10),
                                T ~ population),
         density = population/(area*1e-6),
         city_ascii = "Melbourne with a million people around SRL stations")

data <-           read_rds("data/ghsl_sf/1km_Australia Melbourne.RDS") %>% 
        bind_rows(read_rds("data/ghsl_sf/1km_United States Los Angeles.RDS"))


data %>% 
  bind_rows(mel_data) %>% 
  ggplot(aes(x = dist_km_round,
             y = density, 
             colour = city_ascii))+
  geom_line(stat = "identity",
            size = 2)+
  theme_minimal()+
  labs(title = "LA's suburbs are more dense than Melbourne.",
       caption = "Source: GHSL, excludes bodies of water.",
       group = "City",
       x = "Distance from CBD",
       y = "Density")


