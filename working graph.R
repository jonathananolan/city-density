library(tidyverse)


data <- read_csv("city_density.csv")


data %>% 
  filter(metric_type == "Population",
         cumulative  == "Cumulative",
        # population_weighted == "Density",
        # population_weighted == "Population weighted density", 
        # include_water == "Counting water in area calculation"
         ) %>% 
  filter(city %in% c("Amsterdam","Melbourne","Brisbane","Sydney"),
         `Distance from the city centre in KM`<50) %>% 
  ggplot(aes(x = `Distance from the city centre in KM`, y = value, colour = city))+
  geom_line(size = 1)+
  theme_minimal()+
  theme(panel.grid = element_blank())+
  labs(title = "Population within distance of city centre",
       caption = "Data from GHSL",
       y = element_blank(),
       colour = "City")+
  scale_y_continuous(labels = scales::number_format(big.mark = ","))


data %>% 
  filter(metric_type == "Density",
         cumulative  == "Not cumulative",
         # population_weighted == "Density",
         population_weighted == "Population weighted density", 
         include_water == "Counting water bodies in area calculation"
  ) %>% 
  filter(city %in% c("Amsterdam","Melbourne","Brisbane","Sydney"),
         `Distance from the city centre in KM`<30) %>% 
  ggplot(aes(x = `Distance from the city centre in KM`, y = value, colour = city))+
  geom_line(size = 1)+
  theme_minimal()+
  theme(panel.grid = element_blank())+
  labs(title = "Population density at distance from city centre",
       subtitle = "People per square km - population weighted using 1km grid",
       caption = "Data from GHSL",
       y = element_blank(),
       colour = "City")+
  scale_y_continuous(labels = scales::number_format(big.mark = ","))


amsterdam <- read_rds("data/city_summaries/detailed/Netherlands Amsterdam.RDS") %>% 
  filter(dist_km_round %in% c(7,8,9))

