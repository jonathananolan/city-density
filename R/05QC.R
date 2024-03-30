source("R/functions/ggplot_theme.R")

circles_qc %>% 
  ggplot(aes(x = dist_km_round,tiles_km, group = city_name)) +
  geom_line(stat = "identity", colour = jn_colours$complementary[1])+
  theme_jn_caption(plot_type = "line")+
  labs(title = "Tiles per km")


circles_qc %>%   
  mutate(ratio = tiles_cum / median(tiles_cum)) %>% 
  filter(dist_km_round == 99) %>% 
  ggplot(aes(x     = dist_km_round,
             y     = ratio, 
             group = city_name)) +
  geom_point(stat   = "identity", 
            colour = jn_colours$complementary[1])+
  theme_jn_caption(plot_type = "line") +
  labs(title = "Cumulative tiles per km")

geonames_to_run_again <- circles_qc %>% 
  filter(dist_km_round == 99) %>% 
  group_by(dist_km_round) %>% 
  mutate(ratio = tiles_cum / median(tiles_cum)) %>% 
  filter(ratio < .999) %>% 
  pull(geoname_id)
geonames_to_run_again
data <- paste0("d")
geonames_to_run_again_str <- as.character(geonames_to_run_again)



# Use the function and store the result
found_files <- search_files("data")

# Start the search
walk(found_files,file.remove)
