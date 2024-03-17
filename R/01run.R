#This script creates a file for each populous city with km square grids showing population density in each square

source("R/00renv.R")

global_raster_pop <- get_ghsl_files()

cities_list <- get_city_locations() %>%
               st_transform(st_crs(global_raster_pop))



water_bodies_global_moll <- get_water_bodies()


gc()
plan(multisession, 
     workers = 3)


already_finished_cities <- list.files("data/city_summaries/ghsl_radii_circle_qs/") %>% str_remove(".qs")

cities_to_run <- cities_list %>% 
        filter(!(geoname_id %in% already_finished_cities)) %>% 
        pull(geoname_id)

gc()

future_walk(.x = cities_to_run,
            .f = create_summary_files_for_each_city,
            .progress = T)

walk(.x = cities_list$geoname_id,
     .f = create_summary_files_for_each_city,
     .progress = T)
