# get water body map from:
# http://gis.ess.washington.edu/data/vector/worldshore/index.html

# get water body map from:
# http://gis.ess.washington.edu/data/vector/worldshore/index.html

get_global_water_bodies <- function(moll = T){
  water_body_files <-  c("shore_ne",                     
                         "shore_se", 
                         "shore_sw",
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
  
  if(!file.exists("data/water_bodies/water_bodies.rds")) {
    output <- map_df(water_body_filenames,sf::st_read)
    
    write_rds(output,"data/water_bodies/water_bodies.rds") } else {
      output <- read_rds("data/water_bodies/water_bodies.rds")
    }
  
  return(output)
  
}
