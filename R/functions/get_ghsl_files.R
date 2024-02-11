# Download this file.... a bit lazy not to do it for you sorry but it's so big it can be a hassle

get_ghsl_files <- function(){
output_directory = "data/ghsl/"
if (!dir.exists(output_directory)){
  dir.create(output_directory,recursive = T) }

file_name = paste0(output_directory,"ghsl.zip")
if (!file.exists(file_name)){
  download.file("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_1000/V1-0/GHS_POP_E2025_GLOBE_R2023A_54009_1000_V1_0.zip",
                file_name,method = "curl")
  unzip(file_name,exdir = output_directory)
}

world_map <- raster("data/ghsl/GHS_POP_E2025_GLOBE_R2023A_54009_1000_V1_0.tif")
  
#water bodies can be helpful for density calculations, so we'll change those NA values to 0. 
world_map <- reclassify(world_map, cbind(NA, 0))

return(world_map)

}
