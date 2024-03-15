

get_water_bodies <- function(){
#Water bodies are from an esri source - it was slightly quicker to convert to mollweide using qgis, but you could easily recreate the data yourself by going here. 
  
  #https://hub.arcgis.com/content/esri::world-water-bodies/about
  #or here...
  #https://global-surface-water.appspot.com/
  #https://collections.sentinel-hub.com/water-bodies/
  
  folder <- "input_data/water_bodies"
  filename <- "water_mol_simplified.shp"
  full_path <- paste0(folder,"/",filename)
  zip_file <- paste0(folder,"/water_bodies.zip")
  if (!dir.exists(folder)){  dir.create(folder,recursive = T) }
  
  if(!file.exists(full_path)){
    print("Downloading water bodies from AWS")
    download.file("https://city-density.s3.amazonaws.com/water_bodies/water_bodies.zip",zip_file)
    unzip(zipfile = zip_file,exdir = folder)
    read_sf(full_path)
  } else{
    read_sf(full_path)
  }
  
}
