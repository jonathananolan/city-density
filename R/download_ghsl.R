get_GHS_for<-function(geo=NULL,resolution=c("250","1k"),
                      year=c("1975","1990","2000","2015"),
                      timeout=10000,
                      base_path=getOption("custom_data_path")){
  if (is.null(base_path)) base_path <- tempdir()
  buffer <- ifelse(resolution=="1k",500,125)
  raster_path = paste0(base_path,"GHS/GHS_POP_GPW4",year,"_GLOBE_R2015A_54009_",resolution,"_v1_0/GHS_POP_GPW4",year,"_GLOBE_R2015A_54009_",resolution,"_v1_0.tif")
  if (!file.exists(raster_path)) {
    temp=tempfile()
    to <- getOption("timeout")
    options("timeout"=timeout)
    download.file(paste0("http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GPW4_GLOBE_R2015A/GHS_POP_GPW4",year,"_GLOBE_R2015A_54009_",resolution,"/V1-0/GHS_POP_GPW4",year,"_GLOBE_R2015A_54009_",resolution,"_v1_0.zip"),temp)
    options(timeout = to)
    exdir=file.path(base_path,"GHS")
    if (!dir.exists(exdir)) {dir.create(exdir)}
    zip::unzip(temp,exdir = exdir)
    if (!file.exists(raster_path))
      stop("Downloading of raster file failed, probably needs some tweaking of the code.")
  }
  r <- raster::raster(raster_path)
  if (!is.null(geo)) {
    vv <- as(geo %>% sf::st_transform(as.character(projection(r))) %>% sf::st_buffer(buffer),"Spatial")
    rr <- raster::crop(r,extent(vv))
    rr <- raster::mask(rr,vv)
  } else {
    rr=r
  }
  #wgs_poj4 <- "+proj=longlat +datum=WGS84 +no_defs"
  #rr %>% projectRaster(crs=wgs_poj4)
  rr
}



year_list = c(1975,1990,2000,2015)

download <- function(year,resolution = "1k") {
download.file(paste0("http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GPW4_GLOBE_R2015A/GHS_POP_GPW4",year,"_GLOBE_R2015A_54009_",resolution,"/V1-0/GHS_POP_GPW4",year,"_GLOBE_R2015A_54009_",resolution,"_v1_0.zip"),
              paste0("data/",year,resolution,".zip"))
}
library(tidyverse)
map(year_list,download)
map(year_list,download,resolution = 250)
