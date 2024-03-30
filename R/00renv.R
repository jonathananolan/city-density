
#Originally this package had RENV but it was messing up the shiny server

library(httr)
library(jsonlite)
library(leaflet)
library(furrr)
library(ggmap) #Google API key is required to find the centre of each city. This is free for limited use
library(geosphere) #find distance between two points
library(raster) #import raster data
library(sf) # You also need install.packages("rgdal") for this to work, which isn't installed automatically when installing sf/raster
library(countrycode) #get country codes to find url to download pop densities
library(curl) # More reliable file download than R's default
library(qs)
library(aws.s3)
library(htmlwidgets)
library(tidyverse)
library(htmltools)


source("R/functions/get_ghsl_files.R")
source("R/functions/get_city_lat_lon_from_web.R")
source("R/functions/create_summary_files_for_each_city.R")
source("R/functions/aws_and_leaflet_fns.R")
source("R/functions/get_global_water_bodies.R")


#postgres server no longer used.
# if (requireNamespace("rstudioapi", quietly = TRUE)) {
#   password <- rstudioapi::askForPassword("Enter database password")
# }


# # create connection to postgres 
# con <- dbConnect(RPostgres::Postgres(),
#                  host = 'localhost', # host name, can be website/server
#                  port = 5433, # default port of postgres
#                  dbname = 'postgres', # name of database in postgres
#                  user = 'postgres', # the default user
#                  password = password, # password of user
#                  options="-c search_path=public" # specify what schema to connect to
# )
# 
# options(digits=6)

###API KEYS

#Public transport locations
#https://www.ptv.vic.gov.au/footer/data-and-reporting/datasets/ptv-timetable-api/
#Sys.setenv("PTV_USER_ID" = rstudioapi::showPrompt(title = "Enter details",
#                                                  message="Enter PTV username"))
#Sys.setenv("PTV_API_KEY" = rstudioapi::showPrompt(title = "Enter details",
#                                                  message = "Enter PTV API key"))

#Heritage database
#Get an AKI key from https://www.developer.vic.gov.au/
#Sys.setenv(vic_gov_api = rstudioapi::showPrompt(title = "Enter developer vic api key",
#                                                  message="Enter") )

#helper function to look for files in a location
# Function to search directories recursively for files matching the geoname IDs
search_files <- function(directory,search_query) {
  # Initialize an empty vector to store the paths of files that match the geoname IDs
  matching_files <- character()
  
  # Internal function to perform the search
  find_matching_files <- function(directory) {
    # List all files and directories in the current directory
    files_and_dirs <- list.files(directory, full.names = TRUE)
    
    # Separate files and directories
    dirs <- files_and_dirs[vapply(files_and_dirs, function(x) file.info(x)$isdir, logical(1))]
    files <- setdiff(files_and_dirs, dirs)
    
    # Check each file to see if its name contains any of the geoname IDs
    for (file in files) {
      if (any(sapply(search_query, function(id) str_detect(file, id)))) {
        matching_files <<- c(matching_files, file) # Use <<- to modify variable in parent scope
      }
    }
    
    # Recursively search in subdirectories
    for (dir in dirs) {
      find_matching_files(dir)
    }
  }
  
  # Start the recursive search
  find_matching_files(directory)
  
  # Return the vector of matching files
  return(matching_files)
}

