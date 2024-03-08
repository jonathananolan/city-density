
#one-time use
#renv::init()
#save packages status
#renv::snapshot()
#load package from save file
renv::restore()

library(DBI)
library(RPostgres)
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
library(tidyverse)

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  password <- rstudioapi::askForPassword("Enter database password")
}


# create connection to postgres 
con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost', # host name, can be website/server
                 port = 5433, # default port of postgres
                 dbname = 'postgres', # name of database in postgres
                 user = 'postgres', # the default user
                 password = password, # password of user
                 options="-c search_path=public" # specify what schema to connect to
)

options(digits=6)

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

