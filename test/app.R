#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(sf)
library(leaflet)
library(curl)
library(ggmap)
library(ggiraph)
library(units)
library(scales)
library(tidyverse)

setwd("..") #only run this in the shiny app - don't do it in your project folder! 
source("R/graph_functions.R")

city_locations_sf <- get_city_locations(cities_to_import=7) 
circle_map <- map_df(city_locations_sf$rds_1km_circle_name,readRDS) %>% 
  mutate(density = density*1e6)
km_2_map <- map_df(city_locations_sf$rds_file_name,readRDS) %>% 
  mutate(density = population/(area/1e6),
         density = as.numeric(density))

dist_from_cbd<-30

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
  navbarPage("Density explorer"),
    fluidRow(
      column(6,
             selectInput("main_city",
                         "City:",
                         choices = city_locations_sf$city_label,
                         selected = "Australia: Melbourne"),
      ),
      column(6,
             selectInput("second_city",
                         "City:",
                         choices = city_locations_sf$city_label,
                         selected = "United Kingdom: London")
      )),
    
    fluidRow(
      column(12,  
    textOutput("intro_text"))), 
    

    fluidRow(

      column(6,  
             textOutput("main_city_km_title"), 
             leafletOutput("main_city_km_leaflet")

              ),
      column(6,  
             textOutput("second_city_km_title"), 
             leafletOutput("second_city_km_leaflet")),
    
      ),
      fluidRow(

        column(6,
               textOutput("main_city_ring_title"), 
               leafletOutput("main_city_density_circle")
               
        ),
        column(6,
               textOutput("second_city_ring_title"), 
               leafletOutput("second_city_density_circle"))
               
      ),
    h2(textOutput("compare_title")),
    fluidRow(
      column(6,
             textOutput("compare_text"), 
             leafletOutput("second_city_comparor")),
    
    fluidRow(
      column(6,
             girafeOutput("lineplot"))),
      
      
             
    
    
    
        
        
          
          
        )

        # Show a plot of the generated distribution

    )



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$main_city_km_title <- renderText({ paste0(input$main_city,"'s population density") })
  output$second_city_km_title <- renderText({ paste0(input$second_city,"'s population density") })
  
  output$main_city_ring_title <- renderText({ paste0(input$main_city,"'s population density as 1km rings away from the centre") })
  output$second_city_ring_title <- renderText({ paste0(input$second_city,"'s population density as 1km rings away from the centre") })
  output$compare_text <- renderText({ paste0("Using 1km rings away from the centre") }
                                    
                                    )
  output$compare_title <- renderText({ paste0("Comparing ",
                                             input$main_city, 
                                             "'s density to", 
                                             input$second_city,
                                             ".") }
                                    
  )
  
  output$intro_text <- renderText({ 
    
    main_city <- city_locations_sf %>%
      filter(city_label == input$main_city)%>%
      pull(city_name)
    
    second_city  <- city_locations_sf %>%
      filter(city_label == input$second_city)%>%
      pull(city_name)
    
    pop_numbers <- km_2_map %>% 
      filter(dist_km_round <= dist_from_cbd) %>% 
      filter(city %in% c(main_city,second_city)) %>%
      st_drop_geometry() %>% 
      group_by(city) %>% 
      filter(population>0) %>% 
      summarise(pwd = weighted.mean(x = density,
                                    w = population),
                population = sum(population),
                area = sum(area)/1e6,
                n=n())
    
    paste0(round((pop_numbers %>% filter(city == main_city) %>% pull(population))/1e6,1),
            " million people live within ",
           dist_from_cbd, 
           "km of ",
           main_city,
           "'s centre and ",
           round((pop_numbers %>% filter(city == second_city) %>% pull(population))/1e6,1),
            " million people within ",
           dist_from_cbd, 
           "km of ",
           second_city, 
          ". The average person who lives within ",
           dist_from_cbd,
           "km of ",
          main_city, " lives in an area ",
           round(100*(pop_numbers %>% filter(city == main_city) %>% pull (pwd)/
                     pop_numbers %>% filter(city == second_city) %>% pull (pwd))),
           "% as dense as ",
          second_city,
           ". ") 
    
  })
  
  
  
  output$main_city_km_leaflet <- renderLeaflet({

    main_city <- city_locations_sf %>%
      filter(city_label == input$main_city)%>%
      pull(city_name)
    
    second_city <- city_locations_sf %>%
      filter(city_label == input$second_city)%>%
      pull(city_name)

    km_2_map_creator(main_city,second_city)
  })
  
  
  output$second_city_km_leaflet <- renderLeaflet({
    main_city <- city_locations_sf %>%
      filter(city_label == input$main_city)%>%
      pull(city_name)
    
    second_city <- city_locations_sf %>%
      filter(city_label == input$second_city)%>%
      pull(city_name)
    
    km_2_map_creator(second_city,main_city)
  })
  

  output$main_city_density_circle <- renderLeaflet({
    
    main_city <- city_locations_sf %>%
      filter(city_label == input$main_city)%>%
      pull(city_name)
    
    second_city <- city_locations_sf %>%
      filter(city_label == input$second_city)%>%
      pull(city_name)
    
    leaflet_city_circle(density,main_city,second_city)
  })
  
  output$second_city_density_circle <- renderLeaflet({
    
    main_city <- city_locations_sf %>%
      filter(city_label == input$main_city)%>%
      pull(city_name)
    
    second_city <- city_locations_sf %>%
      filter(city_label == input$second_city)%>%
      pull(city_name)
    
    leaflet_city_circle(density,second_city,main_city)
  })
  
  output$second_city_comparor <- renderLeaflet({
    
    main_city <- city_locations_sf %>%
      filter(city_label == input$main_city)%>%
      pull(city_name)
    
    second_city <- city_locations_sf %>%
      filter(city_label == input$second_city)%>%
      pull(city_name)
    
    leaflet_city_comparer(density_change,second_city,main_city)
    
    
  })
  
  
  
  
  
    output$lineplot <- renderGirafe({

      cities_of_interest <- city_locations_sf %>% filter(city_label %in% 
                                     c(input$main_city,
                                       input$second_city,
                                       input$third_city)) %>% 
        pull(city_name)
      
      dist_line_graph(cities_of_interest)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
