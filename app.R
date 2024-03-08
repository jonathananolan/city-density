options(shiny.autoload.r = FALSE)  # Prevent auto-loading R files from subfolders
renv::restore()

library(shiny)
library(ggplot2)
library(tidyverse)
library(sf)
library(leaflet)
library(glue)
library(qs)
library(data.table)
library(shinyWidgets)
library(plotly)
library(scales)
# Load the data reactively
data <-   qread("output/qs_files/shiny.qs") %>% data.table::setDT() 

cities <- unique(data$city_name)
options(scipen = 50)

choices_list <- data %>% 
  distinct(country,city_name)  %>%
  group_by(country) %>%
  summarise(city_name = list(sort(city_name)), .groups = 'drop') %>%
  deframe()  # Converts to a named list


#source("ggplot_theme.R")
metrics <- tribble(~col_name,~metric_type,~water,~cumulative,
                  "area_with_water",          "Area",                       "","",
                  "area_without_water",       "Area",                       "excluding large bodies of water","",
                  "population",               "Population",                 "","",
                  "pwd_with_water",           "Density",                    "population weighted","",
                  "density_without_water",    "Density",                    "excluding large bodies of water","",
                  "density_with_water" ,      "Density",                    "","",
                  "population_cum",           "Population",                 "","Cumulative",
                  "area_cum_without_water",   "Area",                       "excluding large bodies of water", "cumulative",
                  "pwd_cum_with_water" ,      "Density",                    "population weighted","cumulative",
                  "density_cum_with_water",   "Density",                    "","cumulative",
                  "density_cum_without_water","Density",                    "excluding large bodies of water","cumulative")%>%
  mutate(description = pmap_chr(list(metric_type, water, cumulative), function(...) {
    combined_values <- c(...)
    combined_values <- combined_values[combined_values != ""] # Remove empty values
    paste(combined_values, collapse = ", ")   # Concatenate with comma
      })) %>% 
  mutate(units = case_when(metric_type == "Density"~"Residents per square kilometer",
                           metric_type == "Area"~"Square kilometers",
                           metric_type == "Population"~"Residents")) %>% 
  filter(col_name %in% names(data)) 



# Define UI
ui <- fluidPage(
  titlePanel("How dense is your city? "),
  sidebarLayout(
    sidebarPanel(
      virtualSelectInput(
        inputId = "cities",
        selected = c("New York (United States)",
                     "Melbourne (Australia)"),
        label = "Cities:",
        choices = choices_list,
        showValueAsTags = TRUE,
        search = TRUE,
        multiple = TRUE,
        noOfDisplayValues = 6,
        maxValues = 6,
        keepAlwaysOpen = T
      ),
      selectInput("metric_type", "Metric type", 
                  choices = sort(unique(metrics$description)), 
                  selected = "Density, population weighted"),
      # tableOutput("dataTable")  # Add this line to display the table
      HTML('<div style="text-align: center;">Some cities centres are incorrectly coded to their region, so if you see a city with abnormally low population know that a fix is coming! <br><br>Population weighted density measures how dense an area feels for the typical person who lives there. <br><br><a href="https://jonathannolan.substack.com/p/04f5c9a0-9605-4cb9-8725-adc5d5785ec0" target="_blank">Visit<br><img src="logo.png" alt="Logo" style="width: auto; height: 60px; vertical-align: middle;"/> <br>jonathannolan.substack.com<br>for more info</a></div>')    ),
    mainPanel(
      plotlyOutput("linePlot"),
      tags$div(style = "height: 30px;"),  # This adds space between the plot and the map      leafletOutput("dynamicMap"),
      #leafletOutput("dynamicMap"),
      sliderInput("distSlider", "Maximum Distance",
                  min = 0, 
                  max = 100,  # Placeholder, set this to the max of your data
                  value = 30,  # Default to max value
                  step = 5,
                  ticks = TRUE),

    ),
  )
)

# Define server logic
server <- function(input, output, session) {
  options(shiny.autoload.r = FALSE)  # Prevent auto-loading R files from subfolders
  options(shiny.maxRequestSize = 900*1024^2)  # Set limit to 900MB
  lineplotRendered <- reactiveVal(FALSE)
  

  # output$dataTable <- renderTable({
  #   # Try to directly access and return 'map_data()' for debugging
  #   req(map_data())  # Ensure 'map_data()' is not NULL or empty
  #   
  #   # Assuming 'map_data()' now ensures a data frame is returned
  #   map_data_df <- map_data() %>% st_drop_geometry() %>% inner_join(filtered_data())
  #   
  #   # Ensure the object is a data frame before attempting to render it
  #   if(is.data.frame(map_data_df)) {
  #     map_data_df
  #   } else {
  #     # If not a data frame, attempt to convert or handle the error
  #     tryCatch({
  #       as.data.frame(map_data_df)
  #     }, error = function(e) {
  #       # Return an informative error if conversion fails
  #       data.frame(Error = "Failed to convert map data to a data frame")
  #     })
  #   }
  # })
  
  # Define 'filtered_data' as a reactive expression
  filtered_data <- reactive({
    req(input$cities, input$distSlider)  # Ensure necessary inputs are available
    lineplotRendered <- reactiveVal(FALSE)
    # Filter using data.table syntax
    data[city_name %in% input$cities & dist_km_round <= input$distSlider, ]
  })
  metric_column <- reactive({
    req(metrics)  # Ensure 'data' is available
    col_name <- metrics %>%
      filter(description %in% input$metric_type) %>% 
      pull(col_name)
  })
  
  metric_units <- reactive({
    req(metrics)  # Ensure 'data' is available
    col_name <- metrics %>%
      filter(description %in% input$metric_type) %>% 
      pull(units)
  })
  
    
 
  
  output$linePlot <- renderPlotly({
  req(metric_column,filtered_data)
    # Create the plot
    plot <- filtered_data() %>% 
      mutate(city_name = str_replace(city_name," \\(","\n(")) %>% 
           ggplot(
           aes(x = dist_km_round, 
               y = .data[[metric_column()]], 
               color = city_name,
               text=map(paste0('<b>',prettyNum(round(population_cum,-4),big.mark = ","),
                                    ' people live within ',dist_km_round,' km of ', city,".",'</b><br>',
                              'The population weighted density of this entire area is ', round(pwd_cum_with_water),' people per square km.','</b><br>',
                              prettyNum(round(population,-4),big.mark = ",")," live in the ring ",dist_km_round,"km out, and this ring has a population weighted density of ",round(pwd_with_water),"."), HTML),
               )
           ) +
      geom_line(size = 1.5) +
      labs(title = paste0(input$metric_type), 
                          x = "Distance from city centre (km)", 
                          y = metric_units(),
           colour = "City") +
      theme_jn_caption(plot_type = "line")+
      scale_y_continuous(labels = label_number_si())      
    
    lineplotRendered(TRUE)  # Indicate that the graph has been rendered
    plotly_object <- ggplotly(plot, tooltip = "text") %>%
                              config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", 
                                                         "hoverClosestCartesian", "hoverCompareCartesian", 
                                                         "zoom3d", "pan3d", "orbitRotation", "tableRotation", 
                                                         "resetCameraDefault3d", "resetCameraLastSave3d", 
                                                         "hoverClosest3d", 
                                                         "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", 
                                                         "toImage", 
                                                         "sendDataToCloud", "hoverClosestGl2d", 
                                                         "hoverClosestPie", "toggleHover", "resetViews", 
                                                         "toggleSpikelines", "resetViewMapbox"),
                                     scroll.zoom = FALSE,
                                     editSelection = FALSE,
                                     editable = FALSE,
                                     showAxisDragHandles = FALSE,
                                     showAxisRangeEntryBoxes = FALSE,
                                     dragmode = FALSE,
                              displaylogo = FALSE  
                              )%>%
      layout(dragmode = FALSE)%>%
      layout(margin = list(l = 50, r = 50, b = 100, t = 50),
             annotations = list(x = 1, y = -0.3, text = "Source: CityDensity.com",
                                xref='paper', yref='paper', showarrow = F, textfont = list(color = jn_colours$text[3]),
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font = list(size = 10)))
  })
  # This disables the default drag to zoom
    

 # data_cache <- reactiveValues()
  
  # Reactive expression to load and cache city data
#   map_data <- reactive({
#     
#     req(input$cities,lineplotRendered() == TRUE)  # Ensure there are selected cities
#     
#     # Loop through selected cities
#     lapply(input$cities, function(city_name) {
#       # Check if city data is already in the cache
#       if (!is.null(data_cache[[city_name]])) {
#         # Use cached data
#         data_cache[[city_name]]
#       } else {
#         # Construct the file path for the city's data file
#         file_path <- glue("qs_files/{city_name}.qs")
#         # Load the data if the file exists and cache it
#         if (file.exists(file_path)) {
#           data <- qs::qread(file_path)
#           data_cache[[city_name]] <- data  # Cache the loaded data
#           data
#         } else {
#           NULL  # Return NULL if the file does not exist
#         }
#       }
#     }) %>% purrr::compact() %>% bind_rows()  # Combine all data frames into one
#   }
# )
#   
# 
# 
#   
  
#   output$dynamicMap <- renderLeaflet({
# 
#     req(map_data, filtered_data, lineplotRendered() == TRUE)    # Assuming 'id' represents a unique identifier for each observation if needed
#     
#     # freezeReactiveValue(input, "cities") # change map after you change the graph. 
#     
#     map_data <-
#       map_data() %>%
#       inner_join(filtered_data(), by = join_by(city_name, dist_km_round)) %>%
#       select(any_of(c("city_name","dist_km_round",metric_column()))) %>%
#       pivot_longer(
#         cols = any_of(metric_column()),  # Pivot based on the selected metric
#         names_to = "metric",
#         values_to = "value"
#       )
# 
# 
#     pal <- colorNumeric(palette = "viridis", domain = map_data$value)
# 
#     map_data %>%
#     leaflet() %>%
#       addTiles() %>%
#       addPolygons(
#         fillColor = ~pal(value),
#         color = "white",
#         weight = .1,
#         opacity = .5,
#         fillOpacity = 0.7
#         #popup = ~paste(metric_column(), ":", data$value)  # Correct context for .data
#       ) %>%
#       addLegend(pal = pal, values = ~value, opacity = 1,
#                 title = paste0(input$metric_type), 
#                 position = "bottomright",
#                 label = ) %>% 
#       setView(lng = -73.935242, lat = 40.730610, zoom = 9)
#     # session$onFlushed(function() {
#     #   unfreezeReactiveValue(input, "cities")
#     # }, once = TRUE)
#   }
# )
  
}



# Run the app
shinyApp(ui, server)
