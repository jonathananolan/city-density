

# Define server logic
server <- function(input, output, session) {
  
  selectedCities <- citiesSelectionServer("cities_selection") # Call the module server
  selectedMetric <- metricSelectionServer("metric_selection") # Call the module server
  selectedDist   <- distSelectionServer("distance_selection") # Call the module server
  selectedCity   <- citySelectionServer("city_selection") # Call the module server
  selectedMapType<- map_type_Server("map_type_selection") # Call the module server
  
  
 options(shiny.maxRequestSize = 900*1024^2)  # Set limit to 900MB
  lineplotRendered <- reactiveVal(FALSE)

  # Define 'filtered_data' as a reactive expression
  filtered_data <- reactive({
    req(selectedCities(), selectedDist())  # Ensure necessary inputs are available
    lineplotRendered <- reactiveVal(FALSE)
    # Filter using data.table syntax
    cities_data[city_name %in% selectedCities() & dist_km_round <= selectedDist(), ] 
  }) %>%
    bindCache(selectedDist(), selectedCities())
  
  metric_column <- reactive({
    req(metrics)  # Ensure 'data' is available
    col_name <- metrics %>%
      filter(description %in% selectedMetric()) %>% 
      pull(col_name)
  }) %>%
    bindCache(selectedMetric())
  
  metric_units <- reactive({
    req(metrics)  # Ensure 'data' is available
    col_name <- metrics %>%
      filter(description %in% selectedMetric()) %>% 
      pull(units)
  }) %>%
    bindCache(selectedMetric())
  
  
  ###### MAIN PLOT ######
  output$linePlot <- renderPlotly({
    req(metric_column,filtered_data)
    # Create the plot
    plot <- filtered_data() %>% 
      mutate(city_name = str_replace(city_name," \\(","\n(")) %>% 
      ggplot(
        aes(x = dist_km_round, 
            y = .data[[metric_column()]], 
            color = city_name,
            text=map(paste0('<b>',format(round(population_cum,-4),big.mark = ","),
                            ' people live within ',dist_km_round,' km of ', city,".",'</b><br>',
                            'The population weighted density of this entire area is ', format(round(pwd_cum_with_water),big.mark = ","),' people per square km.','</b><br>',
                            format(round(population,-4),big.mark = ",")," live in the ring ",dist_km_round,"km out, and this ring has a population weighted density of ",format(round(pwd_with_water),big.mark = ','),"."), HTML),
        )
      ) +
      geom_line(size = 1.5) +
      labs(title = paste0(selectedMetric()), 
           x = "Distance from city centre (km)", 
           y = metric_units(),
           colour = "City") +
      theme_jn_caption(plot_type = "line")+
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))      
    
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
  }) %>%
    bindCache(selectedDist(), 
              selectedCities(),
              selectedMetric())
  
  ###### MAPS ######

  # Construct the iframe URL based on selections
  output$frame <- renderUI({
    req(selectedCity(), selectedMapType())  # Ensure both selections are made
    
    # Lookup geoname_id for the selected city
    geoname_id <- cities_lookup[city_name == selectedCity(), geoname_id]
    
    # Determine the file name based on the selected map type
    file_name <- ifelse(selectedMapType() == "Rings around the city", "circle", "square")
    
    # Construct the URL
    url <- paste0("https://city-density.s3.amazonaws.com/leaflet_maps/", file_name, "_", geoname_id, ".html")
    
    # Return the iframe HTML tag with the constructed URL
    tags$iframe(src = url, height = 600, width = "100%", style = "border: none;")
  })

}

