

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
  
  
  
  output$plotTitle <- renderUI({
  
    title_start <- metrics %>%
      filter(description %in% selectedMetric()) %>% 
      pull(title)
    
    units_subtitle <- metrics %>%
      filter(description %in% selectedMetric()) %>% 
      pull(subtitle)
      
      
    cities_alpha <- sort(selectedCities())
    create_one_city <- function(i){
      code <- paste0("<span style='color:", jn_colours$complementary[i], ";'><strong>", cities_alpha[i], "</strong></span>")
      return(code)
    }
    
    # Determine how to collapse the list of cities based on their count
    city_codes <- map_chr(seq_along(cities_alpha), create_one_city)
    if (length(city_codes) > 1) {
      city_codes <- c(paste(head(city_codes, -1), collapse = ", "), "and", tail(city_codes, 1))
    }
    full_title <- paste0("<strong>", title_start, " ", paste(city_codes, collapse = " "), ".</strong><br>",units_subtitle)
    
    HTML(full_title)
  }) %>%
    bindCache(selectedCities(),
              selectedMetric())
  
  output$plotCaption <- renderUI({
    # Check if any of the selected cities include 'Australia'
    australia_caption <- if (any(sapply(selectedCities(), str_detect, pattern = "Australia"))) {
      " and ABS (2022) for Australian data."
    } else {
      "."
    }
    
    # Create the caption with HTML and use CSS for right alignment
    caption_text <- paste0("Source: CityDensity.com<br> Data from Global Human Settlement Layer (2020)", australia_caption)
    HTML(paste0('<div style="text-align: right;">', caption_text, '</div>'))
  })
  

  
  ###### MAIN PLOT ######
  output$linePlot <- renderPlotly({
    req(metric_column,filtered_data)
    
    print("This plot is being generated now.")
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
      geom_line(linewidth = 1.5) +
      labs(x = "Distance from city centre (km)", 
           y = metric_units()) +
      theme_jn_caption(plot_type = "line")+
      theme(legend.position = "none")+
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))      
    
    lineplotRendered(TRUE)  # Indicate that the graph has been rendered
    plotly_object <- ggplotly(plot, 
                              tooltip = "text") %>%
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
             editable = FALSE,
             showAxisDragHandles = FALSE,
             showAxisRangeEntryBoxes = FALSE,
             displaylogo = FALSE  
      )%>%
      layout(dragmode = FALSE)%>%
      layout(font = list(family = "-apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen', 'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue', sans-serif"))
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

