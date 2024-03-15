

# Define server logic
server <- function(input, output, session) {
  
  session$allowReconnect(TRUE) 
  
  selectedCities <- citiesSelectionServer("cities_selection") # Call the module server
  selectedMetric <- metricSelectionServer("metric_selection") # Call the module server
  selectedDist   <- distSelectionServer("distance_selection") # Call the module server
  selectedCity   <- citySelectionServer("city_selection") # Call the module server
  selectedMapType<- map_type_Server("map_type_selection") # Call the module server
  selectedCity_error <- citySelectionServer("city_selection_error") # Call the module server
  selectedMetric_rankings <- metricSelectionServer("metric_selection") # Call the module server
  selectedRankMetric <- rankMetricSelectionServer("metric_selection_rank")
  selectedRankDist   <- distSliderSelectionServer("distance_selection_ranks") # Call the module server
  
 

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
  
  
  
  
  
  
  selectedCityInfo <- reactive({
    city_lat_lons %>% 
      filter(city_name == selectedCity_error()) 
  })
  
  output$map_for_errors <- renderLeaflet({
    require(selectedCity)
    city_ltln <-selectedCityInfo()
    
    leaflet()%>%
      addTiles() %>% 
      addMarkers(lng = city_ltln$lon, lat = city_ltln$lat)})
  
  observe({
    click <- input$map_for_errors_click
    if (!is.null(click)) {
      city_ltln <-selectedCityInfo()
      
      leafletProxy('map_for_errors') %>%
        clearMarkers() %>% # Clear existing markers
        addMarkers(lng = click$lng, lat = click$lat) %>% 
        addMarkers(lng = city_ltln$lon, lat = city_ltln$lat)
    }
  }) %>% 
    bindEvent(input$map_for_errors_click)
  
  lastClickCoordinates <- reactiveVal(NULL)
  
  observe({
    click <- input$map_for_errors_click
    if (!is.null(click)) {
      # Update last click coordinates
      lastClickCoordinates(list(lon = click$lng, lat = click$lat))
      
      # Your existing code to update the map goes here...
    }
  }) %>% 
    bindEvent(input$map_for_errors_click)
  
  output$new_lon_lat <- renderUI({
    if (!is.null(input$map_for_errors_click)) {
      coords <- input$map_for_errors_click
      HTML(paste('<strong>New lon/lat</strong><br>',
                 coords$lng, ',', coords$lat,"<br><br>"))
    } else {
      HTML("<strong>Click on the map to set new coordinates.</strong><br>")
    }
  })
  
  
  
  # Output for the leaflet map showing city location
  output$map_for_errors <- renderLeaflet({
    city_info <- selectedCityInfo()
    if (nrow(city_info) > 0) {
      leaflet() %>%
        addTiles() %>%
        addMarkers(lng = city_info$lon, lat = city_info$lat)
    }
  })
  output$existing_info_display <- renderUI({
    city_info <- selectedCityInfo()
    if (nrow(city_info) > 0) {
      HTML(paste0('<strong>Existing source of lat/lon</strong><br>',
                  city_info$source_of_lat_lon, # Display the source of lat/lon
                  '<br><br><strong>Existing lon/lat</strong><br>',
                  city_info$lon, ",", city_info$lat, # Display the longitude and latitude
                  '<br><br><strong>Existing geonames id of city</strong><br>',
                  '<a href="https://www.geonames.org/', city_info$geoname_id, '">', city_info$geoname_id, '</a>')) # Corrected the <a> tag
    } else {
      HTML('Select a city') # Displayed when no city is selected
    }
  })
  
  
  # Display Geonames.org link
  output$geo_link <- renderUI({
    city_info <- selectedCityInfo()
    if (nrow(city_info) > 0) {
      geoname_id <- city_info$geoname_id
      href <- paste0("https://www.geonames.org/", geoname_id)
      tags$a(href = href, as.character(geoname_id), target = "_blank")
    }
  })
  
  output$dynamic_new_source_input <- renderUI({
    city_info <- selectedCityInfo()
    if (nrow(city_info) > 0) {
      # Extract the existing geoname_id for the selected city
      existing_geoname_id <- city_info$geoname_id
      
      # Create a textInput with existing geoname_id as default value
      tagList(HTML('<strong>New geonames id</strong><br>'),
      textInput("new_source", "(please check if existing is correct)", value = as.character(existing_geoname_id))
      )
    } else {
      # Fallback in case no city is selected or city_info is empty
      textInput("new_source", "New geonames id (please check if existing is correct)", value = "")
    }
  })
  
  output$notes <- renderUI({
    tagList(
    textInput("notes", "Enter any other notes", value = ""),
    HTML('<strong>Enter Email</strong>'),
    textInput("email", "Enter your email if you would like to be let known when I update it", value = "")
    )

  })
  
  # Reactive value to track the button label
  buttonLabel <- reactiveVal("Submit")
  
  observeEvent(input$submit_button, {
    
    # Assuming you have these inputs in your app
    city_info <- selectedCityInfo()
    new_geoname_id <- input$new_source
    notes <- input$notes
    
    new_geoname_id <- ifelse(is.null(input$new_source), "NA", input$new_source)
    new_long <- ifelse(is.null(input$map_for_errors_click$lng), "NA", input$map_for_errors_click$lng)
    new_lat <- ifelse(is.null(input$map_for_errors_click$lat), "NA", input$map_for_errors_click$lat)
    notes <- ifelse(is.null(input$notes), "NA", input$notes)
    email <- ifelse(is.null(input$email), "NA", input$email)

    # Check if city_info has data to prevent errors
    if (nrow(city_info) > 0) {
      data_to_write <- data.frame(
        ExistingGeonameID = city_info$geoname_id,
        Existinglat= city_info$lon,
        Existinglon = city_info$lat,
        NewGeonameID = new_geoname_id,
        new_long = new_long,
        new_lat = new_lat,
        Notes = notes,
        email = email,
        stringsAsFactors = FALSE  # Avoid factors to ensure consistent data types
      )
      
      # Path to the CSV file
      csv_file_path <- "data/requests.csv"
      
      # Check if the file exists, if not create it and add a header row
      if (!file.exists(csv_file_path)) {
        write_csv(data_to_write, csv_file_path)
      } else {
        # Append the data to the existing CSV file without header
        write_csv(data_to_write, csv_file_path, append = T)
      }
      
      buttonLabel("Finished! Thanks") # Change the label after the button is clicked
      
    }
  })
  
 
  
  # Render the action button dynamically with the current label
  output$dynamic_submit_button <- renderUI({
    actionButton("submit_button", label = buttonLabel())
  })
  
  
  # output$tbl = renderDT({
  #   
  #   rank_data_list <- selectedRankMetric()
  #   
  #   rank_level <- selectedRankDist()
  #   
  #   rank_data_list[[rank_level]]
  # })
  # 
  output$rank_plot = renderPlot({
    
    rank_data_list <- selectedRankMetric()
    rank_level <- selectedRankDist()
    plot_data <- rank_data_list$data[[rank_level]]
    
    if(rank_data_list$metric_type == "Population (with water)") {
      plot_y = "Population"
    } else {
      plot_y = "People per square km of land"
    }

    
    print(plot_data)
    plot_data%>%  
      ggplot(
        aes(x = city, 
            y = value,
            fill = region))+
      geom_bar(stat = "identity")+
      coord_flip() +
      theme_jn_caption() +
      scale_fill_manual(values = c("South Asia" = jn_colours$complementary[7],
                                  "East Asia & Pacific" = jn_colours$complementary[1],
                                  "Middle East & North Africa"= jn_colours$complementary[3],
                                  "Sub-Saharan Africa"= jn_colours$complementary[4],
                                  "Europe & Central Asia"= jn_colours$complementary[5],
                                  "Latin America & Caribbean"= jn_colours$complementary[6],
                                  "North America"= jn_colours$complementary[2]
      ))+
      labs(x        = element_blank(),
           y        = plot_y,
           fill = "Region"
           )+
      #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      #geom_text(aes(label = city,    y = -max(value)/2.5,   x = order), hjust = 0) +
      #geom_text(aes(label = country, y = -max(value)/100, x = order), hjust = 1) +
      ggflags::geom_flag(aes(y = -max(plot_data$value)/20, country = country_code_iso2c))+
      scale_y_continuous(labels = label_number(scale_cut = cut_si(""))) +
      theme(text = element_text(family = "sans", size = 15, colour = "#333333"), # Base font for all text
            plot.title = element_text(face = "bold", colour = "#333333"), # If you want the title bold
            axis.title = element_text(size = 15, face = "bold", colour = "#333333"), # Specific size for axis titles
            plot.subtitle = element_text(size = 15, colour = "#333333"), # Specific size for axis titles
            
            axis.text = element_text(size = 15, colour = "#333333"),
            axis.title.x = element_text(size = 16, colour = "#333333",face = "plain"),
            
            legend.position = "bottom"
            
            ) # Specific size for axis text
  } 
)%>%
    bindCache(selectedRankDist(), 
              selectedRankMetric())
  
  
  output$rankPlotTitle <- renderUI({
    
    
    rank_data_list <- selectedRankMetric()
    rank_level <- selectedRankDist()
    plot_data <- rank_data_list$data[[rank_level]]
    
    if(rank_data_list$metric_type == "Population (with water)") {
      
      plot_title    = "The biggest global cities" 
      plot_subtitle = paste0("Number of people who live within ",rank_level,"km of the center")
    } else {
      plot_title    = "The most dense global cities" 
      plot_subtitle = paste0("Population density of land within ",rank_level,"km of the center\n(excludes water)")
    }

    full_title <- paste0("<strong>", plot_title,"</strong><br>",plot_subtitle)
    
    HTML(full_title)
  }) %>%
    bindCache(selectedRankDist(), 
              selectedRankMetric())
  
  output$rankPlotCaption <- renderUI({
    
      HTML(paste0('<div style="text-align: right;">Source: CityDensity.com<br>Cities closer than ',2* selectedRankDist(),'km from a bigger city excluded from rankings.'))
    })%>%
      bindCache(selectedRankDist())
  
  
}

