multi_city_selectorUI <- function(id,choices_list,metrics) {
  ns <- NS(id)
  tagList(
    virtualSelectInput(
      inputId = ns("cities"),
      selected = c("Melbourne (Australia)","New York City (United States)"),
      label = "Enter cities:",
      choices = choices_list, # Make sure this is defined or passed to the module
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = TRUE,
      noOfDisplayValues = 6,
      maxValues = 6,
      keepAlwaysOpen = FALSE
    )
  )
}

# Define the server logic for the city selection module
citiesSelectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # For demonstration, simply return the reactive expression of selected cities
    return(reactive(input$cities))
  })
}


metric_selectorUI <- function(id,choices_list,metrics) {
  ns <- NS(id)
  tagList(
    selectInput(ns("metric_type"), "Metric type",
                choices = sort(unique(metrics$description)), # Ensure `metrics` is accessible
                selected = "Density, population weighted")
  )
}

# Define the server logic for the city selection module
metricSelectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # For demonstration, simply return the reactive expression of selected cities
    return(reactive(input$metric_type))
  })
}



# Define the server logic for the city selection module
citySelectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactive(input$cities))
  })
}



website_link_UI <- function() {
    HTML('<div style="text-align: center;">Some cities centres are incorrectly coded to 
         their region, so if you see a city with abnormally low population know that 
         a fix is coming! <br><br>Population weighted density measures how dense 
         an area feels for the typical person who lives there. <br><br>
         <a href="https://jonathannolan.substack.com/p/04f5c9a0-9605-4cb9-8725-adc5d5785ec0" 
         target="_blank">Visit<br><img src="logo.png" alt="Logo" style="width: 
         auto; height: 60px; vertical-align: middle;"/> <br>jonathannolan.substack.com<br>for 
         more info</a></div>')
}


dist_sliderUI <- function(id) {
  ns <- NS(id)
  sliderInput(ns("distSlider"), "Maximum Distance",  # Ensure the input ID is namespaced
              min = 0, 
              max = 100,  # Placeholder, set this to the max of your data
              value = 30,  # Default to max value
              step = 10,
              ticks = TRUE)
}


# Define the server logic for the city selection module
distSelectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactiveValue <- reactive({
      print(input$distSlider)  # Debugging: print the current value of the distance slider
      input$distSlider
    })
    return(reactiveValue)
  })
}


city_selectorUI <- function(id,choices_list,metrics) {
  ns <- NS(id)
  tagList(
    virtualSelectInput(
      inputId = ns("city_single"),
      #selected = c("Melbourne (Australia)","New York City (United States)"),
      label = "Enter city:",
      choices = choices_list, # Make sure this is defined or passed to the module
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = FALSE,
      noOfDisplayValues = 6,
      maxValues = 1,
      keepAlwaysOpen = TRUE
    )
  )
}

# Define the server logic for the city selection module
citySelectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # For demonstration, simply return the reactive expression of selected cities
    return(reactive(input$city_single))
  })
}


map_type_selectorUI <- function(id,choices_list,metrics) {
  ns <- NS(id)
  tagList(
    selectInput(ns("map_type"), "Choose a map:",
                choices = c("Rings around the city","1km squares (slow on mobile)"), # Ensure `metrics` is accessible
                selected = "Rings around the city")
  )
}

# Define the server logic for the city selection module

map_type_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # For demonstration, simply return the reactive expression of selected cities
    return(reactive(input$map_type))
  })
}


