#Main graph page

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
      keepAlwaysOpen = TRUE
    )
  )
}

# Define the server logic for the city selection module
citiesSelectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
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
    return(reactive(input$metric_type))
  })
}




pwd_info_UI <- function() {
    HTML('<div style="text-align: left;">Population weighted density measures how dense 
         an area feels for the typical person who lives there.<br><br> </div>')
}


website_link_UI <- function() {
    HTML('<div style="text-align: center;"> <a href="https://city-density.s3.amazonaws.com/city-density.csv">Download the data<br<br></a><br>
         <a href="https://jonathannolan.substack.com/p/04f5c9a0-9605-4cb9-8725-adc5d5785ec0" 
target="_blank">Visit<br><img src="logo.png" alt="Logo" style="width: 
         auto; height: 60px; vertical-align: middle;"/> <br>jonathannolan.substack.com<br>for 
more info</a></div>')}


## MAP PAGE


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


## CITY RANKS

metric_selector_rankUI <- function(id,choices_list) {
  ns <- NS(id)
  tagList(
    selectInput(ns("metric_type"), "Metric type",
                choices = choices_list, # Ensure `metrics` is accessible
                selected = "Population (with water)")
  )
}




rankMetricSelectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Use a reactive expression to return both the selected data and the metric type
    selected_info <- reactive({
      # Determine which dataset to return based on the input
      data <- if(input$metric_type == "Population (with water)") {
        pop_cum
      } else {
        density_cum
      }
      
      # Return a list that includes both the data and the metric type
      list(data = data, metric_type = input$metric_type)
    })
    
    return(selected_info)
  })
}

rankdist_sliderUI <- function(id) {
  ns <- NS(id)
  sliderInput(ns("distSliderRank"), "Distance from center",  # Ensure the input ID is namespaced
              min = 5, 
              max = 100,  # Placeholder, set this to the max of your data
              value = 30,  # Default to max value
              step = 5,
             # playButton = T,
              #animate = T,
              ticks = TRUE)
}



# Define the server logic for the city selection module
distSliderSelectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactiveValue <- reactive({
      input$distSliderRank
    })
    return(reactiveValue)
  })
}








##ERROR SHEET


error_info_UI <- function() {
  HTML('<strong>About the data</strong><br>
         The team at <a href="https://ghsl.jrc.ec.europa.eu/ghs_pop.php" rel="nofollow ugc noopener">Global Human Settlement Layer</a>
         made the map of 1km squares, with data collated from around the world by 
         <a href="https://sedac.ciesin.columbia.edu/" rel="nofollow ugc noopener">NASA</a>. 
         Most of the populations are projected forward to 2020 from national censuses. Many of the original data is from around 
         <a href="https://sedac.ciesin.columbia.edu/downloads/docs/gpw-v4/gpw-v4-country-level-summary-rev11.xlsx" rel="nofollow ugc noopener">2010</a>
         , and so is starting to lose accuracy in fast growing areas. NASA are updating their data in Spring of 2024, and hopefully GHSL will not be far behind. 
         For Australia I’ve overcome this by using the ABS’s 2022 
         <a href="https://www.abs.gov.au/statistics/people/population/regional-population/latest-release#interactive-maps" rel="nofollow ugc noopener">km grid</a>
         , which is far more accurate for Melbourne and Sydney’s fast growing outer suburbs. </p><div class="subscription-widget-wrap"><div class="subscription-widget show-subscribe">
       For more information about the data, read my <a href="https://jonathannolan.substack.com/p/how-dense-is-your-city" 
         target="_blank">substack post.</a><br><br>
       <strong>Report an error</strong><br>
       What appears to be wrong? Have a look at the map and try and figure out what the source of the error is. 
       
       <ul>
        <li> Do some squares in the 1km square map have more or fewer people than you expect? If so, we\'ll have to wait for a new GHSL to fix it.</li>
       <li> Does a city seem to be in the wrong place? That can be fixed. Often there is no official record of where the center of a city is, but locals tend to know. If you would like me to move a city-center then please enter in the city below. 
       Please also check the city\'s <a href ="https://www.geonames.org/">geoname ID (it\'s in the URL when you fid the right city on geonames.org)</a> and provide a correct one if necessary. 
       If several votes are received then I\'ll update the center.
       </ul><br>
       If you have any other errors to report you can create an issue on <a href ="https://github.com/jonathananolan/city-density">GitHub</a>. <br><br>
       <strong>Request a city</strong><br>
       To request a city please type "new city" in the search box below, find the center of the city and the <a href ="https://www.geonames.org/">geoname ID (it\'s in the URL when you fid the right city on geonames.org)</a>. 
       <br><br>
       <strong>Feature requests</strong><br>
       Create an issue on <a href ="https://github.com/jonathananolan/city-density">GitHub</a>. 
       <br><br>
       
       
       
       '
)
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
    #  print(input$distSlider)  # Debugging: print the current value of the distance slider
      input$distSlider
    })
    return(reactiveValue)
  })
}


city_selectorUI <- function(id,choices_list,metrics) {
  ns <- NS(id)
  
  choices_with_new <- choices_list
  choices_with_new$new_city <- c("Request a new city")
  tagList(
    virtualSelectInput(
      inputId = ns("city_single"),
      selected = c("Melbourne (Australia)"),
      label = "Enter city:",
      choices = choices_with_new, # Make sure this is defined or passed to the module
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


countrySelectorUI <- function(id,choices_list,metrics) {
  ns <- NS(id)
  
  choices <- countries_list
  tagList(
    virtualSelectInput(
      inputId = ns("country"),
      label = "Enter country:",
      choices = choices, # Make sure this is defined or passed to the module
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = TRUE,
      noOfDisplayValues = 6,
      maxValues = 6,
      keepAlwaysOpen = TRUE
    )
  )
}

# Define the server logic for the city selection module
countrySelectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # For demonstration, simply return the reactive expression of selected cities
    return(reactive(input$country))
  })
}


version_ui <- function() {
  HTML('<br><br><span style="color: lightgrey;">Version 20241011.1</span>')

}
