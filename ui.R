
# Define UI
ui <- fluidPage(
  tags$head(includeHTML("input_data/google_tag.html")),
  navbarPage("CityDensity.com", theme = shinytheme("lumen"),
  tabPanel("Graph", fluid = TRUE,
    tags$style(button_color_css),
    # Sidebar layout with a input and output definitions
    sidebarLayout(
      sidebarPanel(
        # titlePanel("Cities:"),
         #shinythemes::themeSelector(),
         fluidRow(
           multi_city_selectorUI("cities_selection",choices_list,metrics) # Use the city selection module
          # tableOutput("dataTable")  # Add this line to display the table
            ) #CLOSE ROW
      ),#CLOSE SIDEBAR PANEL
    mainPanel(
     fluidRow(
       uiOutput("plotTitle"),   
       plotlyOutput("linePlot"),
       uiOutput("plotCaption")
       
     ),
     fluidRow(
        tags$div(style = "height: 30px;"),
        pwd_info_UI(),   
        dist_sliderUI("distance_selection"), 
        metric_selectorUI("metric_selection",choices_list,metrics), # Use the city selection module
        website_link_UI()
        ) #CLOSE ROW
      ) #CLOSE MAIN PANEL 
    )# CLOSE SIDEBAR LAYOUT
   ),
  tabPanel("Map",fluid = TRUE,
           tags$style(button_color_css),
           # Sidebar layout with a input and output definitions
           sidebarLayout(
             sidebarPanel(
               city_selectorUI("city_selection",choices_list,metrics), # Use the city selection module
               map_type_selectorUI("map_type_selection")
             ),
             mainPanel(
               fluidRow(
                 uiOutput("frame")  # Placeholder for the dynamically generated iframe
               ) #CLOSE ROW
             ) #CLOSE MAIN PANEL 
           )
  ),
  tabPanel("Rankings", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
      countrySelectorUI("country_selection"),
      metric_selector_rankUI("metric_selection_rank",c("Population (with water)","Density (without water)")), # Use the city selection module
      rankdist_sliderUI("distance_selection_ranks")
      ),
      mainPanel(
        uiOutput("rankPlotTitle"),   
        plotOutput("rank_plot",height = "700px"),
        uiOutput("rankPlotCaption")   
      )
    )
  ),
  tabPanel("About the data",fluid = TRUE,
           tags$style(button_color_css),
           error_info_UI(), 
           # Sidebar layout with a input and output definitions
           sidebarLayout(
             sidebarPanel(
               city_selectorUI("city_selection_error",choices_list,metrics) # Use the city selection module
             ),
             mainPanel(
               fluidRow(
                 leafletOutput('map_for_errors')               ),
               # Displaying existing and new information
               fluidRow(
                 column(6,
                        h4("Existing Information"),
                        uiOutput("existing_info_display")
                 ),
                 column(6,
                        h4("New Information"),
                        uiOutput("new_lon_lat"),
                        uiOutput("dynamic_new_source_input"),
                        uiOutput("notes"),
                        uiOutput("dynamic_submit_button"),
                        version_ui()
                 )
               )
             ) #CLOSE MAIN PANEL 
           )
  )
) #CLOSE NAVBAR PAGE
) # CLOSE UI 

         
         
      