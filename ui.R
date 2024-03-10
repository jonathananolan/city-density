

# Define UI
ui <- fluidPage(
  navbarPage("How dense is your city?", theme = shinytheme("lumen"),
  tabPanel("Graph", fluid = TRUE,
    tags$style(button_color_css),
    # Sidebar layout with a input and output definitions
    sidebarLayout(
      sidebarPanel(
        # titlePanel("Cities:"),
         #shinythemes::themeSelector(),
         fluidRow(
           multi_city_selectorUI("cities_selection",choices_list,metrics), # Use the city selection module
          # tableOutput("dataTable")  # Add this line to display the table
            ) #CLOSE ROW
      ),#CLOSE SIDEBAR PANEL
    mainPanel(
     fluidRow(
       uiOutput("plotTitle"),   
       plotlyOutput("linePlot"),
       uiOutput("plotCaption"),   
       
     ),
     fluidRow(
        tags$div(style = "height: 30px;"),
        dist_sliderUI("distance_selection"), 
        metric_selectorUI("metric_selection",choices_list,metrics), # Use the city selection module
        website_link_UI(),   
        ) #CLOSE ROW
      ) #CLOSE MAIN PANEL 
    )# CLOSE SIDEBAR LAYOUT
   ),
  tabPanel("Map",fluid = TRUE,
           tags$style(button_color_css),
           # Sidebar layout with a input and output definitions
           sidebarLayout(
             sidebarPanel(
               titlePanel("test:"),
               city_selectorUI("city_selection",choices_list,metrics), # Use the city selection module
               map_type_selectorUI("map_type_selection")
             ),
             mainPanel(
               fluidRow(
                 uiOutput("frame")  # Placeholder for the dynamically generated iframe
               ) #CLOSE ROW
             ) #CLOSE MAIN PANEL 
           )
  )
) #CLOSE NAVBAR PAGE
) # CLOSE UI 

         
         
      