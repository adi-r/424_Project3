setwd("C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project3")
print(getwd())

# LIBRARIES=======================================================================================================
library(lubridate)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyr)
library(scales)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(stringr)
library(shinyjs)

options(scipen=999)


# READ DATA=======================================================================================================
df <- do.call(rbind, lapply(list.files(pattern = "*.csv"), read.csv))
df <- df[, -1]

# Set date column in Date format
str(df)



#setting initial leaflet map  
map <- leaflet(options= leafletOptions(preferCanvas = T)) %>%
  addTiles() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", group = "Default") %>%
  addProviderTiles("OpenRailwayMap", group = "CTA Lines") %>%
  addProviderTiles("CartoDB.Positron", group = "Minimal") %>%
  #Resettable map
  addResetMapButton() %>%
  #Choice for background
  addLayersControl(
    baseGroups = c("Default", "HeatMap"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomright"
  )

#Credits for below code snippet: https://stackoverflow.com/questions/70288989/programatically-trigger-marker-mouse-click-event-in-r-leaflet-for-shiny

# create js function that triggers a click on a marker selected by station name
jsCode <- 'shinyjs.markerClick = function(id) {
              map.eachLayer(function (layer) {
                if (layer.options.layerId == id) {
                  layer.fire("click");
                }
              })
           };'


# UI==============================================================================================================
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode, functions = c('markerClick')),
  titlePanel("CS424 Project-3"),
  fluidRow(
    #height = "100%",
    #column-1 for about and controls 
    column( width = 2,
            wellPanel(
              #About goes here
              HTML(" <h1><b>About</b></h1> 
             <h2>Developed as part of Project 3 for CS424 (Visualization and Visual Analytics) - UIC Spring 2022</h2>
             <h2><b>Authors:</b> Aditya Ranganathan </h2>
             <h2> <b>Created April 15th, 2022</b></h2>,
             <h3> This App presents the Chicago CTA Ridership data and Lat/Long data obtained from the Chicago Data Portal website </h3>
             <h3> <b> *Data Sources: </b></h3>
             <h3><a href=\"https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f\">link L-Ridership data</a> </h3>
          <h3>  <a href=\"https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme\">link L-System_Information data</a> </h3>
          </br>"),
              
              style = "height:85vh;", 
              fluidRow(style = "margin-top:15%",
                       radioButtons("comm_areas", "Select data",
                                    c("Include Community" = "comm_incl","Exclude Community" = "comm_excl"),
                                    inline = FALSE)
              ),
              
              #date picker for single date
              fluidRow(
                dateInput("date", label="Single Dates", value = "2021-08-23",
                          min="2001-01-01", max="2021-11-30", format = "yyyy/mm/dd")
              ),
              
              #date range for comparison
              fluidRow(dateRangeInput("date1", label="Compare Dates", start = "2021-08-23", end = "2015-08-23",  min="2001-01-01", max="2021-11-30", format = "yyyy/mm/dd",
                                      separator = "and")),
              #next and prev buttons
              fluidRow(
                column(6,
                       actionButton(inputId = "prevButton", label = "Prev")),
                column(6,
                       actionButton(inputId = "nextButton", label = "Next"))
              ),
              HTML("<br>"),
              fluidRow(
                selectInput("sortby", "Bar Plot View", choices = c("Alphabetical" = "alpha", "Ascending" = 'asc', "Descending" = "desc")),
              ),
              
              fluidRow(
                selectizeInput('select_station', "Select Station", choices = stations,
                               selected = "UIC-Halsted")
              ),
              
              fluidRow(
                selectInput('switch', "Graph Time Period", choices = c("Daily", "Weekly", "Monthly","Yearly"),
                            selected = "Daily")
              ),
              
              fluidRow(
                selectInput("year", "Year",
                            choices = c("All", 2021:2001),
                            selected = c(2021)
                )
                
              )
            )#Wellpanel1
    ), #Column-1
    
    #Rest of the stuff
    column(width = 10,
           
           #Bar plot column 
           column(width = 4,
                  fluidRow( style = "height:85vh;", plotlyOutput(height = "100%", "bar_graph"))
           ),
           
           #Line data Table and map column
           column( width = 4,
                   fluidRow(style = "height:15vh;", uiOutput(height = "85%", style ="width: 50%;","main_table")),
                   fluidRow(style = "margin-top:300px; height:60vh;",leafletOutput(height = "90%","map_dash"))
           ),
           
           #Yearly graph column 
           column( width = 4,
                   #The yearly graph for station goes here 
                   fluidRow(uiOutput("plot_and_table"))
           )
           
    )
    
    
    
  )
)

# ui <- dashboardPage(skin = "black",
#                     dashboardHeader(title = "CS424 Project-2"),
#                     dashboardSidebar(collapsed = FALSE, disable = FALSE,
#                                      sidebarMenu(
#                                        id = "menu_tabs",
#                                        tags$div(style = "margin-top: 300px;"),
#                                        menuItem("Dashboard", tabName = "map_dash", selected = TRUE, icon = icon("dashboard")),
#                                        menuItem("About", tabName = "about", icon = icon("sunglasses", lib = "glyphicon"))
#                                        
#                                      )
#                     ),
#                     dashboardBody(
#                       #using shinyjs to disable/enable inputs
#                      
#                       #tags$head(tags$style(".sidebar-menu li { margin-bottom: 20px; }")),
#                       tabItems(
#                         tabItem(tabName = "map_dash", 
#                                 fluidPage(
#                                   sidebarLayout(position = "left",
#                                                 sidebarPanel(style = "margin-top: 70%",
#                                                              width = 2,
#                                                              #Radio buttons
#                                                              fluidRow(
#                                                                column(8,
#                                                                       radioButtons("radio_single", "Select mode",
#                                                                                    c("Single Date" = "single",
#                                                                                      "Comparison" = "compare"),
#                                                                                    inline = FALSE)
#                                                                       )
#                                                                ),
#                                                              #date picker for single date
#                                                              fluidRow(
#                                                                dateInput("date", label="Single Dates", value = "2021-08-23",
#                                                                        min="2001-01-01", max="2021-11-30", format = "yyyy/mm/dd")
#                                                              ),
#                                                              #date range for comparison
#                                                              fluidRow(dateRangeInput("date1", label="Compare Dates", start = "2021-08-23", end = "2001-08-23",  min="2001-01-01", max="2021-11-30", format = "yyyy/mm/dd",
#                                                                             separator = "and")),
#                                                              #next and prev buttons
#                                                              fluidRow(
#                                                                column(6,
#                                                                       actionButton(inputId = "prevButton", label = "Prev")),
#                                                                column(6,
#                                                                       actionButton(inputId = "nextButton", label = "Next"))
#                                                              ),
#                                                              HTML("<br>"),
#                                                              div(selectizeInput('select_station', "Select Station", choices = stations,
#                                                                                selected = "UIC-Halsted")
#                                                              
#                                                              ),
#                                                              HTML("<br>"),
#                                                              div(
#                                                                fluidRow(
#                                                                  selectInput("sortby", "Bar Plot View", choices = c("Alphabetical" = "alpha", "Ascending" = 'asc', "Descending" = "desc")),
#                                                                )
#                                                              ),
#                                                              fluidRow(column(8,
#                                                                              div(selectInput("year", "Year",
#                                                                                              choices = c("All", 2021:2001),
#                                                                                              selected = c(2021)
#                                                                              )
#                                                                              )
#                                                              )
#                                                              )
#                                                             
#                                                            
#                                               ),
#                                               
#                                               mainPanel(
#                                                 
#                                                 # fluidRow(
#                                                 #   column(12,
#                                                 #          leafletOutput("map_dash")),
#                                                 #   column(12,
#                                                 #          uiOutput("bar_graph"),),
#                                                 #   column(12,dddfd
#                                                 #          uiOutput("plot_and_table"))
#                                                 # )
#                                                 # leafletOutput("map_dash"),
#                                                 # uiOutput("bar_graph"),
#                                                 # uiOutput("plot_and_table")
#                                                  fluidPage(
#                                                   #splitLayout(cellWidths = c("100%", "100%", "400%"), leafletOutput("map_dash"), uiOutput("bar_graph"), uiOutput("plot_and_table")),
#                                                   splitLayout(
#                                                     cellWidths = 1000,
#                                                     cellArgs = list(style = "padding: 6px"),
#                                                     leafletOutput("map_dash"),
#                                                     uiOutput("bar_graph"),
#                                                     uiOutput("plot_and_table")
#                                                   )
#                                                   #Leaflet Map UI
#                                                   # column(width = 12,
#                                                   #        leafletOutput("map_dash"))
#                                                   )
#                                                 )
#                         )
#                       )),
#                         
#                         tabItem(tabName = "about",
#                                 tags$div(style = "margin-top: 200px;"),
#                                 h1('About'),
#                                 h2('Created by Aditya Ranganathan on 02/07/2022'),
#                                 h3(""),
#                                 h3(""),
#                                 h3("The dashboard displays data reagrding CTA rides in a clear and intuitive manner.
#                  Users can check ride data of 3 different CTA stations: O'Hare Airport, UIC-Halsted and Racine.
#                  The data can be viewed from a yearly, monthly, weekly or daily basis. Users have the option of seeing the data either as plots or in a tabular form"),
#                                 h3("Users can get an idea about the number of passengers that travel through the 'L' and can also correlate certain major events that occurred in Chicago with respect to the number of riders during that time period."),
#                                 h3("Data was sourced from from the Chicago Data Portal at", tags$a(href="https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f/data", "this link")),
#                                 h3("The dataset consists of 1.1 million rows and has attributes 'stationname', 'station_id', 'date', 'rides' and 'daytype'")
#                                 
#                         )
#                       )
#                     )
# )



# SERVER=======================================================================================================
server <- function(input, output, session){
  observeEvent(log(10))
}
shinyApp(ui = ui, server = server)
