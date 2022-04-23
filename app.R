#setwd("C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project2")
#setwd("C:/Users/Krishnan CS/424_Project2")
#print(getwd())

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
memory.limit(24000)

# READ DATA=======================================================================================================
all_df <- do.call(rbind, lapply(list.files(pattern = "*.csv"), read.csv))
companies <- read.csv('./ext/companies.csv', row.names = 1)
communities <- read.csv('./ext/communities.csv', row.names = 1)  

# PREPROCESSING======================================================================
# Remove index column in df
all_df <- all_df[, -1]

# Set date column in Date format
all_df$date <- as.Date(all_df$date, "%Y-%m-%d")

# Dataframe w/o outside areas
sub_df <- subset(all_df, pickup > 0 & dropoff > 0)

# Create 'companies' to store list of all taxi companies
company_names <- unique(row.names(companies))
company_names <-  c("All Taxis" = "all_taxis", company_names)

# Create 'comms' to store list of all Community areas
comm_names <- unique(row.names(communities))
comm_names <-  c("All Communities" = "all_comms", "Outside Chicago" = 0, comm_names)



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
    baseGroups = c("Default", "CTA Lines", "Minimal"),
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
  titlePanel("CS424 Project-2"),
  fluidRow(
    #height = "100%",
    #column-1 for about and controls 
    column( width = 2,
            wellPanel(
              #About goes here
              HTML(" <h1><b>About</b></h1> 
             <h2>Developed as part of Project 2 for CS424 (Visualization and Visual Analytics) - UIC Spring 2022</h2>
             <h2><b>Authors:</b> Aditya R, Krishnan CS </h2>
             <h2> <b>Created February 5th, 2022</b></h2>,
             <h3> This App presents the Chicago CTA Ridership data and Lat/Long data obtained from the Chicago Data Portal website </h3>
             <h3> <b> *Data Sources: </b></h3>
             <h3><a href=\"https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f\">link L-Ridership data</a> </h3>
          <h3>  <a href=\"https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme\">link L-System_Information data</a> </h3>
          </br>"),
              
              style = "height:85vh;", 
              fluidRow(style = "margin-top:15%",
                       radioButtons("comm_areas", "Areas Outside Chicago",
                                    c("Include in data" = "incl_comm",
                                      "Exclude from data" = "excl_comm"),
                                    selected = "incl_comm",
                                    inline = FALSE)
              ),
              
              fluidRow(radioButtons("dist_view", "Distance",
                                    c("Miles" = "mi",
                                      "Kilometers" = "km"),
                                    selected = "mi",
                                    inline = FALSE)
              ),
              
              fluidRow(radioButtons("time_view", "Time",
                                    c("12 Hour" = "hr_12",
                                      "24 Hour" = "hr_24"),
                                    selected = "hr_24",
                                    inline = FALSE)
              ),
              
              fluidRow(radioButtons("dir_view", "Direction",
                                    c("To" = "to",
                                      "From" = "from"),
                                    selected = "to",
                                    inline = FALSE)
              ),
              
              HTML("<br>"),
              fluidRow(
                selectizeInput("taxi_view", "Taxi Company", choices = company_names, selected="all_taxis"),
              ),
              
              fluidRow(
                selectizeInput('comm_view', "Community Areas", choices = comm_names,
                               selected = "all_comms")
              ),
              
              
              fluidRow(
                selectInput("bar_view", "Bar Plot View", choices = c("Day of Year" = "daily", "Hour of Day" = 'hourly', 
                                                                     "Day of Week" = "weekly", "Month of Year" = "monthly", 
                                                                     "Binned Mileage" = "bmile", "Binned Trip Time" = "btime")),
              )
            )#Wellpanel1
    ), #Column-1
    
    #Rest of the stuff
    column(width = 10,
           
           #Bar plot column 
           column(width = 4,
                  fluidRow( style = "height:85vh;", leafletOutput(height = "100%","map_dash"))
           ),
           
           #Line data Table and map column
           column( width = 4,
                   fluidRow(style = "height:15vh;", uiOutput(height = "85%", style ="width: 50%;","plot_and_table")),
                   ),
           
           #Yearly graph column 
           column( width = 4,
                   #The yearly graph for station goes here 
                   fluidRow(style = "margin-top:300px; height:60vh;",plotlyOutput(height = "100%", "percentage_graph"))
                   
           )
           
    )
    
    
    
  )
)



# SERVER=======================================================================================================
server <- function(input, output, session){
  # Filter data based on conditions
  dataframeReactive <- reactive({ 
    if(input$comm_areas == "incl_comm"){
      data <- all_df
      
      # Filter data by taxi company
      if(input$taxi_view != 'all_taxis'){
        code = companies[input$taxi_view, ]
        data <- subset(data, code ==  code)
      }
      
      # Filter data by community area
      if(input$comm_view != "all_comms"){
        if(input$dir_view == "to"){
          code = communities[input$comm_view, ]
          data <- subset(data, dropoff == code)
        }
        else{
          data <- subset(data, pickup == code)
        }
      }
    }
    else{
      data <- sub_df
      
      if(input$taxi_view != 'all_taxis'){
        code = companies[input$taxi_view,]
        data <- subset(data, code == code)
      }
      
      if(input$comm_view != "all_comms"){
        if(input$comm_view == 0){
          validate(
            need(input$comm_view == 0, "Community Areas removed. Please change to non-zero value")
          )
        }
        else{
          if(input$dir_view == "to"){
            code = communities[input$comm_view, ]
            data <- subset(data, dropoff == code)
          }
          else{
            data <- subset(data, pickup == code)
          }
        }
        
      }
    }
    
    return(data)
    
  })
  
  daily_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    data <- data[c("date")]
    
    return(data)
  })
  
  monthly_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    data <- data[c("month_name")]
    
    return(data)
  })
  
  weekly_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    data <- data[c("week_day")]
    
    return(data)
  })
  
  hourly_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    data <- data[c("hour")]
    
    return(data)
  })
  
  mile_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    
    breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
    bins <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
              "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
    
    groups <- cut(data$miles, 
                  breaks=breaks,
                  labels=bins,
                  include.lowest=TRUE, 
                  right=FALSE
                  )
    
    data <- as_tibble(groups)
    return(data)
  })
  
  trip_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    
    breaks <- c(0, 180, 300, 420, 600, 720, 900, 1200, 1800, 2700, 3600, 7200, 10800, 14400, 18000)
    bins <- c("3min","5min","7min","10min","12min","15min","20min","30min","45min","1hr","2hr","3hr","4hr","5hr")
    
    groups <- cut(data$sec,
                  breaks=breaks,
                  labels=bins,
                  include.lowest=TRUE,
                  right=FALSE
                  )
    data <- as_tibble(groups)
    return(data)
  })
  
  
  output$daily_plot <- renderPlot({
    ggplot(data=daily_df(), aes(x=date)) +
      geom_bar(stat="count") +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Total Rides", title="Total Rides by Date")
  })
  
  output$monthly_plot <- renderPlot({
    ggplot(data=monthly_df(), aes(x=month_name)) +
      geom_bar(stat="count") +
      scale_y_continuous(labels = comma) +
      labs(x="Month", y="Total Rides", title="Total Rides by Date")
  })
  
  output$weekly_plot <- renderPlot({
    ggplot(data=weekly_df(), aes(x=week_day)) +
      geom_bar(stat="count") +
      scale_y_continuous(labels = comma) +
      labs(x="Week Day", y="Total Rides", title="Total Rides by Date")
  })
  
  output$hourly_plot <- renderPlot({
    ggplot(data=hourly_df(), aes(x=hour)) +
      geom_bar(stat="count") +
      scale_y_continuous(labels = comma) +
      labs(x="Time", y="Total Rides", title="Total Rides by Date")
  })
  
  output$mile_plot <- renderPlot({
    ggplot(data=mile_df(), aes(x=value)) +
      geom_bar(width = 1.0) +
      scale_y_continuous(labels = comma) +
      labs(x="Miles", y="Total Rides", title="Total Rides by Mileage")
  })
  
  output$trip_plot <- renderPlot({
    ggplot(data=trip_df(), aes(x=value)) +
      geom_bar(width = 1.0) +
      scale_y_continuous(labels = comma) +
      labs(x="Trip Time", y="Total Rides", title="Total Rides by Trip Time")
  })
  
  
  
  # render graph and table output
  output$plot_and_table <- renderUI({
    
    if(input$bar_view == "daily"){
      fluidPage(
        fluidRow( style = "height:40vh;",
                  column(12, div(plotOutput("daily_plot"))),
                  #HTML("</br></br></br></br></br>"),
                  #column(12, uiOutput("daily_table"))
        )
      )
    }
    else if(input$bar_view == "hourly"){
      fluidPage( style = "height:40vh;",
                 fluidRow(
                   column(12, div(plotOutput("hourly_plot"))),
                   #HTML("</br></br></br></br></br>"),
                   #column(12, uiOutput("hourly_table"))
                 )
      )
    }
    else if(input$bar_view == "weekly"){
      fluidPage( style = "height:40vh;",
                 fluidRow(
                   column(12, div(plotOutput("weekly_plot"))),
                   #HTML("</br></br></br></br></br>"),
                   #column(12, uiOutput("weekly_table"))
                 )
      )
    }
    else if(input$bar_view == "monthly"){
      fluidPage(
        fluidRow(
          column(12, div(plotOutput("monthly_plot"))),
          #HTML("</br></br></br></br></br>"),
          #column(12, uiOutput("monthly_table"))
        )
      )
    }
    else if(input$bar_view == "bmile"){
      fluidPage(
        fluidRow(
          column(12, div(plotOutput("mile_plot"))),
          #HTML("</br></br></br></br></br>"),
          #column(12, uiOutput("monthly_table"))
        )
      )
    }
    else{
      fluidPage(
        fluidRow(
          column(12, div(plotOutput("trip_plot")))
        )
      )
    }
    
    
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
