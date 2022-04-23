setwd("C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project3")


# LIBRARIES=======================================================================================================
library(lubridate)
library(ggplot2)
library(leaflet)
library(leaflet.providers)
library(maptools)
library(rgdal)
library(viridis)
library(dplyr)
library(DT)
library(tidyr)
library(scales)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(stringr)


options(scipen=999)

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
company_names <- sort(company_names, decreasing = FALSE)
company_names <-  c("All Taxis" = "all_taxis", company_names)

# Create 'comms' to store list of all Community areas
comm_names <- unique(row.names(communities))
comm_names <- sort(comm_names, decreasing = FALSE)
comm_names2 <- comm_names
comm_names <-  c("All Communities" = "all_comms", "Outside Chicago" = 0, comm_names)


# Map shape file
shape_file <- readOGR('shapes')
spt <- spTransform(shape_file, CRS("+proj=longlat +datum=WGS84"))

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
             <h2>Developed as part of Project 3 for CS424 (Visualization and Visual Analytics) - UIC Spring 2022</h2>
             <h2><b>Authors:</b> Aditya R </h2>
             <h2> <b>Created April 20th, 2022</b></h2>,
             <h3> This App presents the Chicago Taxi Ridership data in the various community areas obtained from the Chicago Data Portal website </h3>
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
                               selected = "OHARE")
              ),
              
              
              fluidRow(
                selectInput("bar_view", "Bar Plot View", choices = c("Day of Year" = "daily", "Hour of Day" = 'hourly', 
                                                                     "Day of Week" = "weekly", "Month of Year" = "monthly", 
                                                                     "Binned Mileage" = "bmile", "Binned Trip Time" = "btime",
                                                                     "Percent Graph" = "pct")),
              )
            )#Wellpanel1
    ), #Column-1
    
    #Rest of the stuff
    column(width = 10,
           
           #Map 
           column(width = 4,
                  fluidRow( style = "height:85vh;", leafletOutput(height = "100%","map_dash"))
           ),
           
           # Bar Plot
           column( width = 4,
                   fluidRow(style = "height:15vh;", uiOutput(height = "150%", style ="width: 100%;","plots")),
                   ),
           
           # Tables 
           column( width = 4,
                   #The yearly graph for station goes here 
                   fluidRow(style = "margin-top:300px; height:60vh;", uiOutput(height = "100%", "tables"))
                   
           )
           
    )
    
    
    
  )
)



# SERVER=======================================================================================================
server <- function(input, output, session){
  
  # ===DATA PROCESSING===
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
        # Filter by direction
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
      
      # Filter data by taxi company
      if(input$taxi_view != 'all_taxis'){
        code = companies[input$taxi_view,]
        data <- subset(data, code == code)
      }
      
      # Filter data by community
      if(input$comm_view != "all_comms"){
        if(input$comm_view == 0){
          validate(
            need(input$comm_view == 0, "Community Areas removed. Please change to non-zero value")
          )
        }
        else{
          # Filter by direction
          if(input$dir_view == "to"){
            code = communities[input$comm_view, ]
            data <- subset(data, dropoff == code)
          }
          else{
            code = communities[input$comm_view, ]
            data <- subset(data, pickup == code)
          }
        }
        
      }
    }
    
    return(data)
    
  })
  
  # === PLOT DATAFRAMES =====
  # Daily dataframe
  daily_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    data <- data[c("date")]
    
    return(data)
  })
  
  # Daily Table
  daily_table <- reactive({
    table_frame <- daily_df()
    table_frame <- table_frame %>% count(date) %>% rename(Dates = date, Rides = n)
    return(table_frame)
  })
  
  
  # Monthly Dataframe
  monthly_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    data <- data[c("month_name")]
    
    return(data)
  })
  
  # Monthly table
  monthly_table <- reactive({
    table_frame <- monthly_df()
    table_frame <- table_frame %>% count(month_name) %>% rename(Month = month_name, Rides = n)
    return(table_frame)
  })
  
  # Weekly Dataframe
  weekly_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    data <- data[c("week_day")]
    
    return(data)
  })
  
  # Weekly table
  weekly_table <- reactive({
    table_frame <- weekly_df()
    table_frame <- table_frame %>% count(week_day) %>% rename(`Week Day` = week_day, Rides = n)
    return(table_frame)
  })
  
  # hourly Dataframe
  hourly_df <- reactive({
    # Retrieve data
    
    data <- dataframeReactive()
    data <- data[c("hour")]
    
    return(data)
  })
  
  # Hourly Table
  hourly_table <- reactive({
    
    table_frame <- hourly_df()
    
    if(input$time_view == "hr_24"){
      table_frame <- table_frame %>% count(hour) %>% rename(Hour = hour, Rides = n)
      return(table_frame)
    }
    else{
      table_frame$hour[table_frame$hour == 0] <- "12 am"
      table_frame$hour[table_frame$hour == 1] <- "1 am"
      table_frame$hour[table_frame$hour == 2] <- "2 am"
      table_frame$hour[table_frame$hour == 3] <- "3 am"
      table_frame$hour[table_frame$hour == 4] <- "4 am"
      table_frame$hour[table_frame$hour == 5] <- "5 am"
      table_frame$hour[table_frame$hour == 6] <- "6 am"
      table_frame$hour[table_frame$hour == 7] <- "7 am"
      table_frame$hour[table_frame$hour == 8] <- "8 am"
      table_frame$hour[table_frame$hour == 9] <- "9 am"
      table_frame$hour[table_frame$hour == 10] <- "10 am"
      table_frame$hour[table_frame$hour == 11] <- "11 am"
      table_frame$hour[table_frame$hour == 12] <- "12 pm"
      table_frame$hour[table_frame$hour == 13] <- "1 pm"
      table_frame$hour[table_frame$hour == 14] <- "2 pm"
      table_frame$hour[table_frame$hour == 15] <- "3 pm"
      table_frame$hour[table_frame$hour == 16] <- "4 pm"
      table_frame$hour[table_frame$hour == 17] <- "5 pm"
      table_frame$hour[table_frame$hour == 18] <- "6 pm"
      table_frame$hour[table_frame$hour == 19] <- "7 pm"
      table_frame$hour[table_frame$hour == 20] <- "8 pm"
      table_frame$hour[table_frame$hour == 21] <- "9 pm"
      table_frame$hour[table_frame$hour == 22] <- "10 pm"
      table_frame$hour[table_frame$hour == 23] <- "11 pm"
      
      table_frame <- table_frame %>% count(hour) %>% rename(Hour = hour, Rides = n)
      return(table_frame)
    }
    
  })
  
  # Binned Mile dataframe
  mile_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    
    # Check distance metric
    if(input$dist_view == "mi"){
      breaks <- c(0, 1, 1.25, 1.5, 2, 2.5, 3, 5, 8, 10, 15, 20, 25, 30, 45, 101)
      bins <- c("[0.5-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-2.5]", "[2.5-3]","[3-5]","[5-8]","[8-10]","[10-15]",
                "[15-20]","[20-25]","[25-30]","[30-45]","[45-100]")
      
      args <- cut(data$miles, 
                  breaks=breaks,
                  labels=bins,
                  include.lowest=TRUE, 
                  right=FALSE
      )
      
      data <- as_tibble(args)
      return(data)
    }
    
    else{
      breaks <- c(0, 1, 1.5, 2, 2.5, 3, 5, 8, 10, 12, 15, 20, 25, 30, 45, 101, 150)
      bins <- c("[0.5-1]","[1-1.5]","[1.5-2]","[2-2.5]", "[2.5-3]","[3-5]","[5-8]","[8-10]","[10-12]",
                "[12-15]", "[15-20]","[20-25]","[25-30]","[30-45]","[45-100]", "[100-150]")
      
      data$km <- data$miles * 1.60934
      
      args <- cut(data$km, 
                  breaks=breaks,
                  labels=bins,
                  include.lowest=TRUE, 
                  right=FALSE
      )
      
      data <- as_tibble(args)
      return(data)
    }
  })
  
  # Binned Mile Table
  mile_table <- reactive({
    table_frame <- mile_df()
    table_frame <- table_frame %>% count(value) %>% rename(`Distance Range` = value, Rides = n)
    return(table_frame)
  })
  
  # Binned Trip data frame
  trip_df <- reactive({
    # Retrieve data
    data <- dataframeReactive()
    
    breaks <- c(0, 180, 300, 480, 600, 720, 900, 1200, 1800, 2700, 3600, 7200, 10800, 14400, 18000)
    bins <- c("2min","5min","8min","10min","12min","15min","20min","30min","45min","1hr","2hr","3hr","4hr","5hr")
    
    args <- cut(data$sec,
                  breaks=breaks,
                  labels=bins,
                  include.lowest=TRUE,
                  right=FALSE
                  )
    data <- as_tibble(args)
    return(data)
  })
  
  # Binned Trip table
  trip_table <- reactive({
    table_frame <- trip_df()
    table_frame <- table_frame %>% count(value) %>% rename(`Time Range` = value, Rides = n)
    return(table_frame)
  })
  
  # Percentage Dataframe
  pct_df <- reactive({
    if((input$comm_view != 'all_comms') & (input$comm_view != 0)){
      data <- mapdata()
      # Check for direction
      if(input$dir_view == "to"){
        data <- data[!(data$dir == "dropoff"),]
        return(data)
      }
      else{
        data <- data[!(data$dir == "pickup"),]
        return(data)
      }
    }
    else{
      validate(
        need(input$comm_view != 'all_comms', 'Please select a community'),
        need(input$comm_view != 0, 'Please select a community')
      )
    }
    
  })
  
  # Percentage Table
  pct_table <- reactive({
    table_frame <- pct_df()
    table_frame <- table_frame %>% rename(comm_code = area_num_1)
    communities <- tibble::rownames_to_column(communities, "community")
    communities$comm_code <- as.character(communities$comm_code)
    table_frame <- merge(table_frame, communities, by="comm_code")
    
    print(table_frame)
    print(communities)
    table_frame <- table_frame[, c('community', 'comm_code', 'percentage')]
    table_frame <- table_frame %>% rename(Community = community, `Area Code` = comm_code, Percentage = percentage)
    return(table_frame)
  })
  
  # === BAR PLOTS ===
  
  # Daily Plot
  output$daily_plot <- renderPlot({
    ggplot(data=daily_df(), aes(x=date)) +
      geom_bar(stat="count") +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Total Rides", title="Total Rides by Date")
  })
  
  # Monthly Plot
  output$monthly_plot <- renderPlot({
    ggplot(data=monthly_df(), aes(x=month_name)) +
      geom_bar(stat="count") +
      scale_y_continuous(labels = comma) +
      labs(x="Month", y="Total Rides", title="Total Rides by Date")
  })
  
  # Weekly Plot
  output$weekly_plot <- renderPlot({
    ggplot(data=weekly_df(), aes(x=week_day)) +
      geom_bar(stat="count") +
      scale_y_continuous(labels = comma) +
      labs(x="Week Day", y="Total Rides", title="Total Rides by Date")
  })
  
  # Hourly Plot
  output$hourly_plot <- renderPlot({
    
    # Check for time format
    if (input$time_view=="hr_24"){
      ggplot(data=hourly_df(), aes(x=hour)) +
        geom_bar(stat="count") +
        scale_y_continuous(labels = comma) +
        labs(x="Time", y="Total Rides", title="Total Rides by Date")
    }
    else{
      ggplot(data=hourly_df(), aes(x=hour)) +
        geom_bar(stat="count") +
        scale_y_continuous(labels = comma) +
        labs(x="Time", y="Total Rides", title="Total Rides by Date") +
        scale_x_continuous(breaks = seq(0, 23, 1),
                           labels =c("12am","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm")
        )
                           
    }
    
  })
  
  # Binned mile plot
  output$mile_plot <- renderPlot({
    if(input$dist_view == "mi"){
      ggplot(data=mile_df(), aes(x=value)) +
        geom_bar(width = 1.0) +
        scale_y_continuous(labels = comma) +
        labs(x="Miles", y="Total Rides", title="Total Rides by Mileage")
    }
    else{
      ggplot(data=mile_df(), aes(x=value)) +
        geom_bar(width = 1.0) +
        scale_y_continuous(labels = comma) +
        labs(x="Kilometres", y="Total Rides", title="Total Rides by Kilometre")
    }
    
  })
  
  # Binned Trip Plot
  output$trip_plot <- renderPlot({
    ggplot(data=trip_df(), aes(x=value)) +
      geom_bar(width = 1.0) +
      scale_y_continuous(labels = comma) +
      labs(x="Trip Time", y="Total Rides", title="Total Rides by Trip Time")
  })
  
  # Percentage Plot
  output$pct_plot <- renderPlot({
    
    ggplot(data = pct_table(), aes(x = Percentage, y = factor(Community, level = comm_names2))) +
      geom_bar(stat = "identity") + 
      labs(x ="Percent of Rides", y = "Community Area", title = "%age of Rides to/from") 
  })
  
  # === TABLES LAYOUT ===
  # Daily Layout
  output$daily_table <- renderUI({
    div(
      datatable(
        daily_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(width = '500px', className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      ))
  })
  
  # Hourly Layout
  output$hourly_table <- renderUI({
    div(
      datatable(
        hourly_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(width = '500px', className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      ))
  })
  
  # Weekly Layout
  output$weekly_table <- renderUI({
    div(
      datatable(
        weekly_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(width = '500px', className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      ))
  })
  
  # Monthly Layout
  output$monthly_table <- renderUI({
    div(
      datatable(
        monthly_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(width = '500px', className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      ))
  })
  
  # Binned Mile Layout
  output$mile_table <- renderUI({
    div(
      datatable(
        mile_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(width = '500px', className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      ))
  })
  
  # Binned Trip Layout
  output$trip_table <- renderUI({
    div(
      datatable(
        trip_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(width = '500px', className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      ))
  })
  
  # Percentage Table Layout
  output$pct_table <- renderUI({
    div(
      datatable(
        pct_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(width = '500px', className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      ))
  })
  
  # === UI OUTPUT ===
  output$plots <- renderUI({
    
    if(input$bar_view == "daily"){
      fluidPage(
        fluidRow( style = "height:80vh;",
                  column(12, div(plotOutput("daily_plot")))
        )
      )
    }
    else if(input$bar_view == "hourly"){
      fluidPage( style = "height:80vh;",
                 fluidRow(
                   column(12, div(plotOutput("hourly_plot")))
                 )
      )
    }
    else if(input$bar_view == "weekly"){
      fluidPage( style = "height:80vh;",
                 fluidRow(
                   column(12, div(plotOutput("weekly_plot")))
                 )
      )
    }
    else if(input$bar_view == "monthly"){
      fluidPage(
        fluidRow(
          column(12, div(plotOutput("monthly_plot")))
        )
      )
    }
    else if(input$bar_view == "bmile"){
      fluidPage(
        fluidRow(
          column(12, div(plotOutput("mile_plot")))
        )
      )
    }
    else if(input$bar_view == "pct"){
      fluidPage(
        fluidRow(
          column(12, div(plotOutput("pct_plot")))
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
  
  # Tables Render
  output$tables <- renderUI({
    
    if(input$bar_view == "daily"){
      fluidPage(
        fluidRow( style = "height:80vh;",
                  column(12, div(uiOutput("daily_table")))
        )
      )
    }
    else if(input$bar_view == "hourly"){
      fluidPage( style = "height:80vh;",
                 fluidRow(
                   column(12, div(uiOutput("hourly_table")))
                 )
      )
    }
    else if(input$bar_view == "weekly"){
      fluidPage( style = "height:80vh;",
                 fluidRow(
                   column(12, div(uiOutput("weekly_table")))
                 )
      )
    }
    else if(input$bar_view == "monthly"){
      fluidPage( style = "height:80vh;",
        fluidRow(
          column(12, div(uiOutput("monthly_table")))
        )
      )
    }
    else if(input$bar_view == "bmile"){
      fluidPage( style = "height:80vh;",
        fluidRow(
          column(12, div(uiOutput("mile_table")))
        )
      )
    }
    else if(input$bar_view == "pct"){
      fluidPage( style = "height:80vh;",
        fluidRow(
          column(12, div(uiOutput("pct_table")))
        )
      )
    }
    else{
      fluidPage( style = "height:80vh;",
        fluidRow(
          column(12, div(uiOutput("trip_table")))
        )
      )
    }
    
    
    
  })
  
####################################################################################################################################################  
# MAP=================================
  
  # MAP DATA
  mapdata <- reactive({
    data <- sub_df
    if(input$taxi_view != "all_taxis"){
      code = companies[input$taxi_view,]
      data <- subset(data, code == code)
    }
    
    if(input$comm_view != "all_comms"){
      code = communities[input$comm_view, ]
      if(input$dir_view == "to"){
        data <- subset(data, dropoff == code)
      }
      else{
        data <- subset(data, pickup == code)
      }
        
      # Aggregate data to find percentage of rides in each community
      trips <- data %>%
        select(`pickup`,`dropoff`) %>%
        gather(dir, area_num_1) %>%
        count(dir, area_num_1) %>%
        drop_na(area_num_1) %>%
        mutate(area_num_1 = as.character(area_num_1)) %>%
        rename(count = n)
      
      tot <- max(trips$count)
      trips$percentage <- ((trips$count)/tot)*100
      return(trips)
    }
    
  })
  
  output$map_dash <- renderLeaflet({
    
    if(input$comm_view == "all_comms"){
      # Basic Map Layout
      leaflet(spt) %>% 
        addTiles() %>% 
        setView(lat=41.891105, lng=-87.652480,zoom = 11) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data=spt,
                    weight=1,
                    highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = TRUE)
        )
    }
    else{
      # Get community data and percentage
      trips <- mapdata()
      
      # basic Map
      leaflet(spt) %>% 
        addTiles() %>% 
        setView(lat=41.891105, lng=-87.652480,zoom = 11) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data=spt,
                    weight=1,
                    highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = TRUE)
        )
      spt_from <- spt
      spt_to <- spt
      
      # Join Percentage data to shape file 
      spt_from@data <- spt_from@data %>%
        left_join(filter(trips, dir == 'pickup'), by = 'area_num_1')
      
      spt_to@data <- spt_to@data %>%
        left_join(filter(trips, dir == 'dropoff'), by = 'area_num_1')
      
      bins <- c(0, 0.3, 0.5, 1, 2, 3, 4, 5, 6, 7, 9, 10, 50, 100)
      pal <- colorBin("viridis", domain = (spt@data$percentage), bins = bins)
      
      # label data for info hover
      from_labels <- sprintf(
        "<strong>Community: %s</strong><br/>percent=%g",
        spt_from@data$community, spt_from@data$percentage
      ) %>% lapply(htmltools::HTML)
      
      to_labels <- sprintf(
        "<strong>Community: %s</strong><br/>percent=%g",
        spt_to@data$community, spt_to@data$percentage
      ) %>% lapply(htmltools::HTML)
      
      leaflet(spt_from) %>% 
        addTiles() %>% 
        setView(lat=41.891105, lng=-87.652480,zoom = 10) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data=spt_from,
                    weight=1,
                    fillColor = ~pal(percentage),
                    fillOpacity = 0.6,
                    group = "pick-up",
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    label=~from_labels) %>%
        addPolygons(data=spt_to,
                    weight=1,
                    fillColor = ~pal(percentage),
                    fillOpacity = 0.6,
                    group = "drop-off",
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    label=~to_labels) %>%
        addLegend(pal = pal, 
                  values = ~percentage,
                  opacity = 0.4, 
                  title = "Rides %",
                  position = "topright") %>%
        addLayersControl(
          baseGroups = c("pick-up", "drop-off"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
    
    
    
    
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
