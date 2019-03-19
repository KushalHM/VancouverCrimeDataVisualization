#Crime Statistics Vancouver
#Developed by: Kushal Mahalingaiah and Harsh Sharma
#              Dept. of Computing Science, MRC, University of Alberta
#############################################################################
#library
library(shiny)
library(ggplot2)
library(rgdal)
library(dplyr)
library(readr)
library(leaflet)


#############################################################################
#import current dataset
crimeData <- read_csv("crime.csv")
crimeData <- filter(crimeData, X != 0)

#############################################################################
#Prepare dataset for shiny server

#Turn month and hour into int
crimeData$YEAR <- as.numeric(crimeData$YEAR)
crimeData$MONTH <- as.numeric(crimeData$MONTH)
crimeData$HOUR <- as.numeric(crimeData$HOUR)

crimeData$DATE <- as.Date(paste0(crimeData$YEAR, "-", crimeData$MONTH, "-", crimeData$DAY))

utmcoor<-SpatialPoints(cbind(crimeData$X, crimeData$Y), proj4string=CRS("+proj=utm +zone=10"))

#From utm to latitude or longitude
longlatcoor<-data.frame(spTransform(utmcoor,CRS("+proj=longlat")))

colnames(longlatcoor) <- c("Longitude", "Latitude")


#combine the new lat and lon with original dataset
crimeData <- cbind(crimeData, longlatcoor)


#minimize memory
# remove(utmcoor, longlatcoor)


#select relevant columns only
crimeData <-
  crimeData[, c("TYPE",
              "YEAR",
              "MONTH",
              "DAY",
              "HOUR",
              "MINUTE",
              "DATE",
              "Longitude",
              "Latitude")]
#summarized dataset
#By crime by month
CrimeHour <- crimeData %>% group_by(YEAR, MONTH, HOUR, TYPE) %>% summarise(
  Count = n()
)

#############################################################################

# Define UI for application
ui <- fluidPage(tabsetPanel(
  
  tabPanel("Interactive Crime Map",
           
           # Application title
           titlePanel("Vancouver Crime Map"),
           
           p(
             "(Developed with ♥ by",
             a(href = "http://www.linkedin.com/in/kushalhm", "Kushal"), "and", 
             a(href = "http://www.linkedin.com/in/hs174", "Harsh")," | Data Source: ",
             a(href = "http://data.vancouver.ca/datacatalogue/crime-data.htm", "Vancouver Police Department"),
             ")"
           ),
           
           #horizontal line
           hr(),
           
           # Sidebar with a slider input for Year 
           sidebarLayout(
             sidebarPanel(
               sliderInput("YearMap",
                           h4("YEAR: Select the range of years"),sep="",
                           min = min(crimeData$YEAR),
                           max = max(crimeData$YEAR),
                           value = c(2018, max(crimeData$YEAR))),
               
               br(),
               
               #Month slider
               sliderInput("MonthMap",
                           h4("MONTH: Select the range of months:"),
                           min = 1,
                           max = 12,
                           value = c(1,12)),
               
               br(),
               
               #crime type checkbox
               checkboxGroupInput("CrimeTypeMap", 
                                  h4("Select which crime types to display on map:"),
                                  unique(crimeData$TYPE),
                                  selected = unique(crimeData$TYPE))
               
             ),
             
             mainPanel(
               
               h4(strong("Instruction: Zoom in the map or mouse over the map to see the crime details.")),
               
               br(),
               
               leafletOutput("CrimeMap", height = "600px")
             )
           )
           
  ),
  
  tabPanel("Crime Statistics",
           
           # Application title
           titlePanel("Crime Statistics Across Year, Month and Hour"),
           
           p(
             "(Developed with ♥ by",
             a(href = "http://www.linkedin.com/in/kushalhm", "Kushal"), "and", 
             a(href = "http://www.linkedin.com/in/hs174", "Harsh")," | Data Source: ",
             a(href = "http://data.vancouver.ca/datacatalogue/crime-data.htm", "Vancouver Police Department"),
             ")"
           ),
           
           #horizontal line
           hr(), 
           
           
           # Sidebar with a slider input for Year 
           sidebarLayout(
             sidebarPanel(
               
               #Year slider 
               sliderInput("Year",
                           h4("YEAR: Select a year or a range of years"),sep="",
                           min = min(crimeData$YEAR),
                           max = max(crimeData$YEAR),
                           value = c(min(crimeData$YEAR), max(crimeData$YEAR))),
               
               #next line
               br(),
               
               #Month Slider
               sliderInput("Month",
                           h4("MONTH: Select a month or a range of months"),
                           min = 1,
                           max = 12,
                           value = c(1,12)),
               
               #next line
               br(),
               
               
               #crime type
               checkboxGroupInput("CrimeType", 
                                  h4("Select which crime types to display"),
                                  unique(crimeData$TYPE),
                                  selected = unique(crimeData$TYPE))
               
             ),
             
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("PlotYear", height = "600px"),
               br(),
               plotOutput("PlotMonth", height = "600px"),
               br(),
               plotOutput("PlotHour", height = "600px")
             )
           )
  )
  
  
))

###############################################################
# Define server logic to plot

server <- function(input, output) {
  
  
  #Plot Year
  output$PlotYear <- renderPlot({
    ggplot(CrimeHour[CrimeHour$YEAR %in% c(min(input$Year):max(input$Year)) &
                       CrimeHour$MONTH %in% c(min(input$Month):max(input$Month)) &
                       CrimeHour$TYPE %in% input$CrimeType
                     , ], 
           aes(x = as.numeric(YEAR), y = Count, fill = TYPE)) +
      geom_bar(stat = "sum", size=1) +
      labs(title = "Crime Type Trend Across Year", x = "Year", y = "Total Crime") +
      facet_wrap(~TYPE, ncol = 3) +
      theme(text=element_text(size=17),legend.position = "none")
  })
  
  
  #Plot Month
  output$PlotMonth <- renderPlot({
    ggplot(CrimeHour[CrimeHour$YEAR %in% c(min(input$Year):max(input$Year)) &
                       CrimeHour$MONTH %in% c(min(input$Month):max(input$Month)) &
                       CrimeHour$TYPE %in% input$CrimeType
                     , ], 
           aes(x = as.factor(MONTH), y = Count, fill = TYPE)) +
      geom_bar(stat = "sum", size=1) +
      labs(title = "Crime Type Trend Across Month", x = "Month", y = "Total Crime") +
      facet_wrap(~TYPE, ncol = 3) +
      theme(text=element_text(size=17),legend.position = "none")
  })
  
  
  #Plot Hour
  output$PlotHour <- renderPlot({
    ggplot(CrimeHour[CrimeHour$YEAR %in% c(min(input$Year):max(input$Year)) &
                       CrimeHour$MONTH %in% c(min(input$Month):max(input$Month)) &
                       CrimeHour$TYPE %in% input$CrimeType
                     , ], 
           aes(x = as.numeric(HOUR), y = Count, fill = TYPE)) +
      geom_line()+#geom_bar(stat = "sum", size=1) +
      labs(title = "Crime Type Trend Across Hour", x = "Hour", y = "Total Crime") +
      facet_wrap(~TYPE, ncol = 3) +
      theme(text=element_text(size=17),legend.position = "none")
  })
  
  #Map
  output$CrimeMap <- renderLeaflet({
    leaflet(crimeData[which(crimeData$YEAR %in% c(min(input$YearMap):max(input$YearMap)) &
                              crimeData$MONTH %in% c(min(input$MonthMap):max(input$MonthMap)) &
                            !is.na(crimeData$HOUR) &
                            crimeData$TYPE %in% input$CrimeTypeMap), ]) %>%
      addTiles() %>%
      addMarkers(
        lng = ~ Longitude,
        lat = ~ Latitude,
        label = ~ lapply(paste0('<h6><p><b>TYPE - </b>',TYPE, 
                                "<br /> <b>DATE - </b>", DATE, 
                                "<br/><b>TIME - </b>", HOUR, ":", MINUTE), HTML),
        clusterOptions = markerClusterOptions()
      )
  })
  
  
}


#########################################
# Run the application 
shinyApp(ui = ui, server = server)


##########################################
#Upload to Shiny Server

#library(rsconnect)
#rsconnect::deployApp('/full/path/to/the/app/folder') 