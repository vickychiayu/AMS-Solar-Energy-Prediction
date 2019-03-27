#Install libraries
library(shiny);
library(data.table);
library(ggplot2);
library(leaflet);

#Define names of weather stations
ws <- colnames(dt[, 2:99]);


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Solar Panel Analysis"),
  
  tabsetPanel(
    type = "tabs",
    
    tabPanel("Production Data Table", 
             
             ### SELECTORS
             
             # Select date
             dateRangeInput("dat_dates", label = h4("Select Date Range"), 
                            start = "2001-01-01", end = "2001-12-31",
                            min = "1994-01-01", max = "2007-12-31",
                            startview = "year"),
             
             # Select weather station
             selectInput("dat_ws", label = h4("Select Weather Station"),
                         ws,
                         width='55%',
                         multiple = TRUE),
    
             
             
             ### OUTPUT
            
             # Print table
             fluidRow(
               column(12, DT::dataTableOutput('data'))    
             )
             
        
        
    ),
  
    tabPanel("Plots",
      
      # select the solar plant that is going to be plotted
      sidebarPanel(
        
        #select target station
        selectInput("selected_t_ws", label = h4("Select Station"), ws,
                    selected = "ACME", multiple = FALSE),
        
        #select compared station
        selectInput("selected_c_ws", label = h4("Compare with"), ws,
                    selected = "GOOD", multiple = FALSE),
        
        # select the dates that is going to be plotted
        dateRangeInput("dates", label = h4("Select Date Range"), 
                       start = "2001-01-01", end = "2001-12-31",
                       min = "1994-01-01", max = "2007-12-31",
                       startview = "year")
        
      ),
      
      
      mainPanel(

        leafletOutput("stationMap", width = "800px", height = "350px"),
        br(),
        plotOutput("prod_plot", width = "800px", height = "350px")
      )
      
    )
 
   
  )
)
)