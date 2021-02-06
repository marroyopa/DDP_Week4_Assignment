#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(lubridate)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    observeEvent(input$Territory, {
        if(input$Territory == "Regions"){
            shinyjs::enable("Region1")
            shinyjs::enable("Region2")
            shinyjs::disable("Country1")
            shinyjs::disable("Country2")
        }else{
            shinyjs::disable("Region1")
            shinyjs::disable("Region2")
            shinyjs::enable("Country1")
            shinyjs::enable("Country2")
        }
    })
    
    output$dailyPlot <- renderPlotly({
        
        # Read parameters from ui.R
        
        datefrom <- input$Period[1]
        dateto <- input$Period[2] 
        territory <- input$Territory
        if (territory == "Regions"){
            region1 <- input$Region1
            region2 <- input$Region2
            graphtype <- 10
        } else {
            country1 <- input$Country1
            country2 <- input$Country2
            graphtype <- 20
        }
        
        infotype <- input$CasesDeath
        ifelse(infotype == "Cases", graphtype <- graphtype + 1,
               graphtype <- graphtype + 2)
        
        # generate the correct graph depending on the parameters
        
        if (graphtype == 11){
            
            datagraph <- data %>% 
                filter(Date_reported >= datefrom & Date_reported <= dateto &
                           (region == region1 | region == region2)) %>%
                group_by(region, Date_reported) %>% 
                summarise(daily = sum(New_cases), 
                          accum = sum(Cumulative_cases))
            
            plot_ly(data = datagraph, x=datagraph$Date_reported, 
                    y=datagraph$daily, type = 'scatter', mode = 'lines',
                    color = ~ datagraph$region) %>% 
                layout(title="Daily New Covid-19 Cases per Region", 
                       xaxis = list(title = "Days"), 
                       yaxis = list(title = "Daily Cases"))    
        }
        
        if (graphtype == 12){
            
            datagraph <- data %>% 
                filter(Date_reported >= datefrom & Date_reported <= dateto &
                           (region == region1 | region == region2)) %>%
                group_by(region, Date_reported) %>% 
                summarise(daily = sum(New_deaths), 
                          accum = sum(Cumulative_deaths))
            
            plot_ly(data = datagraph, x=datagraph$Date_reported, 
                    y=datagraph$daily, type = 'scatter', mode = 'lines',
                    color = ~ datagraph$region) %>% 
                layout(title="Daily New Covid-19 Deaths per Region", 
                       xaxis = list(title = "Days"), 
                       yaxis = list(title = "Daily Deaths"))    
        }
    })
    
})
