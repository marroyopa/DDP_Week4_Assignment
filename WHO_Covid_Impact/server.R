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

# Load necessary data to run the app

if(exists("whodata") == FALSE){
    address <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"
    whodata <- read.csv(address, sep = ",", dec = ".")
    colnames(whodata) <- c("Date_reported", "Country_code", "Country", 
                           "WHO_region", "New_cases", "Cumulative_cases", 
                           "New_deaths", "Cumulative_deaths")
    whodata$Date_reported <- as.Date(whodata$Date_reported)
    areas <- matrix(c("EMRO", "Eastern Mediterranean", "EURO", "Europe",
                      "AFRO", "Africa", "WPRO", "Western Pacific", "AMRO", 
                      "Americas", "SEARO", "South East Asia", "Other", 
                      "Others"), ncol = 2, byrow = TRUE)
    colnames(areas) <- c("Cod", "region")
    areas <- as.data.frame(areas)
    whodata <- merge(x = whodata, y = areas, by.x = "WHO_region", 
                     by.y = "Cod")
    maxdate <- max(whodata$Date_reported)
    mindate <- min(whodata$Date_reported)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Activate and Deactivate Regions or Countries depending on the selection
    # done by the user on the Territory radio button.
    
    observeEvent(input$Territory, {
        if(input$Territory == "Regions"){
            shinyjs::enable("Region1")
            shinyjs::enable("Region2")
            shinyjs::disable("Country1")
            shinyjs::disable("Country2")
        } else {
            shinyjs::disable("Region1")
            shinyjs::disable("Region2")
            shinyjs::enable("Country1")
            shinyjs::enable("Country2")
        }
    })

    output$dailyPlot <- renderPlotly({

        # Read parameters from ui.R and create graphtype variable to know the
        # type of graph to build depending on user choices
        
        datefrom <- input$Period[1]
        dateto <- input$Period[2]
        territory <- input$Territory
        infotype <- input$CasesDeath
        graphtype <- 0
        if (territory == "Regions"){
            region1 <- input$Region1
            region2 <- input$Region2
            graphtype <- 10
        } else {
            country1 <- input$Country1
            country2 <- input$Country2
            graphtype <- 20
        }
        ifelse(infotype == "Cases", graphtype <- graphtype + 1,
               graphtype <- graphtype + 2)
        
        # Generate the correct graph using graphtype and passing the parameters
        
        if(graphtype == 11){
            datagraph <- whodata %>% filter(Date_reported >= datefrom &
                                                Date_reported <= dateto &
                                                (region == region1 |
                                                     region == region2)) %>%
                group_by(region, Date_reported) %>%
                summarise(daily = sum(New_cases), accum = sum(Cumulative_cases))
            
            plot_ly(data = datagraph, x = datagraph$Date_reported, 
                    y = datagraph$daily, type = "scatter", mode = "lines", 
                    color = ~ datagraph$region) %>% 
                layout(title = "Daily New Covid-19 Cases per Region", 
                       xaxis = list(title = "Days"), 
                       yaxis = list(title = "Daily Infections Reported"))
        } else {
        
        if(graphtype == 12){
                datagraph <- whodata %>% filter(Date_reported >= datefrom &
                                                Date_reported <= dateto &
                                                (region == region1 |
                                                         region == region2)) %>%
                                group_by(region, Date_reported) %>%
                                summarise(daily = sum(New_deaths), 
                                          accum = sum(Cumulative_deaths))
                        
                plot_ly(data = datagraph, x = datagraph$Date_reported, 
                        y = datagraph$daily, type = "scatter", mode = "lines", 
                        color = ~ datagraph$region) %>% 
                        layout(title = "Daily New Covid-19 Deaths per Region", 
                               xaxis = list(title = "Days"), 
                               yaxis = list(title = "Daily Deaths Reported"))
        } else {
                
        if(graphtype == 21){
                datagraph <- whodata %>% filter(Date_reported >= datefrom &
                                                Date_reported <= dateto &
                                                (Country == country1 |
                                                 Country == country2)) %>%
                                group_by(Country, Date_reported) %>%
                                summarise(daily = sum(New_cases), 
                                          accum = sum(Cumulative_cases))
                        
                plot_ly(data = datagraph, x = datagraph$Date_reported, 
                        y = datagraph$daily, type = "scatter", mode = "lines", 
                        color = ~ datagraph$Country) %>% 
                        layout(title = "Daily New Covid-19 Cases per Country", 
                               xaxis = list(title = "Days"), 
                               yaxis = list(title = "Daily Cases Reported"))
        } else {
        
        if(graphtype == 22){
                datagraph <- whodata %>% filter(Date_reported >= datefrom &
                                                Date_reported <= dateto &
                                                (Country == country1 |
                                                     Country == country2)) %>%
                                group_by(Country, Date_reported) %>%
                                summarise(daily = sum(New_deaths), 
                                          accum = sum(Cumulative_deaths))
                        
                plot_ly(data = datagraph, x = datagraph$Date_reported, 
                        y = datagraph$daily, type = "scatter", mode = "lines", 
                        color = ~ datagraph$Country) %>% 
                        layout(title = "Daily New Covid-19 Deaths per Country", 
                               xaxis = list(title = "Days"), 
                               yaxis = list(title = "Daily Deaths Reported"))
        }        
                        
        }
                
        }        
                        
        }
        
        
    })
    
    output$accumPlot <- renderPlotly({
            
        # Read parameters from ui.R and create graphtype2 variable to know 
        # the type of graph to build depending on user choices
            
        datefrom <- input$Period[1]
        dateto <- input$Period[2]
        territory <- input$Territory
        infotype <- input$CasesDeath
        graphtype2 <- 0
        if (territory == "Regions"){
            region1 <- input$Region1
            region2 <- input$Region2
            graphtype2 <- 30
        } else {
            country1 <- input$Country1
            country2 <- input$Country2
            graphtype2 <- 40
        }
        ifelse(infotype == "Cases", graphtype2 <- graphtype2 + 1,
           graphtype2 <- graphtype2 + 2)
            
        # Generate the correct graph using graphtype and passing the parameters
            
        if(graphtype2 == 31){
            datagraph <- whodata %>% filter(Date_reported >= datefrom &
                                            Date_reported <= dateto &
                                            (region == region1 |
                                                     region == region2)) %>%
                    group_by(region, Date_reported) %>%
                    summarise(daily = sum(New_cases), 
                              accum = sum(Cumulative_cases))
                    
            plot_ly(data = datagraph, x = datagraph$Date_reported, 
                    y = datagraph$accum, type = "scatter", mode = "lines", 
                    color = ~ datagraph$region) %>% 
                    layout(title = "Accumulated Covid-19 Cases per Region", 
                           xaxis = list(title = "Days"), 
                           yaxis = list(title = "Accumulated Infections Reported"))
        } else {
                    
        if(graphtype2 == 32){
            datagraph <- whodata %>% filter(Date_reported >= datefrom &
                                            Date_reported <= dateto &
                                            (region == region1 |
                                                     region == region2)) %>%
                    group_by(region, Date_reported) %>%
                    summarise(daily = sum(New_deaths), 
                              accum = sum(Cumulative_deaths))
                    
            plot_ly(data = datagraph, x = datagraph$Date_reported, 
                    y = datagraph$accum, type = "scatter", mode = "lines", 
                    color = ~ datagraph$region) %>% 
                    layout(title = "Accumulated Covid-19 Deaths per Region", 
                           xaxis = list(title = "Days"), 
                           yaxis = list(title = "Accumulated Deaths Reported"))
        } else {
                            
        if(graphtype2 == 41){
            datagraph <- whodata %>% filter(Date_reported >= datefrom &
                                            Date_reported <= dateto &
                                            (Country == country1 |
                                                     Country == country2)) %>%
                    group_by(Country, Date_reported) %>%
                    summarise(daily = sum(New_cases), 
                                      accum = sum(Cumulative_cases))
                                    
            plot_ly(data = datagraph, x = datagraph$Date_reported, 
                    y = datagraph$accum, type = "scatter", mode = "lines", 
                    color = ~ datagraph$Country) %>% 
                    layout(title = "Accumulated Covid-19 Cases per Country", 
                           xaxis = list(title = "Days"), 
                           yaxis = list(title = "Accumulated Cases Reported"))
        } else {
                                    
        if(graphtype2 == 42){
            datagraph <- whodata %>% filter(Date_reported >= datefrom &
                                            Date_reported <= dateto &
                                            (Country == country1 |
                                                     Country == country2)) %>%
                    group_by(Country, Date_reported) %>%
                    summarise(daily = sum(New_deaths), 
                                      accum = sum(Cumulative_deaths))
                                            
            plot_ly(data = datagraph, x = datagraph$Date_reported, 
                    y = datagraph$accum, type = "scatter", mode = "lines", 
                    color = ~ datagraph$Country) %>% 
                    layout(title = "Accumulated Covid-19 Deaths per Country", 
                           xaxis = list(title = "Days"), 
                           yaxis = list(title = "Accumulated Deaths Reported"))
        }        
                                    
        }
                            
        }        
                    
        }
            
            
    })

})
