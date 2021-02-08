#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(lubridate)

# Define UI for application that draws daily and accumulated graphs
shinyUI(fluidPage(

    shinyjs::useShinyjs(),
    
    # Application title
    titlePanel("Covid Impact Comparison"),
    
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
        mindate <- min(whodata$Date_reported)
        maxdate <- max(whodata$Date_reported)
        blank <- NULL
    },

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("Period", "Date From - To:", min = mindate, 
                        max = maxdate, value = c(mindate, maxdate)),
            radioButtons("Territory", "By Regions or Countries:", 
                         choices = c("Regions", "Countries"), selected = "Regions"),
            selectInput("Region1", "Select Region 1", 
                        choices = distinct(whodata, region), selected = "Europe"),
            selectInput("Region2", "Select Region 2", 
                        choices = distinct(whodata, region), selected = "Africa"),
            selectInput("Country1", "Select Country 1", 
                        choices = distinct(whodata, Country), selected = "Germany"),
            selectInput("Country2", "Select Country 2", 
                        choices = distinct(whodata, Country), selected = "Spain"),
            radioButtons("CasesDeath", "Cases or Deaths:", 
                         choices = c("Cases", "Deaths"), selected = "Cases")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Daily", br(), plotlyOutput("dailyPlot")),
                        tabPanel("Accumulated", br(), plotlyOutput("accumPlot")),
                        tabPanel("Instructions", br(),
                                 h2("Instructions to use the App"),
                                 h3("Objectives"),
                                 p("This app loads data directly from the WHO 
                                   website of the daily impact of Covid-19 in 
                                   every country of the world and allows to 
                                   compare infections or deaths between two 
                                   countries or two world areas, both in a 
                                   daily or accumulated basis."),
                                 h3("Control Parameters"),
                                 p("Following controls are available to control 
                                   the app:"),
                                 p("- Date From - To: is a slider to select 
                                   beggining and end date of the comparison"),
                                 p("- By Regions or Countries: is a button to 
                                   select if you want to compare Covid evolution 
                                   between two World Regions or between two 
                                   Countries."),
                                 p("- Region 1 and Region 2: are two list were
                                   we can select the two world regions to 
                                   compare. If By Regions or Countries is 
                                   marqued By Countries, these two selectors 
                                   will be disabled."),
                                 p("- Country 1 and Country 2: are two list were
                                   we can select the two world regions to 
                                   compare. If By Regions or Countries is 
                                   marqued By Regions, these two selectors will
                                   be disabled."),
                                 p("- Cases or Deaths: is a button to select
                                   if you want to compare cases reported or
                                   deaths reported."),
                                 h3("Tabs"),
                                 p("Three tabs compose this app:"),
                                 p("- Daily is the tab were daily reported data
                                   comparison can be found."),
                                 p("- Accumulated is the tab were accumulated
                                 reported data comparison can be found."),
                                 p("- Instructions is this tab with the 
                                   instructions to use the app."),
                                 h3("DISCLAIMER"),
                                 p("In order to have fresh data, this app loads
                                   a more than 100k lines file directly from the
                                   WHO website, thats why first load takes more 
                                   than 20 seconds, but the rest of 
                                   manipulations are immediate."))
            )
        )
    )
))
