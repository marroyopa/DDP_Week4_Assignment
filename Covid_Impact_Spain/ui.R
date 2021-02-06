#
# This is the user-interface definition for the Shiny web application that will
# evaluate Covid-19 evolution among two countries or two world regions that the  
# user will select.
#
# The original data can be found on the official page of the World Health 
# Organization:
#
#    https://covid19.who.int/WHO-COVID-19-global-data.csv
#

library(shiny)
library(plotly)
library(dplyr)
library(lubridate)



# Define UI for application that draws the different graphs for every tab

shinyUI(fluidPage(

    shinyjs::useShinyjs(),
    
    # Application title
    titlePanel("Covid Impact Comparison"),

    # Load and treat necessary data to run the App
    if(exists("data") == FALSE) {
        data <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", 
                         sep = ",", dec = ".")
        data <- rename(data, Date_reported = ï..Date_reported)
        data$Date_reported <- as.Date(data$Date_reported)
        areas <- matrix(c("EMRO", "Eastern Mediterranean", "EURO", 
                          "Europe", "AFRO", "Africa", "WPRO", 
                          "Western Pacific", "AMRO", "Americas", 
                          "SEARO", "South-East Asia", "Other", "Other"),
                        ncol=2 ,byrow=TRUE)
        colnames(areas) <- c("Cod","region")
        areas <- as.data.frame(areas)
        data <- merge(x = data, y = areas, by.x = "WHO_region", 
                      by.y = "Cod")
        maxdate <- max(data$Date_reported)
        mindate <- min(data$Date_reported)
    }
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("Period", "Date From / To:", min = mindate, 
                        max = maxdate, value = c(mindate, maxdate)),
            radioButtons("Territory", "Region or Countries", 
                         choices = c("Regions", "Countries")),
            selectInput("Region1", "Select Region 1", 
                        choices = distinct(data, region), selected = "Europe"),
            selectInput("Region2", "Select Region 2", 
                        choices = distinct(data, region), selected = "Africa"),
            selectInput("Country1", "Select Country 1", 
                        choices = distinct(data, Country), selected = "Germany"),
            selectInput("Country2", "Select Country 2", 
                        choices = distinct(data, Country), selected = "Spain"),
            radioButtons("CasesDeath", "Cases or Deaths:", 
                         choices = c("Cases", "Deaths"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Instructions", br(), 
                                 h2("Instructions to use the App"),
                                 h3("Objectives"),
                                 p("The objective of this App is to evaluate the
                                   impact of Covid in Spain by comparing the 
                                   deaths in 2020 or 2021 compared with 2019. 
                                   This results can be splited by Autonomous 
                                   Comunity (Region), Gender and week ending."),
                                 h3("How to use"),
                                 p("On the side bar pannel you can chose:"),
                                 p("- Year: The year you want to know the impact
                                   of the Covid-19. For the moment, obviously 
                                   only 2020 and 2021."),
                                 p("- Week until: The week until you want to
                                    see the data. From 1 to 53 in case of 2020,
                                    and only first and second week in case of 
                                    2021."),
                                 p("- Spanish Region: Plots calculated by
                                   the app can be particularized by Autonomous
                                   Community or Total Spain."),
                                 p("     - Gender: Plots calculated by
                                   the app can be particularized by gender or,
                                   total if both selected."),
                                 h3("Tab Contents"),
                                 p("Content in tabs is:"),
                                 p("- Weekly: Weekly excess deaths comparing
                                   with 2019 data in all weeks available for
                                   sex and region selected."),
                                 p("- Accumulated: Weekly Accumulated excess
                                   deaths comparing with 2019 data in all weeks
                                   available for sex and region selected."),       
                                 p("- By Age Group: Accumulated excess 
                                    deaths comparing with 2019 data by Age 
                                    Group, separing Male and Female from the
                                    first week until the week selected.")),
                        tabPanel("Daily", br(), plotlyOutput("dailyPlot")), 
                        tabPanel("Accumulated", br(), plotlyOutput("accumPlot")) 
                )
        )
    ))
)
