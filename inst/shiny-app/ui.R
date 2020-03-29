#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(leaflet)
library(covid19vis)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID-19 Map"),

    # Sidebar with a slider input for number of bins
    fluidRow(
        sliderInput("ngroup","Number of states",min=1,max=50,value=10)
    ),

    fluidRow(
        leaflet::leafletOutput("usmap",height=450)
    )
))
