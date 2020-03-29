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
        column(3,numericInput("ngroup","Number of states",min=1,max=50,value=8)),
        column(3,selectInput("yscale","Y scale",c("Linear","Log 10"))),
        # column(3,checkboxInput("us_logy","Log y",value=T)),
        column(3,selectInput("yaxis","Y axis",c("Cases (absolute)","Deaths (absolute)","Cases (% change)","Deaths (% change)"))),
        column(3,selectInput("xaxis","X axis",c("Last 30 days","Days since 100th case","Days since 25th death"))),
        column(3,selectInput("rankname","Rank by",c("Cases (absolute)","Deaths (absolute)","Cases (% change)","Deaths (% change)")))
    ),

    fluidRow(
        column(6,
               leaflet::leafletOutput("usmap",height=450)
        ),
        column(6,
               plotly::plotlyOutput("plot")
        )
    )
))
