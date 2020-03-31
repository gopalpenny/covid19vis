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
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    column(12,align='center',
           h1(textOutput("maintab_title"))
           # titlePanel("COVID19: United States")
    ),

    fluidRow(
        column(3,offset=0,align='center',
               HTML("<br/>"),
               selectInput('maintab',NULL,c("World"="world","United States"="us"),selected = "world")
        ),
        column(4,offset=1,align='center',
               HTML("<br/>"),
               htmlOutput("us_cases_summary")
        ),
        column(4,align='center',
               HTML("<br/>"),
               htmlOutput("us_deaths_summary")
        )
    ),
    hr(),

    fluidRow(
        column(6,
               h5(shiny::textOutput("tableheader")),
               DT::DTOutput("table")),
        column(6,
               h5("Map of total cases (hover for details)"),
               leaflet::leafletOutput("usmap",height=445)
        )
    ),

    hr(),
    fluidRow(
        column(8,h4("Timeseries plots (data filtered by map)"),offset = 4)
    ),


    # Sidebar with a slider input for number of bins
    fluidRow(
        column(4,
               p(" "),
               p(" "),
               h5("Plot options"),
               fluidRow(column(4,
                               numericInput("ngroup","N states",min=1,max=50,value=8),
                               selectInput("smooth","Smooth",c("No","Yes")),
                               selectInput("yscale","Y scale",c("Linear","Log 10"),selected = "Log 10")),
                        column(8,
                               selectInput("yaxis","Y axis",c("Cases (daily)","Cases (total)","Deaths (daily)","Deaths (total)","Cases (% change)","Deaths (% change)")),
                               selectInput("xaxis","X axis",c("Last 30 days","Days since 100th case","Days since 25th death")),
                               selectInput("rankname","Rank by",c("Cases (absolute)","Deaths (absolute)","Cases (% change)","Deaths (% change)"))
                        ))
        ),
        column(8,
               plotly::plotlyOutput("plot")
        )
    )
))
