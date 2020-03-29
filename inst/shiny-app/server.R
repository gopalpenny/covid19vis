#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(covid19vis)
library(dplyr)

# if (!interactive()) {
#   dir <- "./"
# } else {
#   dir <- "inst/shiny-app"
# }
dir <- "./"


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    covid_data <- readr::read_csv(file.path(dir,"covid-19-data/us-states.csv")) %>%
      dplyr::left_join(us_states %>% dplyr::select(lat=latitude,lon=longitude,state=name),by="state") %>%
      na.omit()
    covid_totals <- covid_data %>%
      dplyr::group_by(state,lat,lon) %>%
      dplyr::summarize(cases=sum(cases),
                       deaths=sum(deaths)) %>%
      group_by() %>%
      dplyr::arrange(desc(cases)) %>%
      dplyr::mutate(rank_cases=dplyr::row_number()) %>%
      dplyr::arrange(desc(deaths)) %>%
      dplyr::mutate(rank_deaths=dplyr::row_number())

    output$usmap <- renderLeaflet({
      leaflet() %>%
        addTiles(options = providerTileOptions(noWrap = TRUE), group="Map") %>%
        leafem::addMouseCoordinates() %>%
        # addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
        addScaleBar(position = c("bottomright"), options = scaleBarOptions()) %>%
        # addLayersControl(baseGroups = c("Map"),#overlayGroups = c("Red","Blue") ,
        #                  options = layersControlOptions(collapsed = FALSE)) %>%
        leaflet::addCircles(~lon,~lat,~cases,data=covid_totals) %>%
        setView(lng = -97.7129, lat = 37.0902, zoom=4)
    })

    observe({
      us_bounds <- input$usmap_bounds
      us_bounds <- list(north=44.5,south=28.8,east=-77.7,west=-117.7)
      print(input$usmap_bounds)

      covid_top <- covid_totals %>%
        dplyr::filter(lat>us_bounds$south,lat<us_bounds$north,
                      lon>us_bounds$west,lon<us_bounds$east) %>%
        dplyr::arrange(desc(cases)) %>%
        slice(1:4) #input$ngroup
      covid_map <- covid_data %>%
        dplyr::filter(state %in% covid_top$state)

      plot <- ggplot(covid_map) %>%

    })

})
