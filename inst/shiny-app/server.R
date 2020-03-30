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
library(magrittr)
library(ggplot2)
library(plotly)
library(sf)

# if (!interactive()) {
#   dir <- "./"
# } else {
#   dir <- "inst/shiny-app"
# }
dir <- "./"

bins <- 10^c(1:5)
# class(states)
pal <- colorBin("YlOrRd", domain = c(0,1e6), bins = bins)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    covid_data <- readr::read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) %>%
      left_join(us_states %>% dplyr::select(lat=latitude,lon=longitude,state=name),by="state") %>%
      na.omit() %>%
      group_by(state) %>% arrange(state,date) %>%
      mutate(cases_daily=cases-lag(cases),
             deaths_daily=deaths-lag(deaths),
             cases_change=ifelse(cases>cases_daily,cases_daily/(cases-cases_daily),0),
             deaths_change=ifelse(deaths>deaths_daily,deaths_daily/(deaths-deaths_daily),0),
             cum_cases_100_lgl=cases>=100,
             cum_deaths_25_lgl=deaths>=25,
             days30=if_else(as.numeric(Sys.Date()-date)<=30,date,as.Date(NA))) %>%
      group_by(state,cum_cases_100_lgl) %>%
      mutate(cases100days=ifelse(cum_cases_100_lgl,row_number()-1,NA)) %>%
      group_by(state,cum_deaths_25_lgl) %>%
      mutate(deaths25days=ifelse(cum_deaths_25_lgl,row_number()-1,NA)) %>%
      group_by()
    covid_totals <- covid_data %>%
      arrange(state,date) %>%
      dplyr::group_by(state,lat,lon,fips) %>%
      dplyr::summarize(cases=last(cases),
                       deaths=last(deaths),
                       cases_change=last(cases_change),
                       deaths_change=last(deaths_change)) %>%
      group_by() %>%
      # cases
      dplyr::arrange(desc(cases)) %>%
      dplyr::mutate(rank_cases=dplyr::row_number(),
                    rank_cases_state=factor(paste0(rank_cases,". ",state),levels=paste0(rank_cases,". ",state))) %>%
      # deaths
      dplyr::arrange(desc(deaths)) %>%
      dplyr::mutate(rank_deaths=dplyr::row_number(),
                    rank_deaths_state=factor(paste0(rank_deaths,". ",state),levels=paste0(rank_deaths,". ",state))) %>%
      # change in cases
      dplyr::arrange(desc(cases_change)) %>%
      dplyr::mutate(rank_cases_change=dplyr::row_number(),
                    rank_cases_change_state=factor(paste0(rank_cases_change,". ",state),levels=paste0(rank_cases_change,". ",state))) %>%
      # change in deaths
      dplyr::arrange(desc(deaths_change)) %>%
      dplyr::mutate(rank_deaths_change=dplyr::row_number(),
                    rank_deaths_change_state=factor(paste0(rank_deaths_change,". ",state),levels=paste0(rank_deaths_change,". ",state)))

    states <- USAboundaries::us_states() %>% dplyr::rename(fips=statefp) %>%
      dplyr::left_join(covid_totals,by="fips")
    states_labels <- sprintf(
      "<strong>%s</strong><br/>%g cases<br/>%g deaths",
      states$name, states$cases, states$deaths
    ) %>% lapply(htmltools::HTML)

    output$usmap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        leafem::addMouseCoordinates() %>%
        # addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
        addScaleBar(position = c("bottomright"), options = scaleBarOptions()) %>%
        addPolygons(data=states,stroke=TRUE,weight=2,opacity=0.3,fillOpacity = 0.3,
                    fillColor = ~pal(cases),color=~pal(cases),
                    # highlightOptions = highlightOptions(
                    #   weight = 5,
                    #   color = "#666",
                    #   dashArray = "",
                    #   fillOpacity = 0.7,
                    #   bringToFront = TRUE),
                    label=states_labels) %>%
        # addLayersControl(baseGroups = c("Map"),#overlayGroups = c("Red","Blue") ,
        #                  options = layersControlOptions(collapsed = FALSE)) %>%
        # leaflet::addCircles(~lon,~lat,~log(cases)*1e4,data=covid_totals,stroke=FALSE) %>%
        addLegend(position = "bottomright",pal=pal,data=states,values = ~cases) %>%
        setView(lng = -97.7129, lat = 37.0902, zoom=3)
    })

    output$plot <- renderPlotly({
      y_axis_name <- case_when(
        input$yaxis == "Cases (absolute)" ~ "cases",
        input$yaxis == "Deaths (absolute)" ~ "deaths",
        input$yaxis == "Cases (% change)" ~ "cases_change",
        input$yaxis == "Deaths (% change)" ~ "deaths_change"
      )
      rank_name <- case_when(
        input$rankname == "Cases (absolute)" ~ "rank_cases_state",
        input$rankname == "Deaths (absolute)" ~ "rank_deaths_state",
        input$rankname == "Cases (% change)" ~ "rank_cases_change_state",
        input$rankname == "Deaths (% change)" ~ "rank_deaths_change_state"
      )
      x_axis_name <- case_when(
        input$xaxis == "Last 30 days" ~ "days30",
        input$xaxis == "Days since 100th case" ~ "cases100days",
        input$xaxis == "Days since 25th death" ~ "deaths25days"
      )
      var_name <- "state"
      covid_totals


      us_bounds <- input$usmap_bounds
      # us_bounds <- list(north=44.5,south=28.8,east=-77.7,west=-117.7)
      # print(input$usmap_bounds)

      # pal <- colorNumeric(
      #   palette = "Blues",
      #   domain = countries$gdp_md_est)

      covid_top <- covid_totals %>%
        rename(rank = !!rank_name) %>%
        dplyr::filter(lat>us_bounds$south,lat<us_bounds$north,
                      lon>us_bounds$west,lon<us_bounds$east) %>%
        dplyr::arrange(rank) %>%
        dplyr::slice(1:input$ngroup)
      covid_map <- covid_data %>%
        rename(yvar = !!y_axis_name, xvar = !!x_axis_name) %>%
        dplyr::filter(state %in% covid_top$state, !is.na(yvar), !is.na(xvar)) %>%
        dplyr::select(xvar,yvar,state) %>%
        left_join(covid_top %>% select(state,rank),by="state")


      plot <- ggplot(covid_map,aes(xvar,yvar,color=rank)) + geom_line() + geom_point() +
        labs(y=input$yaxis,x=input$xaxis) +
        theme_bw() %+replace% theme(legend.title = element_blank())

      if (input$yscale == "Log 10") {
        plot <- plot + scale_y_log10()
      }
      plotly::ggplotly(plot)
    })

})
