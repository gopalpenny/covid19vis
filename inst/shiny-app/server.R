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
library(DT)

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

    covid_data_us <- readr::read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) %>%
      left_join(us_states %>% dplyr::select(lat=latitude,lon=longitude,state=name),by="state") %>%
      na.omit() %>%
      group_by(state) %>% arrange(state,date) %>%
      mutate(cases_daily=cases-lag(cases),
             deaths_daily=deaths-lag(deaths),
             cases_pct_change=ifelse(cases>cases_daily,cases_daily/(cases-cases_daily)*100,0),
             deaths_pct_change=ifelse(deaths>deaths_daily,deaths_daily/(deaths-deaths_daily)*100,0),
             cum_cases_100_lgl=cases>=100,
             cum_deaths_25_lgl=deaths>=25,
             days30=if_else(as.numeric(Sys.Date()-date)<=30,date,as.Date(NA))) %>%
      group_by(state,cum_cases_100_lgl) %>%
      mutate(cases100days=ifelse(cum_cases_100_lgl,row_number()-1,NA)) %>%
      group_by(state,cum_deaths_25_lgl) %>%
      mutate(deaths25days=ifelse(cum_deaths_25_lgl,row_number()-1,NA)) %>%
      group_by()
    covid_totals_us <- covid_data_us %>%
      left_join(covid19vis::us_states %>% dplyr::select(abbrev=state,state=name),by="state") %>%
      arrange(state,date) %>%
      dplyr::group_by(state,abbrev,lat,lon,fips) %>%
      dplyr::summarize(cases=last(cases),
                       deaths=last(deaths),
                       cases_daily=last(cases_daily),
                       deaths_daily=last(deaths_daily),
                       cases_pct_change=last(cases_pct_change),
                       deaths_pct_change=last(deaths_pct_change),
                       last_date=last(date)) %>%
      mutate(summary=paste0(abbrev," +",format(cases_daily,big.mark = ",")," (+",round(cases_pct_change),"%)")) %>%
      group_by() %>%
      # cases
      dplyr::arrange(desc(cases)) %>%
      dplyr::mutate(rank_cases=dplyr::row_number(),
                    rank_cases_state=factor(paste0(rank_cases,". ",abbrev),levels=paste0(rank_cases,". ",abbrev))) %>%
      # deaths
      dplyr::arrange(desc(deaths)) %>%
      dplyr::mutate(rank_deaths=dplyr::row_number(),
                    rank_deaths_state=factor(paste0(rank_deaths,". ",abbrev),levels=paste0(rank_deaths,". ",abbrev))) %>%
      # change in cases
      dplyr::arrange(desc(cases_pct_change)) %>%
      dplyr::mutate(rank_cases_change=dplyr::row_number(),
                    rank_cases_change_state=factor(paste0(rank_cases_change,". ",abbrev),levels=paste0(rank_cases_change,". ",abbrev))) %>%
      # change in deaths
      dplyr::arrange(desc(deaths_pct_change)) %>%
      dplyr::mutate(rank_deaths_change=dplyr::row_number(),
                    rank_deaths_change_state=factor(paste0(rank_deaths_change,". ",abbrev),levels=paste0(rank_deaths_change,". ",abbrev)))

    states <- USAboundaries::us_states() %>% dplyr::rename(fips=statefp) %>%
      dplyr::left_join(covid_totals_us,by="fips")
    states_labels <- sprintf(
      "<strong>%s</strong><br/>%g (+%g) cases<br/>%g (+%g) deaths",
      states$name, states$cases, states$cases_daily, states$deaths, states$deaths_daily
    ) %>% lapply(htmltools::HTML)
    covid_us_summary <- covid_totals_us %>%
      select(cases,deaths,cases_daily,deaths_daily) %>%
      group_by() %>%
      summarize_all(sum) %>%
      mutate(cases_pct_change=cases_daily/(cases-cases_daily),
             deaths_pct_change=deaths_daily/(deaths-deaths_daily))



    output$us_cases_summary <- renderText({
      text <- with(covid_us_summary,
           paste0("<strong>Total cases</strong><br/>",format(cases,big.mark = ",")," (+",
                  format(cases_daily,big.mark = ","),", ",format(cases_pct_change,big.mark = ",",digits = 3),"%)"))
      HTML(text)
    })
    output$us_deaths_summary <- renderText({
      text <- with(covid_us_summary,
                   paste0("<strong>Total deaths</strong><br/>",format(deaths,big.mark = ",")," (+",
                          format(deaths_daily,big.mark = ","),", ",format(deaths_pct_change,big.mark = ",",digits = 3),"%)"))
      HTML(text)
    })


    output$usmap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # leafem::addMouseCoordinates() %>%
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

    output$table <- renderDT({
      if (input$maintab == "us") {
        covid_totals <- covid_totals_us
      } else if (input$maintab == "world") {
        covid_totals <- covid_totals_world
      }

      covid_DT <- prep_covid_DT(covid_totals)

      covid_DT
    })

    output$plot <- renderPlotly({
      # debug:
      # y_axis_name <- "cases_daily"
      # x_axis_name <- "days30"
      # rank_name <- "rank_cases_state"
      # map_bounds <- list(north=44.5,south=28.8,east=-77.7,west=-117.7)
      # input <- list(ngroup=8)

      foo <- "bar"

      if (input$maintab == "us") {
        covid_totals <- covid_totals_us
        covid_data <- covid_data_us
        map_bounds <- input$usmap_bounds
      } else if (input$maintab == "world") {
        covid_totals <- covid_totals_world
        covid_data <- covid_data_world
        map_bounds <- input$worldmap_bounds
      }
      print('covid_data')
      print(covid_data)
      if (!is.null(covid_data) & !is.null(covid_totals)) {

        y_axis_name <- case_when(
          input$yaxis == "Cases (daily)" ~ "cases_daily",
          input$yaxis == "Cases (total)" ~ "cases",
          input$yaxis == "Cases (% change)" ~ "cases_pct_change",
          input$yaxis == "Deaths (daily)" ~ "deaths_daily",
          input$yaxis == "Deaths (total)" ~ "deaths",
          input$yaxis == "Deaths (% change)" ~ "deaths_pct_change"
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
        # var_name <- "state"

        covid_top <- covid_totals %>%
          rename(rank = !!rank_name) %>%
          dplyr::filter(lat>map_bounds$south,lat<map_bounds$north,
                        lon>map_bounds$west,lon<map_bounds$east) %>%
          dplyr::arrange(rank) %>%
          dplyr::slice(1:input$ngroup)

        covid_plot_data_prep <- covid_data %>%
          rename(yvar = !!y_axis_name, xvar = !!x_axis_name) %>%
          dplyr::filter(state %in% covid_top$state) %>%
          dplyr::select(xvar,yvar,state) %>%
          group_by()

        if(input$smooth=="Yes") {
          print("smoothing...")
          covid_plot_data_prep <- covid_plot_data_prep %>%
            group_by(state) %>% arrange(state,xvar) %>%
            mutate(yvar=as.numeric(stats::filter(yvar,rep(1,7),sides=1))) %>%
            group_by()
        }

        covid_plot_data <- covid_plot_data_prep %>%
          filter(!is.na(yvar), !is.na(xvar)) %>%
          left_join(covid_top %>% select(state,rank),by="state")

        plot <- ggplot(covid_plot_data,aes(xvar,yvar,color=rank)) + geom_line() + geom_point() +
          labs(y=input$yaxis,x=input$xaxis) +
          theme_bw() %+replace% theme(legend.title = element_blank())

        if (input$yscale == "Log 10") {
          plot <- plot + scale_y_log10()
        }
        plotly::ggplotly(plot)
      } else {
        p_empty <- ggplot(data.frame(x=1,y=1,label="Loading...")) +
          geom_text(aes(x,y,label=label))
        plotly::ggplotly(p_empty)
      }
    })

    output$maintab_title <- renderText({
      scale_text <- dplyr::case_when(
        input$maintab == "us"~"United States",
        input$maintab == "world"~"World"
      )
      paste("COVID19:",scale_text)
    })

    output$tableheader <- renderText({
      last_update <- covid_totals_us$last_date %>% max() %>% strftime("%b %d, %Y")
      tableheader <- paste0("Latest data from the NY Times (",last_update,")")
      tableheader
    })

})
