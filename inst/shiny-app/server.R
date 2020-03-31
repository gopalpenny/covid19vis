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

  # Get and prepare data
  ### US DATA
  covid_data_us_prep <- readr::read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) %>%
    rename(name=state,id=fips) %>%
    left_join(us_states %>% dplyr::select(abbrev=state,lat=latitude,lon=longitude,name),by="name")
  covid_data_us <- prep_covid_data(covid_data_us_prep)
  # covid_data <- covid_data_us
  covid_totals_us <- prep_covid_totals(covid_data_us)

  ### WORLD DATA
  covid_data_world_prep <- prep_covid_raw_world()
  covid_data_world <- prep_covid_data(covid_data_world_prep)
  # covid_data <- covid_data_us
  covid_totals_world <- prep_covid_totals(covid_data_world) %>% dplyr::arrange(name)

  # Reactive data objects -- change depending on maintab
  covid_data <- reactive({
    if (input$maintab=="us") {
      covid_data <- covid_data_us
    } else if (input$maintab=="world") {
      covid_data <- covid_data_world
    }
    covid_data
  })

  covid_totals <- reactive({
    if (input$maintab=="us") {
      covid_totals <- covid_totals_us
    } else if (input$maintab=="world") {
      covid_totals <- covid_totals_world
    }
    covid_totals
  })

  covid_summary <- reactive({
    covid_summary <- covid_totals() %>%
      select(cases,deaths,cases_daily,deaths_daily) %>%
      group_by() %>%
      summarize_all(sum) %>%
      mutate(cases_pct_change=cases_daily/(cases-cases_daily)*100,
             deaths_pct_change=deaths_daily/(deaths-deaths_daily)*100)
  })

  map_data <- reactive({
    if (input$maintab=="us") {
      map_data <- USAboundaries::us_states() %>% dplyr::rename(id=statefp) %>% dplyr::select(-name) %>%
        dplyr::left_join(covid_totals_us,by=c("id"))
    } else if (input$maintab=="world") {
      # covid_totals <- covid_totals_world
    }
    map_data
  })

  map_labels <- reactive({
    map_data_df <- map_data()
    map_labels <- sprintf(
      "<strong>%s</strong><br/>%g (+%g) cases<br/>%g (+%g) deaths",
      map_data_df$name, map_data_df$cases, map_data_df$cases_daily, map_data_df$deaths, map_data_df$deaths_daily
    ) %>% lapply(htmltools::HTML)
    map_labels
  })

  output$us_cases_summary <- renderText({
    text <- with(covid_summary(),
                 paste0("<strong>Total cases</strong><br/>",format(cases,big.mark = ",")," (+",
                        format(cases_daily,big.mark = ","),", ",format(cases_pct_change,big.mark = ",",digits = 3),"%)"))
    HTML(text)
  })
  output$us_deaths_summary <- renderText({
    text <- with(covid_summary(),
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
      addPolygons(data=map_data(),stroke=TRUE,weight=2,opacity=0.3,fillOpacity = 0.3,
                  fillColor = ~pal(cases),color=~pal(cases),
                  # highlightOptions = highlightOptions(
                  #   weight = 5,
                  #   color = "#666",
                  #   dashArray = "",
                  #   fillOpacity = 0.7,
                  #   bringToFront = TRUE),
                  label=map_labels()) %>%
      # addLayersControl(baseGroups = c("Map"),#overlayGroups = c("Red","Blue") ,
      #                  options = layersControlOptions(collapsed = FALSE)) %>%
      # leaflet::addCircles(~lon,~lat,~log(cases)*1e4,data=covid_totals,stroke=FALSE) %>%
      addLegend(position = "bottomright",pal=pal,data=map_data(),values = ~cases) %>%
      setView(lng = -97.7129, lat = 37.0902, zoom=3)
  })

  output$table <- renderDT({
    covid_DT <- prep_covid_DT(covid_totals())
    covid_DT
  })

  output$plot <- renderPlotly({
    # debug:
    # y_axis_name <- "cases_daily"
    # x_axis_name <- "days30"
    # rank_name <- "rank_cases_name"
    # map_bounds <- list(north=44.5,south=28.8,east=-77.7,west=-117.7)
    # input <- list(ngroup=8)

    foo <- "bar"

    if (input$maintab == "us") {
      map_bounds <- input$usmap_bounds
    } else if (input$maintab == "world") {
      map_bounds <- input$worldmap_bounds
    }
    print('covid_data')
    print(covid_data())
    if (!is.null(covid_data()) & !is.null(covid_totals())) {

      y_axis_name <- case_when(
        input$yaxis == "Cases (daily)" ~ "cases_daily",
        input$yaxis == "Cases (total)" ~ "cases",
        input$yaxis == "Cases (% change)" ~ "cases_pct_change",
        input$yaxis == "Deaths (daily)" ~ "deaths_daily",
        input$yaxis == "Deaths (total)" ~ "deaths",
        input$yaxis == "Deaths (% change)" ~ "deaths_pct_change"
      )
      rank_name <- case_when(
        input$rankname == "Cases (absolute)" ~ "rank_cases_name",
        input$rankname == "Deaths (absolute)" ~ "rank_deaths_name",
        input$rankname == "Cases (% change)" ~ "rank_cases_change_name",
        input$rankname == "Deaths (% change)" ~ "rank_deaths_change_name"
      )
      x_axis_name <- case_when(
        input$xaxis == "Last 30 days" ~ "days30",
        input$xaxis == "Days since 100th case" ~ "cases100days",
        input$xaxis == "Days since 25th death" ~ "deaths25days"
      )
      # var_name <- "state"

      covid_top <- covid_totals() %>%
        rename(rank = !!rank_name) %>%
        dplyr::filter(lat>map_bounds$south,lat<map_bounds$north,
                      lon>map_bounds$west,lon<map_bounds$east) %>%
        dplyr::arrange(rank) %>%
        dplyr::slice(1:input$ngroup)

      covid_plot_data_prep <- covid_data() %>%
        rename(yvar = !!y_axis_name, xvar = !!x_axis_name) %>%
        dplyr::filter(name %in% covid_top$name) %>%
        dplyr::select(xvar,yvar,name) %>%
        group_by()

      if(input$smooth=="Yes") {
        print("smoothing...")
        covid_plot_data_prep <- covid_plot_data_prep %>%
          group_by(name) %>% arrange(name,xvar) %>%
          mutate(yvar=as.numeric(stats::filter(yvar,rep(1,7),sides=1))) %>%
          group_by()
      }

      covid_plot_data <- covid_plot_data_prep %>%
        filter(!is.na(yvar), !is.na(xvar)) %>%
        left_join(covid_top %>% select(name,rank),by="name")

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
