#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyMobile)
# library(echarts4r)
library(shinyWidgets)
# library(leaflet)
library(covid19vis)
library(dplyr)
# library(magrittr)
# library(ggplot2)
# library(plotly)
# library(readr)
library(sf)

dir <- "./"
formnumber <- function(x, ...) {
    gsub(" ","",format(x, ...))
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


    # Get and prepare data
    ### US DATA
    # # NY Times data (no longer used)
    # covid_data_us_prep1 <- readr::read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) %>%
    #   rename(name=state,id=fips) %>%
    #   left_join(us_states %>% dplyr::select(abbrev,lat=latitude,lon=longitude,name),by="name")

    # # CSSE data from Johns Hopkins
    us_cases_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
    us_deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
    covid_data_us_prep <- prep_covid_raw_us(us_cases_url,us_deaths_url)

    covid_data_us <- prep_covid_data(covid_data_us_prep)
    covid_totals_us <- prep_covid_totals(covid_data_us)

    map_data_us <- usaboundaries_usstates %>% dplyr::rename(abbrev=stusps) %>%
        dplyr::left_join(covid_totals_us,by=c("abbrev"))

    ### WORLD DATA
    world_cases_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    world_deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    covid_data_world_prep <- prep_covid_raw_world(world_cases_url, world_deaths_url)
    covid_data_world <- prep_covid_data(covid_data_world_prep)
    # covid_data <- covid_data_us
    covid_totals_world <- prep_covid_totals(covid_data_world) %>% dplyr::arrange(name)

    map_data_world <- rnatural_worldmap %>%
        left_join(covid_totals_world,by="id") %>%
        filter(!is.na(name))

    ## additional data
    umin <- floor(log10(min(covid_totals_us$cases,na.rm=TRUE)))
    umax <- ceiling(log10(max(covid_totals_us$cases,na.rm=TRUE)))
    usbins <- 10^c(umin:umax)
    uspal <- leaflet::colorBin("YlOrRd", domain = c(10^umin,10^umax), bins = usbins)
    wmin <- floor(log10(min(covid_totals_world$cases,na.rm=TRUE)))
    wmax <- ceiling(log10(max(covid_totals_world$cases,na.rm=TRUE)))
    worldbins <- 10^c(wmin:wmax)
    worldpal <- leaflet::colorBin("YlOrRd", domain = c(10^wmin,10^wmax), bins = worldbins)

    ## additional data
    bin_width_max_pct <- 5
    usbins_c <- unique(c(0,
                  floor(quantile(covid_totals_us$cases_7day_change,seq(0.2,0.8,by=0.2))/bin_width_max_pct)*bin_width_max_pct,
                  ceiling(max(covid_totals_us$cases_7day_change,na.rm=TRUE)/bin_width_max_pct)*bin_width_max_pct))
    uspal_change <- leaflet::colorBin("YlOrRd", domain = c(min(usbins_c),max(usbins_c)), bins = usbins_c)
    worldbins_c <- unique(c(0,
                         floor(quantile(covid_totals_world$cases_7day_change,seq(0.2,0.8,by=0.2),na.rm=TRUE)/bin_width_max_pct)*bin_width_max_pct,
                         ceiling(max(covid_totals_world$cases_7day_change,na.rm=TRUE,na.rm=TRUE)/bin_width_max_pct)*bin_width_max_pct))
    worldpal_change <- leaflet::colorBin("YlOrRd", domain = c(min(worldbins_c),max(worldbins_c)), bins = worldbins_c)

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
            map_data <- map_data_us
        } else if (input$maintab=="world") {
            map_data <- map_data_world
        }
        map_data
    })

    map_labels <- reactive({
        map_data_df <- map_data()
        map_labels_vector <- paste0("<strong>",map_data_df$name,"</strong><br/>",
                                    formnumber(map_data_df$cases,big.mark=",",width=0)," (+",formnumber(map_data_df$cases_daily,big.mark=",",width=0),") cases<br/>",
                                    formnumber(map_data_df$deaths,big.mark=",",width=0)," (+",formnumber(map_data_df$deaths_daily,big.mark=",",width=0),") deaths<br/>",
                                    "7-day increase: ",round(map_data_df$cases_7day_change,1),"%<br/>")
        map_labels <- lapply(map_labels_vector,function(x) htmltools::HTML(x))
        # map_labels <- sprintf(
        #   "<strong>%s (%s)</strong><br/>%g (+%g) cases<br/>%g (+%g) deaths",
        #   map_data_df$name, map_data_df$abbrev, map_data_df$cases, map_data_df$cases_daily, map_data_df$deaths, map_data_df$deaths_daily
        # ) %>% lapply(htmltools::HTML)
        # print("map_data")
        # print(map_data_df)
        map_labels
        # print("map_labels")
        # print(map_labels)
    })

    mappal <- reactive({
        if (input$maintab=="us") {
            mappal <- uspal
        } else if (input$maintab=="world") {
            mappal <- worldpal
        }
        mappal
    })
    mappal_change <- reactive({
        if (input$maintab=="us") {
            mappal_change <- uspal_change
        } else if (input$maintab=="world") {
            mappal_change <- worldpal_change
        }
        mappal_change
    })

    output$us_cases_summary <- renderText({
        text <- with(covid_summary(),
                     paste0("<strong>Total cases</strong><br/>",format(cases,big.mark = ",")," (+",
                            format(cases_daily,big.mark = ","),", ",format(cases_pct_change,big.mark = ",",digits = 3),"%)"))
        # HTML(text)
        text
    })

    output$us_deaths_summary <- renderText({
        text <- with(covid_summary(),
                     paste0("<strong>Total deaths</strong><br/>",format(deaths,big.mark = ",")," (+",
                            format(deaths_daily,big.mark = ","),", ",format(deaths_pct_change,big.mark = ",",digits = 3),"%)"))
        HTML(text)
    })

    map_info <- reactiveValues(
        usbounds = list(north = 61.8, east = -60.3, south = 0.879, west = -135),
        worldbounds = NULL
        # bounds = NULL
    )

    # when bounds change, update bounds for US or World
    observeEvent(input$usmap_bounds,{
        if (input$maintab=="us") {
            map_info$usbounds <- input$usmap_bounds
            # map_info$bounds <- input$usmap_bounds
        } else if (input$maintab=="world") {
            map_info$worldbounds <- input$usmap_bounds
            map_info$bounds <- input$usmap_bounds
        }
    })

    # current bound are always stored in a reactive contect
    mapbounds <- reactive({
        if (input$maintab=="us") {
            mapbounds <- map_info$usbounds
        } else if (input$maintab=="world") {
            mapbounds <- map_info$worldbounds
        }
        mapbounds
    })

    observeEvent(input$maintab,{
        map_data_df <- map_data()
        pal <- mappal()
        pal_change <- mappal_change()
        map_bounds <- mapbounds()

        # if (input$maintab=="world") {
        #     total_group <- "Total cases global"
        #     increase_7day_group <- "7-day increase global"
        # } else if (input$maintab=="us") {
        total_group <- "Total cases"
        increase_7day_group <- "7-day increase"
        # }
        leaflet::leafletProxy("usmap") %>%
            leaflet::clearControls() %>%
            # leaflet::removeLayersControl() %>%
            leaflet::clearGroup("Total cases") %>%
            leaflet::clearGroup("7-day increase") %>%
            # leaflet::clearGroup("Total cases US") %>%
            # leaflet::clearGroup("7-day increase US") %>%
            # leaflet::addPolygons(data=map_data_df,stroke=TRUE,weight=2,opacity=0.3,fillOpacity = 0.3,
            #                      fillColor = ~pal(cases),color=~pal(cases),group=total_group,
            #                      # highlightOptions = highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),
            #                      label=map_labels()) %>%
            leaflet::addPolygons(data=map_data_df,stroke=TRUE,weight=2,opacity=0.3,fillOpacity = 0.3,
                                 fillColor = ~pal_change(cases_7day_change),color=~pal_change(cases_7day_change),group=increase_7day_group,
                                 label=map_labels()) %>%
            leaflet::addLegend(position = "bottomleft",pal=pal_change,data=map_data_df %>% filter(!is.na(cases_7day_change)),
                               values = ~cases_7day_change,title="7-day increase (%)", group=increase_7day_group) %>%
            # leaflet::addLegend(position = "bottomleft",pal=pal,data=map_data_df,values = ~cases, title=total_group,group=total_group) %>%
            leaflet::fitBounds(lng1 = map_bounds$west , lng2 = map_bounds$east, lat1 = map_bounds$south , lat2=map_bounds$north)# %>%
            # leaflet::hideGroup(total_group) %>%
            # leaflet::addLayersControl(overlayGroups = c(total_group,increase_7day_group))

    })


    output$usmap <- leaflet::renderLeaflet({
        leaflet::leaflet() %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            # leafem::addMouseCoordinates() %>%
            # addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
            leaflet::addScaleBar(position = c("bottomright"), options = leaflet::scaleBarOptions())%>%
            leaflet::setView(lng = 0, lat = 0, zoom=1) #%>%
            # leaflet::addLayersControl(overlayGroups = c("7-day increase"))
    })

    output$table <- DT::renderDT({
        # print(covid_totals())
        # if (TRUE) {
        #     foo <- "bar"
        #     foo <- 2
        # }
        # abc <- data.frame(a=1:5,b=6:10)
        # DT::datatable(abc)
        covid_DT <- prep_covid_DT_mobile(covid_totals()) %>%
            DT::formatStyle(1,backgroundColor = "#222222") %>%
            DT::formatStyle(2:5,color = "#222222") %>%
            DT::formatStyle(0:4,border='0.5px solid #111')
        covid_DT
    })

    output$plot <- plotly::renderPlotly({
        # debug:
        if(FALSE) {
            y_axis_name <- "cases_daily"
            x_axis_name <- "days30"
            rank_name <- "rank_cases_name"
            map_bounds <- list(north=44.5,south=0,east=-77.7,west=-117.7)
            input <- list(ngroup=8)
            cov_total <- covid_totals_world
            cov_data <- covid_data_world
        }
        foo <- "bar"

        print(str(input$usmap_bounds))

        map_bounds <- mapbounds()
        print(str(map_bounds))

        # cov_data <- covid_data()
        # print('covid_data')
        # print(covid_data())
        if (!is.null(map_bounds)) {
            cov_total <- covid_totals()
            cov_data <- covid_data()

            y_axis_name <- case_when(
                input$yaxis_val == "Cases"  & input$yaxis_type == "New" ~ "cases_daily",
                input$yaxis_val == "Cases"  & input$yaxis_type == "Total"  ~ "cases",
                input$yaxis_val == "Cases"  & input$yaxis_type == "7-day % change" ~ "cases_7day_change",
                input$yaxis_val == "Deaths" & input$yaxis_type == "New"  ~ "deaths_daily",
                input$yaxis_val == "Deaths" & input$yaxis_type == "Total"  ~ "deaths",
                input$yaxis_val == "Deaths" & input$yaxis_type == "7-day % change" ~ "deaths_7day_change"
            )
            rank_name <- case_when(
                input$rankname == "Total" & input$yaxis_val == "Cases"~ "rank_cases_name",
                input$rankname == "Total" & input$yaxis_val == "Deaths" ~ "rank_deaths_name",
                input$rankname == "New" & input$yaxis_val == "Cases" ~ "rank_cases_daily_name",
                input$rankname == "New" & input$yaxis_val == "Deaths" ~ "rank_deaths_daily_name",
                input$rankname == "Increase %" & input$yaxis_val == "Cases" ~ "rank_cases_7day_name",
                input$rankname == "Increase %" & input$yaxis_val == "Deaths" ~ "rank_deaths_7day_name"
            )
            x_axis_name <- case_when(
                input$xaxis == "Last N days" ~ "date",
                input$xaxis == "Days since Nth" & input$yaxis_val == "Cases" ~ "cases100days",
                input$xaxis == "Days since Nth" & input$yaxis_val == "Deaths" ~ "deaths25days"
            )
            x_axis_label <- case_when(
                input$xaxis == "Last N days" ~ paste("Last",input$ndays,"days"),
                input$xaxis == "Days since Nth" & input$yaxis_val == "Cases" ~ "Days since 100th case",
                input$xaxis == "Days since Nth" & input$yaxis_val == "Deaths" ~ "Days since 25th death"
            )

            if (x_axis_name == "date") {
                cov_data$date[Sys.Date() - cov_data$date > input$ndays] <- NA
            } else if (x_axis_name == "cases100days") {
                cov_data$cases100days[cov_data$cases100days > input$ndays] <- NA
            } else if (x_axis_name == "deaths25days") {
                cov_data$cases100days[cov_data$deaths25days > input$ndays] <- NA
            }

            # var_name <- "state"

            covid_top <- cov_total %>%
                rename(rank = !!rank_name) %>%
                dplyr::filter(lat>map_bounds$south,lat<map_bounds$north,
                              lon>map_bounds$west,lon<map_bounds$east) %>%
                dplyr::arrange(rank) %>%
                dplyr::slice(1:input$ngroup)

            covid_plot_data_prep <- cov_data
            covid_plot_data_prep$xvar <- cov_data[,x_axis_name][[1]]
            covid_plot_data_prep$yvar <- cov_data[,y_axis_name][[1]]
            covid_plot_data_prep <- covid_plot_data_prep %>%
                dplyr::filter(name %in% covid_top$name) %>%
                # dplyr::select(xvar,yvar,name) %>%
                group_by()
            # cov_data %>% mutate(xvar = !! x_axis_name,
            #                     yvar = vars(y_axis_name)) %>%

            # covid_plot_data_prep2 <- covid_data() %>%
            #   dplyr::filter(name %in% covid_top$name) %>%
            #   dplyr::select(!!y_axis_name,!!x_axis_name,name) %>%
            #   group_by()

            if("7-day avg" %in% input$plotoptions) {
                print("smoothing...")
                covid_plot_data_prep <- covid_plot_data_prep %>%
                    group_by(name) %>% arrange(name,date) %>%
                    mutate(yvar=as.numeric(stats::filter(yvar,rep(1/7,7),sides=1))) %>%
                    group_by()
            }

            covid_plot_data <- covid_plot_data_prep %>%
                filter(!is.na(yvar), !is.na(xvar)) %>%
                left_join(covid_top %>% select(name,rank),by="name")

            ### plotly
            if ("log(y)" %in% input$plotoptions) {
                covid_plot_data <- covid_plot_data %>% mutate(yvar = ifelse(yvar==0,NA,yvar))
            }
            plot1 <- plotly::plot_ly(type='scatter',mode='lines+markers')
            for (i in 1:nrow(covid_top)) {
                plot_data_df <- covid_plot_data %>% filter(name==covid_top$name[i])
                plot1 <- plot1 %>%
                    plotly::add_trace(data=plot_data_df,
                                      x=~xvar,y=~yvar,name=~rank,
                                      text=~paste0('<b>',date,'</b>',
                                                   '<br>',name,' (',abbrev,')',
                                                   '<br>',formnumber(cases,big.mark = ","),' (+',formnumber(cases_daily,big.mark = ","),') cases',
                                                   '<br>',formnumber(deaths,big.mark = ","),' (+',formnumber(deaths_daily,big.mark = ","),') deaths'),
                                      hoverinfo='text')
            }

            avg_7_note <- ifelse("7-day avg" %in% input$plotoptions,", 7-day avg","")
            plot_title <- paste0(input$yaxis_val," (",input$yaxis_type,"), ranked by ", input$rankname,"\n")
            x_list <- list(title = x_axis_label, linecolor = "#AAAAAA",tickcolor = "#AAAAAA")
            y_list <- list(title = paste0(input$yaxis_val," (",input$yaxis_type,avg_7_note,")"),
                           linecolor = "#AAAAAA",tickcolor = "#AAAAAA")
            if ("log(y)" %in% input$plotoptions) {
                y_list <- list(title = paste0(input$yaxis_val," (log ",input$yaxis_type,avg_7_note,")"),type="log")
            }
            plot1 <- plot1 %>% plotly::layout(yaxis = y_list, xaxis = x_list) %>%
                plotly::layout(margin=list(t=100),legend = list(orientation = 'h',y = 1,yanchor="bottom"),
                               plot_bgcolor='black',paper_bgcolor="black",font=list(color="#FFFFFF"))

            ###

            # plot <- ggplot2::ggplot(covid_plot_data,ggplot2::aes(xvar,yvar,color=rank)) +
            #   ggplot2::geom_line() + ggplot2::geom_point() +
            #   ggplot2::labs(y=input$yaxis,x=input$xaxis) +
            #   ggplot2::`%+replace%`(ggplot2::theme_bw(),ggplot2::theme(legend.title = ggplot2::element_blank()))
            #
            # if (input$logy) {
            #   plot <- plot + ggplot2::scale_y_log10()
            # }
            # plotly::ggplotly(plot) %>%
            #   plotly::layout(legend = list(orientation = 'h',
            #                                y = 1,
            #                                yanchor="bottom"))
            plot1
        } else {
            # p_empty <- ggplot2::ggplot(data.frame(x=1,y=1,label="Loading...")) +
            #   ggplot2::geom_text(ggplot2::aes(x,y,label=label))
            # plotly::ggplotly(p_empty)
            plot_null <- NULL
            plot_null
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
        covid_tot <- covid_totals()
        last_update <- covid_tot$last_date %>% max() %>% strftime("%B %d, %Y")
        if (input$maintab=="us") {
            # data_source <- "the NY Times"
            data_source <- "JHU CSSE"
        } else if (input$maintab=="world") {
            data_source <- "JHU CSSE"
        }
        # tableheader <- paste0("Latest data from ",data_source," (",last_update,")")
        tableheader <- paste0(last_update," (",data_source,")")
        tableheader
    })


    output$plotcard_header <- renderText({
        paste0(input$yaxis_type, " ", input$yaxis_val, ", ranked by ", input$rankname)
    })


    ###########    ###########    ###########    ###########    ###########    ###########    ###########

    ###########    ###########    ###########    ###########    ###########    ###########    ###########

    ###########    ###########    ###########    ###########    ###########    ###########    ###########

    ###########    ###########    ###########    ###########    ###########    ###########    ###########

    ###########    ###########    ###########    ###########    ###########    ###########    ###########

    ###########    ###########    ###########    ###########    ###########    ###########    ###########

})
