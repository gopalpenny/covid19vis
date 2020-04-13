#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyMobile)
# library(echarts4r)
library(shinyWidgets)

# Define UI for application that draws a histogram
f7Page(
    title = "My app",
    init = f7Init(theme = "dark"),
    f7TabLayout(
        panels = tagList(
            f7Panel(title = "Scale", side = "left", theme = "light", effect = "cover",
                    # HTML("<br/>"),
                    selectInput('maintab',NULL,c("World"="world","United States"="us"),selected = "world")
                    # HTML("<br/>"),
                    # htmlOutput("us_cases_summary"),
                    # HTML("<br/>"),
            ),
            f7Panel(title = "Plot options", side = "right", theme = "dark", effect = "cover",
                    sliderInput("ngroup","N states",min=1,max=20,value=10),
                    sliderInput("ndays","N days",min=10,max=as.numeric(Sys.Date() - as.Date("2020-02-01")),value=as.numeric(Sys.Date() - as.Date("2020-03-01"))),
                    radioButtons("yaxis_val","Y axis value",c("Cases","Deaths"),selected="Cases",inline=TRUE),
                    radioButtons("yaxis_type","Y axis type",c("Total","New","% change"),selected="Total",inline=TRUE),
                    checkboxGroupInput("plotoptions",NULL,c("log(y)","7-day avg"),selected="log(y)",inline=TRUE),
                    radioButtons("xaxis","X axis",c("Last 30 days","Days since Nth"),inline=TRUE),
                    radioButtons("rankname","Rank by",c("Cases (Total)","Deaths (Total)","Cases (New)","Deaths (New)"),inline=TRUE)
            )
        ),
        navbar = f7Navbar(
            title = textOutput("maintab_title"),
            hairline = TRUE,
            shadow = TRUE,
            left_panel = TRUE,
            right_panel = TRUE
        ),
        f7Tabs(
            animated = TRUE,
            #swipeable = TRUE,
            f7Tab(
                tabName = "Overview",
                icon = f7Icon("list_number"),
                active = TRUE,

                # f7Flex(
                #     prettyRadioButtons(
                #         inputId = "theme",
                #         label = "Select a theme:",
                #         thick = TRUE,
                #         inline = TRUE,
                #         selected = "md",
                #         choices = c("ios", "md"),
                #         animation = "pulse",
                #         status = "info"
                #     ),
                #
                #     prettyRadioButtons(
                #         inputId = "color",
                #         label = "Select a color:",
                #         thick = TRUE,
                #         inline = TRUE,
                #         selected = "dark",
                #         choices = c("light", "dark"),
                #         animation = "pulse",
                #         status = "info"
                #     )
                # ),

              #   shiny::tags$head(
              #       shiny::tags$script(
              #           'Shiny.addCustomMessageHandler("ui-tweak", function(message) {
              #   var os = message.os;
              #   var skin = message.skin;
              #   if (os === "md") {
              #     $("html").addClass("md");
              #     $("html").removeClass("ios");
              #     $(".tab-link-highlight").show();
              #   } else if (os === "ios") {
              #     $("html").addClass("ios");
              #     $("html").removeClass("md");
              #     $(".tab-link-highlight").hide();
              #   }
              #
              #   if (skin === "dark") {
              #    $("html").addClass("theme-dark");
              #   } else {
              #     $("html").removeClass("theme-dark");
              #   }
              #
              #  });
              # '
              #       )
              #   ),

                f7Shadow(
                    intensity = 10,
                    hover = TRUE,
                    f7Card(
                        title = textOutput("tableheader"),
                        # sliderTextInput(
                        #     inputId = "by",
                        #     label = "Date Selector:",
                        #     choices = c("day", "week", "month"),
                        #     selected = "day"
                        # ),
                        htmlOutput("us_cases_summary"),
                        br(),
                        htmlOutput("us_deaths_summary"),
                        br(),
                        # h5(shiny::textOutput("tableheader")),
                        DT::DTOutput("table")
                        # footer = tagList(
                        #     f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
                        #     f7Badge("Badge", color = "green")
                        # )
                    )
                )
            ),
            f7Tab(
                tabName = "Map",
                icon = f7Icon("compass"),
                active = FALSE,
                f7Shadow(
                    intensity = 10,
                    hover = TRUE,
                    f7Card(
                        title = "Total cases",
                        # prettySwitch(
                        #     inputId = "show",
                        #     label = "Show Plot",
                        #     status = "danger"
                        # ),
                        leaflet::leafletOutput("usmap",height=445)

                        # echarts4rOutput("network"),
                        # footer = tagList(
                        #     f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
                        #     f7Badge("Badge", color = "green")
                        # )
                    )
                )
            ),
            f7Tab(
                tabName = "Plot",
                icon = f7Icon("graph_square"),
                active = FALSE,
                f7Shadow(
                    intensity = 10,
                    hover = TRUE,
                    f7Card(
                        title = "Timeseries (filtered by map)",
                        # prettyCheckboxGroup(
                        #     "variable",
                        #     "Variables to show:",
                        #     c("Cylinders" = "cyl",
                        #       "Transmission" = "am",
                        #       "Gears" = "gear"),
                        #     inline = TRUE,
                        #     status = "danger",
                        #     animation = "pulse"
                        # ),
                        plotly::plotlyOutput("plot",height = "100%")
                        # tableOutput("data"),
                        # footer = tagList(
                        #     f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
                        #     f7Badge("Badge", color = "green")
                        # )
                    )
                )
            )
        )
    )
)
