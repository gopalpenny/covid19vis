# covid_table.R

#' Prepare covid DataTable
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
prep_covid_DT <- function(covid_totals) {
  covid_table <- covid_totals %>%
    dplyr::mutate(`+C%`=round(.data$cases_pct_change,1),
           `+D%`=round(.data$deaths_pct_change,1)) %>%
    dplyr::select(` `=.data$abbrev,
           Cases=.data$cases,
           `+C`=.data$cases_daily,
           .data$`+C%`,
           D=.data$deaths,
           `+D`=.data$deaths_daily) %>%
    dplyr::arrange(desc(.data$Cases))


  color_breaks <- covid_table %>%
    dplyr::summarize_at(dplyr::vars(.data$Cases,.data$`+C`,.data$D,.data$`+D`),
                 function(x) list(exp(seq(log(max(c(min(x,na.rm=T)-0.1,0.1))),log(max(x,na.rm=T)+0.5),length.out=19))))
  color_breaks <- color_breaks %>% dplyr::bind_cols(
    covid_table %>%
      dplyr::summarize_at(dplyr::vars(contains("%")),function(x) list(seq(min(x)-0.1,max(x)+0.5,length.out=19)))
  )

  # US summary in same format
  # covid_us_summary <- covid_table_states %>% mutate(` `="US") %>%
  #   group_by(` `) %>% summarize_if(is.numeric,sum) %>%
  #   mutate(`+C%`=round(`+C`/(Cases-`+C`) * 100),1)

  colors <- colorspace::heat_hcl(n=20,c=c(50,100),l=c(100,60),h=c(60,10))
  # colorspace::swatchplot(colors)
  DT::datatable(covid_table,rownames=F,
                options = list(searching=FALSE,
                               # formatNumber= formatNumber(),
                               lengthChange=FALSE,
                               pageLength = 10,
                               language.thousands=",",
                               pagingType="simple",
                               class="compact cell-border",
                               # initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '10px'});}"),
                               autoWidth = TRUE
                               # columnDefs = list(list(width = '80%', targets = c(2,6)))#,
                )
  ) %>%
    DT::formatRound(c("Cases","+C","D","+D"),digits = 0) %>%
    DT::formatStyle("Cases",background = DT::styleInterval(color_breaks$Cases[[1]],colors)) %>%
    DT::formatStyle("+C",background = DT::styleInterval(color_breaks$`+C`[[1]],colors)) %>%
    DT::formatStyle("+C%",background = DT::styleInterval(color_breaks$`+C%`[[1]],colors)) %>%
    DT::formatStyle("D",background = DT::styleInterval(color_breaks$D[[1]],colors)) %>%
    DT::formatStyle("+D",background = DT::styleInterval(color_breaks$`+D`[[1]],colors)) #%>%
  # formatStyle("+D%",background = styleInterval(color_breaks$`+D%`[[1]],colors)) %>%
  # formatStyle(columns = rep(T,7), fontSize = '10px')
}
