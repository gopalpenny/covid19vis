# prep_covid_data.R

#' Prepare covid data
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
prep_covid_data <- function(covid_data_prep) {
  covid_data <- covid_data_prep %>%
    na.omit() %>%
    group_by(name) %>% arrange(name,date) %>%
    mutate(cases_daily=cases-lag(cases),
           deaths_daily=deaths-lag(deaths),
           cases_pct_change=ifelse(cases>cases_daily,cases_daily/(cases-cases_daily)*100,0),
           deaths_pct_change=ifelse(deaths>deaths_daily,deaths_daily/(deaths-deaths_daily)*100,0),
           cum_cases_100_lgl=cases>=100,
           cum_deaths_25_lgl=deaths>=25,
           days30=if_else(as.numeric(Sys.Date()-date)<=30,date,as.Date(NA))) %>%
    group_by(name,cum_cases_100_lgl) %>%
    mutate(cases100days=ifelse(cum_cases_100_lgl,row_number()-1,NA)) %>%
    group_by(name,cum_deaths_25_lgl) %>%
    mutate(deaths25days=ifelse(cum_deaths_25_lgl,row_number()-1,NA)) %>%
    group_by()
  return(covid_data)
}

#' Download and prep world data
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
prep_covid_raw_world <- function() {
  covid_data_world_cases_prep <- readr::read_csv(url(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  )) %>%
    tidyr::gather(date,cases,ends_with("20")) %>%
    rename(name=`Country/Region`,lat=Lat,lon=Long) %>% group_by(name,date) %>%
    summarize(cases=sum(cases)) %>% dplyr::mutate(date=as.Date(date,format="%m/%d/%y"))
  covid_data_world_deaths_prep <- readr::read_csv(url(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  )) %>%
    tidyr::gather(date,deaths,ends_with("20")) %>%
    rename(name=`Country/Region`,lat=Lat,lon=Long) %>% group_by(name,date) %>%
    summarize(deaths=sum(deaths),
              lat=mean(lat),
              lon=mean(lon)) %>%
    dplyr::mutate(date=as.Date(date,format="%m/%d/%y"))
  covid_data_world_prep <- covid_data_world_cases_prep %>%
    left_join(covid_data_world_deaths_prep,by=c("name","date")) %>%
    dplyr::mutate(id=countrycode::countrycode(name,origin="country.name",destination = "iso3c"),
                  abbrev=id) %>%
    dplyr::arrange(name,date)
  return(covid_data_world_prep)
}

#' Prepare covid data totals
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
prep_covid_totals <- function(covid_data) {

  covid_totals <- covid_data %>%
    arrange(name,date) %>%
    dplyr::group_by(name,abbrev,lat,lon,id) %>%
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
                  rank_cases_name=factor(paste0(rank_cases,". ",abbrev),levels=paste0(rank_cases,". ",abbrev))) %>%
    # deaths
    dplyr::arrange(desc(deaths)) %>%
    dplyr::mutate(rank_deaths=dplyr::row_number(),
                  rank_deaths_name=factor(paste0(rank_deaths,". ",abbrev),levels=paste0(rank_deaths,". ",abbrev))) %>%
    # change in cases
    dplyr::arrange(desc(cases_pct_change)) %>%
    dplyr::mutate(rank_cases_change=dplyr::row_number(),
                  rank_cases_change_name=factor(paste0(rank_cases_change,". ",abbrev),levels=paste0(rank_cases_change,". ",abbrev))) %>%
    # change in deaths
    dplyr::arrange(desc(deaths_pct_change)) %>%
    dplyr::mutate(rank_deaths_change=dplyr::row_number(),
                  rank_deaths_change_name=factor(paste0(rank_deaths_change,". ",abbrev),levels=paste0(rank_deaths_change,". ",abbrev)))
  return(covid_totals)
}
