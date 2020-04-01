# prep_covid_data.R

#' Prepare covid data
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
prep_covid_data <- function(covid_data_prep) {
  covid_data <- covid_data_prep %>%
    na.omit() %>%
    dplyr::group_by(name) %>% dplyr::arrange(name,date) %>%
    dplyr::mutate(cases_daily=cases-dplyr::lag(cases),
           deaths_daily=deaths-dplyr::lag(deaths),
           cases_pct_change=ifelse(cases>cases_daily,cases_daily/(cases-cases_daily)*100,0),
           deaths_pct_change=ifelse(deaths>deaths_daily,deaths_daily/(deaths-deaths_daily)*100,0),
           cum_cases_100_lgl=cases>=100,
           cum_deaths_25_lgl=deaths>=25,
           days30=dplyr::if_else(as.numeric(Sys.Date()-date)<=30,date,as.Date(NA))) %>%
    dplyr::group_by(name,cum_cases_100_lgl) %>%
    dplyr::mutate(cases100days=ifelse(cum_cases_100_lgl,dplyr::row_number()-1,NA)) %>%
    dplyr::group_by(name,cum_deaths_25_lgl) %>%
    dplyr::mutate(deaths25days=ifelse(cum_deaths_25_lgl,dplyr::row_number()-1,NA)) %>%
    dplyr::group_by()
  return(covid_data)
}

#' Download and prep US data
#'
#' Download and prep US data from JHU CSSE
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#' @examples
#' cases_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
#' deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
#' cov_raw <- prep_covid_raw_us(cases_url,deaths_url)
prep_covid_raw_us <- function(cases_url,deaths_url) {
  # cases_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  # deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
  abbrev_df <- us_states
  covid_data_us_cases_prep <- readr::read_csv(url(cases_url)) %>%
    tidyr::gather(date,cases,ends_with("20")) %>%
    dplyr::rename(name=Province_State,lat=Lat,lon=Long_) %>% dplyr::group_by(name,date) %>%
    dplyr::summarize(cases=sum(cases)) %>% dplyr::mutate(date=as.Date(date,format="%m/%d/%y"))
  covid_data_us_deaths_prep <- readr::read_csv(url(deaths_url)) %>%
    tidyr::gather(date,deaths,ends_with("20")) %>%
    dplyr::rename(name=Province_State,lat=Lat,lon=Long_) %>% dplyr::group_by(name,date) %>%
    dplyr::summarize(deaths=sum(deaths),
                     lat=mean(lat),
                     lon=mean(lon)) %>%
    dplyr::mutate(date=as.Date(date,format="%m/%d/%y"))
  covid_data_us_prep <- covid_data_us_cases_prep %>%
    dplyr::left_join(covid_data_us_deaths_prep,by=c("name","date")) %>%
    dplyr::left_join(abbrev_df,by="name") %>%
    dplyr::mutate(id=abbrev) %>%
    dplyr::arrange(name,date)
  # covid_data_us_prep %>% dplyr::select(name,abbrev) %>% dplyr::distinct()
  return(covid_data_us_prep)
}

#' Download and prep world data
#'
#' Download and prep world data from JHU CSSE
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#' @examples
#' cov_raw <- prep_covid_raw_world()
prep_covid_raw_world <- function(cases_url,deaths_url) {
  abbrev_df <- country_iso3
  covid_data_world_cases_prep <- readr::read_csv(url(cases_url)) %>%
    tidyr::gather(date,cases,ends_with("20")) %>%
    dplyr::rename(name=`Country/Region`,lat=Lat,lon=Long) %>% dplyr::group_by(name,date) %>%
    dplyr::summarize(cases=sum(cases)) %>% dplyr::mutate(date=as.Date(date,format="%m/%d/%y"))
  covid_data_world_deaths_prep <- readr::read_csv(url(deaths_url)) %>%
    tidyr::gather(date,deaths,ends_with("20")) %>%
    dplyr::rename(name=`Country/Region`,lat=Lat,lon=Long) %>% dplyr::group_by(name,date) %>%
    dplyr::summarize(deaths=sum(deaths),
              lat=mean(lat),
              lon=mean(lon)) %>%
    dplyr::mutate(date=as.Date(date,format="%m/%d/%y"))
  covid_data_world_prep <- covid_data_world_cases_prep %>%
    dplyr::left_join(covid_data_world_deaths_prep,by=c("name","date")) %>%
    dplyr::left_join(abbrev_df,by="name") %>%
    dplyr::mutate(id=abbrev) %>%
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
    dplyr::arrange(name,date) %>%
    dplyr::group_by(name,abbrev,lat,lon,id) %>%
    dplyr::summarize(cases=dplyr::last(cases),
                     deaths=dplyr::last(deaths),
                     cases_daily=dplyr::last(cases_daily),
                     deaths_daily=dplyr::last(deaths_daily),
                     cases_pct_change=dplyr::last(cases_pct_change),
                     deaths_pct_change=dplyr::last(deaths_pct_change),
                     last_date=dplyr::last(date)) %>%
    dplyr::mutate(summary=paste0(abbrev," +",format(cases_daily,big.mark = ",")," (+",round(cases_pct_change),"%)")) %>%
    dplyr::group_by() %>%
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
