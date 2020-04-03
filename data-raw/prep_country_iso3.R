## code to prepare `country_iso3` dataset goes here

country_centroids <- readr::read_csv("ignore/country_centroids_az8.csv") %>%
  select(abbrev=adm0_a3,pop_est,gdp_md_est,pop_year,lat=Latitude,lon=Longitude)

world_cases_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
world_deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
cov_raw <- prep_covid_raw_world(world_cases_url,world_deaths_url)

# get country codes
country_iso3 <- dplyr::distinct(cov_raw[,c("name","abbrev")]) %>%
  left_join(country_centroids,by="abbrev")

usethis::use_data(country_iso3)
