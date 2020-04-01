## code to prepare `country_iso3` dataset goes here

cov_raw <- prep_covid_raw_world()

# get country codes
country_iso3 <- dplyr::distinct(cov_raw[,c("name","abbrev")])

usethis::use_data(country_iso3)
