## code to prepare `us_states` dataset goes here

us_states <- readr::read_csv("ignore/us_states_lat_lon.csv")

usethis::use_data(us_states)
