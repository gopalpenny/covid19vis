## code to prepare `usaboundaries_usstates` dataset goes here

usaboundaries_usstates <- USAboundaries::us_states() %>% dplyr::rename(id=statefp) %>% dplyr::select(-name)

usethis::use_data(usaboundaries_usstates)
