## code to prepare `rnatural_worldmap` dataset goes here


rnatural_worldmap <- rnaturalearth::ne_countries(scale="small",returnclass = "sf") %>%
  select(id=adm0_a3) #%>%
  # left_join(covid_totals_world,by="id") %>%
  # filter(!is.na(name))

usethis::use_data(rnatural_worldmap)
