## code to prepare `rnatural_worldmap` dataset goes here

rnatural_worldmap <- rnaturalearth::ne_countries(scale="small",returnclass = "sf") %>%
  select(id=adm0_a3) #%>%
  # bind_cols(rnatural_worldmap %>%
  #             sf::st_centroid() %>% sf::st_coordinates() %>%
  #             tibble::as_tibble() %>% setNames(c("lon","lat"))) # %>%
  # filter(id=="FRA")
# ggplot2::ggplot(rnatural_worldmap) +
#   ggplot2::geom_point(ggplot2::aes(lat,lon,color=id=="FRA"))
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data=rnatural_worldmap %>% filter(id=="FRA"),ggplot2::aes(lat,lon))

usethis::use_data(rnatural_worldmap)
