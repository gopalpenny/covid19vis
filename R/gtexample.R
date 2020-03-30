tab_1 <-
  exibble %>%
  gt(
    rowname_col = "row",
    groupname_col = "group"
  )

gt()


df_tab <- covid_totals %>%
  # filter() %>%
  arrange(rank_cases_state) %>%
  mutate(i=(rank_cases + 12) %% 13,
         j=(rank_cases + 12) %/% 13) %>%
  select(summary,j,i) %>%
  tidyr::spread(j,summary)
df_tab %>% gt(rowname_col="i") %>%
  data_color(columns=vars(`1`),colors='#333333')



  ggplot() + geom_raster(aes(j,i))
