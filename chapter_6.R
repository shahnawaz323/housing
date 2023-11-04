h <- function(name, food_list = list(), seed = 123){

  # We set the seed, making sure that we get
  # the same selection of food for a given seed

  set.seed(seed)
  food <- sample(c("lasagna", "cassoulet", "feijoada"), 1)

  # We now need to unset the seed, because
  # if we don't, guess what, the seed will
  # stay set for the whole session!

  set.seed(NULL)

  food_list <- append(food_list, food)

  print(paste0(name, " likes ", food))

  food_list
}

h('shah')
ls()



# Using the group_nest from dplyr and map2 from purr to make plots

data("population")

glimpse(population)

# nesting
nested_unemp <- population %>% dplyr::filter(country %in% c('Pakistan','India')) %>%
  group_nest(country)

# ploting and saving plots as entries in the filtered dataframe
nested_unemp <- nested_unemp %>%
  mutate(plots2 = map2(
    .x = data,
    .y = country,
    .f = \(.x,.y)(
      ggplot(data = .x) +
        ggthemes::theme_base() +
        geom_line(
          aes(year, population, color='red')
        ) +
        geom_point(aes(year, population), size=5)+
        labs(title = paste("Growth of Population in", .y))+
        theme(legend.position = 'none')
    )
  )
  ) %>%
  pull(plots2)

# saving each entry of plots2 column as separate plot
ggsave('population in pak.png', nested_unemp[[2]],dpi=300, height = 10, width = 15)
ggsave('population in india.png', nested_unemp[[1]],dpi=300, height = 10, width = 15)
