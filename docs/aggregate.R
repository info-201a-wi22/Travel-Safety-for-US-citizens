library(dplyr)

grouped <- group_by(export, City)

death_per_city <- export %>%
  group_by(City) %>%
  summarize(
    num_of_death = nrow(City)
  )