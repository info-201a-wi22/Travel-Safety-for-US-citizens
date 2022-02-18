install.packages("dplyr")

library(dplyr)

grouped <- group_by(export, City)

death_per_city <- export %>%
  group_by(City) %>%
  summarise(
    num_of_death = NROW(City)
  )

grouped <- group_by(export, `Cause of Death`)

types_of_death <- export %>%
  group_by(`Cause of Death`) %>%
  summarise(
    num_per_cause = NROW(`Cause of Death`)
  )

death_per_city
types_of_death