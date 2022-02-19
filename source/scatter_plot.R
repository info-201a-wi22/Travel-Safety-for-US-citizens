library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
dataset <- read.csv("https://raw.githubusercontent.com/info-201a-wi22/Travel-Safety-for-US-citizens/main/data/export.csv")
world_travel_data <- read.csv("https://raw.githubusercontent.com/info-201a-wi22/Travel-Safety-for-US-citizens/main/data/2018_Country_travel_data.csv")
death_per_city <- dataset %>%
  group_by(City) %>%
  summarise(
    num_of_death = NROW(City)
  )


death_per_city <- death_per_city %>%
  mutate(Country = word(City, -1))

joined <- left_join(death_per_city, world_travel_data, by = "Country")

joined2 <- joined %>%
  group_by(Country) %>%
  summarize(
    num_of_death = NROW(Country)
  )

joined2 <- left_join(joined2, world_travel_data, by = "Country")

joined2 <- joined2 %>% mutate_all(~replace(., is.na(.), 1))

joined2 <- joined2 %>% mutate(travel_to_death_ratio = num_of_death/Number_of_visits)

joined2 <- joined2 %>% filter(travel_to_death_ratio < 0.238095238)

joined2 <- joined2 %>% arrange(travel_to_death_ratio)

attach(joined2)
plot(travel_to_death_ratio, Number_of_visits, main="90 countries",
     xlab="Number of deaths", ylab="Number of visits in 2018", pch=19)
