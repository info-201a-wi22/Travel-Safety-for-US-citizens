library(tidyverse)
dataset <- read.csv("https://raw.githubusercontent.com/info-201a-wi22/Travel-Safety-for-US-citizens/main/data/export.csv")
summaryinfo <- list()
summaryinfo$num_of_causes <- length(unique(dataset$Cause.of.Death))

summaryinfo$most_recent_death_date <- dataset %>% 
  filter(Date == max(Date, na.rm = TRUE)) %>%
  filter(Cause.of.Death == "Drowning") %>%
  pull(Date)

death_count_by_cause <- table(dataset$Cause.of.Death)
most_death_count <- max(death_count_by_cause)
summaryinfo$most_common_death_cause  <- names(death_count_by_cause)[which(death_count_by_cause == most_death_count)]

summaryinfo$num_of_locations <- length(unique(dataset$City))

death_count_by_date <- table(dataset$Date)
summaryinfo$daily_death_avg <- mean(death_count_by_date)

Print(summaryinfo)
