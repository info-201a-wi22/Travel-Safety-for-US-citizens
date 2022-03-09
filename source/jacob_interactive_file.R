library(tidyverse)
library("shiny")
library(ggplot2)

death_data <- read.csv("../data/export.csv")
world_travel_data <- read.csv("../data/2018_Country_travel_data.csv")

death_data <- data.frame(lapply(death_data, function(x) {
  gsub("Veh. Accident-Auto", "Veh. Accid-Auto", x)
}))

death_by_cause <- death_data %>%
  group_by(Cause.of.Death) %>% 
  summarize(number_of_death = NROW(Cause.of.Death))

death_data <- death_data %>%
  mutate(Year = str_sub(Date,start = -4))

dd_veh <- death_data %>%
  filter(Cause.of.Death == "Veh. Accid-Auto")
dd_year_veh <- dd_veh %>%
  group_by(Year) %>%
  summarize(number_of_death_veh = NROW(Cause.of.Death))

dd_hom <- death_data %>%
  filter(Cause.of.Death == "Homicide")
dd_year_hom <- dd_hom %>%
  group_by(Year) %>%
  summarize(number_of_death_hom = NROW(Cause.of.Death))

dd_sui <- death_data %>%
  filter(Cause.of.Death == "Suicide")
dd_year_sui <- dd_sui %>%
  group_by(Year) %>%
  summarize(number_of_death_sui = NROW(Cause.of.Death))

dd_drow <- death_data %>%
  filter(Cause.of.Death == "Drowning")
dd_year_drow <- dd_drow %>%
  group_by(Year) %>%
  summarize(number_of_death_drow = NROW(Cause.of.Death))

dd_combined <- left_join(dd_year_veh, dd_year_hom, by = "Year")

dd_combined <- left_join(dd_combined, dd_year_sui, by = "Year")

dd_combined <- left_join(dd_combined, dd_year_drow, by = "Year")

dd_combined <- data.frame(lapply(dd_combined, function(x) {
  strtoi(x)
}))

Summary_paragraph <- "In this graph, our team graphed the 4 most common ways to die overseas. From this graph we can see that in recent years, these 4 causes of death, along with death overseas in general has slowed down. We understand this to be due to Covid limiting travel overall. Another important observation in overall change of cause of death is in 2014/2015 in which Homicide was decreasing while other forms of death were increasing. Homocide has always been the largest cause of death overseas, so this decrease should indicate that human on human violence is decreasing overall. Drowning and suicide have been large reasons of death overseas which means making sure you are mentally and physically prepared to travel could lower your chance of death by a significant amount."
Jacob_page <- tabPanel(
  "Changes In Death Cause",
  titlePanel("How has cause of death changed? How to stay safe?"),
  mainPanel(
    sliderInput("year_input", label = h3("Year Range"), min = 2004,
                max = 2020, value = c(2005, 2019), step = 1),
    plotOutput("my_chart"),
    p(Summary_paragraph)
  )
)

my_ui <- navbarPage(
  "My Application",
  Jacob_page
)


my_server <- function(input, output) {
  output$my_chart <- renderPlot({
    ggplot(dd_combined, aes(Year)) +
      geom_line(aes(y = number_of_death_veh), colour = "Red") +
      geom_line(aes(y = number_of_death_drow), colour = "Blue") +
      geom_line(aes(y = number_of_death_sui), colour = "Green") +
      geom_line(aes(y = number_of_death_hom), colour = "Black") +
      ggtitle("Top Causes of Deaths Over Years") +
      ylab("Amount of Deaths") +
      xlab("Year / Red = Accident / Blue = Drowning / Green = Suicide / Black = Homicide") +
      xlim(input$year_input) +
      labs(color = "Labels")
  })
}

shinyApp(ui = my_ui, server = my_server)

