
library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

death_data <- read.csv("../data/export.csv")

death_by_cause <- death_data %>%
  group_by(Cause.of.Death) %>% 
  summarize(number_of_death = NROW(Cause.of.Death))

death_data <- death_data %>%
  mutate(Year = str_sub(Date,start = 0))



Summary_paragraph <- "In this graph you can see how many deaths each country had in that country by year. You can select a country from among the countries where US citizens have traveled a lot, and as you select a country, you can see how many deaths occurred in that country each year, as well as trends such as whether the number of deaths increased or decreased. By analyzing the trend, we can identify which country has gotten safer recently and which country has gotten more dangerous. There might be two countries which hae the same amount of death occured within the whole data. However, whether there has been a lot of death occured in the past (less recently) or there has been a lot of death occured currently (less in the past) makes U.S. citizens make a significantly different choice. Therefore data that analyzes the trend of the number of death occured is necessary to be analyzed."



server <- function(input, output, session) {
  
  output$plot <- renderPlot({death_data
    
    barplot(as.matrix(death_data$number_of_death))
    gplot <- ggplot(death_data, aes(y = number_of_death, x = Year))
    gplot + geom_bar(stat = "sum")
    
  })
  
}


ui <- basicPage(
  h1("How has the safety of each country changed throughout the years?"),
  selectInput(inputId = "Sel_Country",
              label = "Select a Country",
              list("Afghanistan", "Bahamas", "Brazil", 
                   "Cambodia","Canada", "France", "Germany", 
                   "Mexico", "United Kingdom")),
              plotOutput("plot"),
              p(Summary_paragraph)
)



shinyApp(ui = ui, server = server)




















