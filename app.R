library(tidyverse)
library("shiny")
library(ggplot2)
library(plotly)
  
death_data <- read.csv("data/export.csv")
world_travel_data <- read.csv("data/2018_Country_travel_data.csv")

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


Year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
Mexico <- c(20.02, 19.92, 20.37, 20.55, 25.88, 28.73, 31.19, 34.95, 36.37, 39.94, 21.73)
Canada <- c(11.75, 11.56, 11.85, 12.01, 11.51, 12.47, 13.89, 14.33, 14.34, 14.99, 1.93)
UK <- c(2.88, 2.76, 2.75, 2.98, 3.54, 3.95, 4.63, 4.57, 4.50, 1.92)
Japan <- c(5.85, 8.36, 10.36, 13.41, 19.74, 24.04, 28.69, 31.19, 31.88, 4.12) 
Germany <- c(1.65, 1.82, 1.96, 2.12, 2.37, 2.42, 2.56, 2.63, 2.70, 2.78, 0.600)

my_data <- as.data.frame(cbind(Year,Mexico,Canada,UK,Japan,Germany))

death_data_by_year <- death_data %>%
  group_by(Year) %>%
  summarize(deaths = NROW(Year))

death_data_by_year <- data.frame(lapply(death_data_by_year, function(x) {
  strtoi(x)
}))
Summary_paragraph_irene <- "In this graph you can see how many deaths each country had in that country by year. You can select a country from among the countries where US citizens have traveled a lot, and as you select a country, you can see how many deaths occurred in that country each year, as well as trends such as whether the number of deaths increased or decreased. By analyzing the trend, we can identify which country has gotten safer recently and which country has gotten more dangerous. There might be two countries which hae the same amount of death occured within the whole data. However, whether there has been a lot of death occured in the past (less recently) or there has been a lot of death occured currently (less in the past) makes U.S. citizens make a significantly different choice. Therefore data that analyzes the trend of the number of death occured is necessary to be analyzed."


Summary_paragraph_jacob <- "In this graph, our team graphed the 4 most common ways to die overseas. From this graph we can see that in recent years, these 4 causes of death, along with death overseas in general has slowed down. We understand this to be due to Covid limiting travel overall. Another important observation in overall change of cause of death is in 2014/2015 in which Homicide was decreasing while other forms of death were increasing. Homocide has always been the largest cause of death overseas, so this decrease should indicate that human on human violence is decreasing overall. Drowning and suicide have been large reasons of death overseas which means making sure you are mentally and physically prepared to travel could lower your chance of death by a significant amount."

Summary_paragraph_chase <- "In this table, we showed how many people visited in 2019 and compared that between the top 15 destinations. It shows us that Mexico is a very big outlier and this should likely be taken into consideration when calculating risk factor. With this information you will be able to evaluate the relevance of deaths when it comes to the most traveled to cities. This data is also likely skewed due to COVID as Mexico has laxer laws when it comes to vaccination status and masks, it is likely that is why they tower above other countries. "

Introduction <- "The major question we are trying to answer is: How can you stay safe overseas? There is no straightforward answer to this question, so our team used data from the US government to try and answer a few different avenues that this question may be answered along with some other important data. Questions such as, when is it best to travel? What should we watch out for when traveling? Which countries have gotten more safe or dangerous?"

Conclusion_paragraph <- "From our research we have come to a few answers to the questions posed in the introduction page. It is found that drowning and suicide are an increasing danger during travel, so making sure you are mentally and physically prepared to travel will greatly decrease your chance of dying. We also found that certain countries have had recent dips in deaths overseas which may be an indication for safety. We also found that deaths overseas have been slightly lower overall and it may be a good time to travel."

Jacob_page <- tabPanel(
  "Changes In Death Cause",
  titlePanel("How has cause of death changed? How to stay safe?"),
  mainPanel(
    sliderInput("year_input", label = h3("Year Range"), min = 2004,
                max = 2020, value = c(2005, 2019), step = 1),
    plotOutput("my_chart"),
    p(Summary_paragraph_jacob)
  )
)

Jacob_page2 <- tabPanel(
  ("How has overall death changed?"),
  titlePanel("Change In Safety"),
  mainPanel(
    sliderInput("year_input_2", label = h3("Year Range"), min = 2004,
                max = 2020, value = c(2005, 2019), step = 1),
  plotOutput("plot"),
  p(Summary_paragraph_irene)
  )
)

Chase_page <- tabPanel(
  ("Visitation Changes"),
  titlePanel("What has Visitation Looked like Recently"),
  sidebarLayout(
    sidebarPanel(
      helpText("Number of Vistors between 2010-2020"),
      
      selectInput("var", 
                  label = "Choose a Country",
                  choices = c("Mexico",
                              "Canada",
                              "UK",
                              "Japan",
                              "Germany"),
                  selected = "Mexico"),
      
      sliderInput("range", 
                  label = "Choose a start and end year:",
                  min = min(my_data$Year), max = max(my_data$Year), value = c(2010, 2020),sep = "",)
    ),
    
    mainPanel(
      tableOutput("DataTable"),
      p(Summary_paragraph_chase)
    )
  )
)

Intro <- tabPanel(
  ("Our Project"),
  titlePanel("Our Project"),
  mainPanel(
    p(Introduction),
    img(src='Image.png', align = "right")
  )
)

Conclusion <- tabPanel(
  ("Conclusion"),
  titlePanel("Conclusion"),
  mainPanel(
    p(Conclusion_paragraph)
  )
)

my_ui <- navbarPage(
  "My Application",
  Intro,
  Jacob_page,
  Jacob_page2,
  Chase_page,
  Conclusion
)


my_server <- function(input, output, session) {
  output$my_chart <- renderPlot({
    ggplot(dd_combined, aes(Year)) +
      geom_line(aes(y = number_of_death_veh), colour = "Red") +
      geom_line(aes(y = number_of_death_drow), colour = "Blue") +
      geom_line(aes(y = number_of_death_sui), colour = "Green") +
      geom_line(aes(y = number_of_death_hom), colour = "Black") +
      ggtitle("Most Common Death Trends") +
      ylab("Amount of Deaths") +
      xlab("Year / Red = Accident / Blue = Drowning / Green = Suicide / Black = Homicide") +
      xlim(input$year_input) +
      labs(color = "Labels")

  })
  output$plot <- renderPlot({
    ggplot(death_data_by_year, aes(Year)) +
      geom_line(aes(y = deaths)) +
      xlim(input$year_input_2)
  })
  output$DataTable <- renderTable({
    dt <- my_data[my_data$Year >= input$range[1] & my_data$Year <= input$range[2],]
    dt[,c("Year",input$var)]
  },include.rownames=FALSE)
}

shinyApp(ui = my_ui, server = my_server)
