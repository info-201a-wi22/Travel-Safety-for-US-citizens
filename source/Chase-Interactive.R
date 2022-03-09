library(shiny)
library(dplyr)
library(ggplot2)
TD_2018 <- read.csv("../data/2018_Country_travel_data.csv")
DATA <- TD_2018 %>% arrange(desc(Number_of_visits))
Visitation <- head(DATA, 15)

Summary_paragraph <- "In this graph, we showed how many people visited in 2019 and compared that between the top 15 destinations. It shows us that Mexico is a very big outlier and this should likely be taken into consideration when calculating risk factor. With this information you will be able to evaluate the relevance of deaths when it comes to the most traveled to cities. This data is also likely skewed due to COVID as Mexico has laxer laws when it comes to vaccination status and masks, it is likely that is why they tower above other countries. "

server <- function(input, output, session) {
  
  output$plot <- renderPlot({Visitation
  

      
    barplot(Visitation$Number_of_visits)
  gplot <- ggplot(Visitation, aes(y = Number_of_visits, x = Country))
  gplot + geom_bar(stat = "sum")
    
  })
  
}

ui <- basicPage(
  h1("What has Visitation Looked like Recently?"),
  selectInput(inputId = "Sel_Country",
              label = "Choose Country",
              list("Alberta", "Bahamas", "Brazil", "British Columbia","Japan", "Dominican Republic", "France", "Germany", "Mexico", "Ontario", "United Kingdom", "Jamaica", "Netherlands", "Colombia")),
  plotOutput("plot"),
  p(Summary_paragraph)
)

shinyApp(ui = ui, server = server)
