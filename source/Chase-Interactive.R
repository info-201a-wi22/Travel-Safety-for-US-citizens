library(shiny)
Summary_paragraph <- "In this table, we showed how many people visited in 2019 and compared that between the top 15 destinations. It shows us that Mexico is a very big outlier and this should likely be taken into consideration when calculating risk factor. With this information you will be able to evaluate the relevance of deaths when it comes to the most traveled to cities. This data is also likely skewed due to COVID as Mexico has laxer laws when it comes to vaccination status and masks, it is likely that is why they tower above other countries. "

Year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
Mexico <- c(20.02, 19.92, 20.37, 20.55, 25.88, 28.73, 31.19, 34.95, 36.37, 39.94, 21.73)
Canada <- c(11.75, 11.56, 11.85, 12.01, 11.51, 12.47, 13.89, 14.33, 14.34, 14.99, 1.93)
UK <- c(2.88, 2.76, 2.75, 2.98, 3.54, 3.95, 4.63, 4.57, 4.50, 1.92)
Japan <- c(5.85, 8.36, 10.36, 13.41, 19.74, 24.04, 28.69, 31.19, 31.88, 4.12) 
Germany <- c(1.65, 1.82, 1.96, 2.12, 2.37, 2.42, 2.56, 2.63, 2.70, 2.78, 0.600)

my_data <- as.data.frame(cbind(Year,Mexico,Canada,UK,Japan,Germany))

ui <- fluidPage(
  titlePanel("What has Visitation Looked like Recently"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Number of Vistors between 2010-2020 (Millions)"),
      
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
      p(Summary_paragraph)
    )
  )
)
server <- function(input, output) {
  
  output$DataTable <- renderTable({
    dt <- my_data[my_data$Year >= input$range[1] & my_data$Year <= input$range[2],]
    dt[,c("Year",input$var)]
  },include.rownames=FALSE)
  
}
shinyApp(ui, server)

