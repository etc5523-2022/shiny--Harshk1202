library(shiny)
library(tidyverse)



mydata1<- read_csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv"))

mydata2 <- read_csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/milk_products_facts.csv")) %>%
  pivot_longer(cols = fluid_milk:dry_whey,
               names_to = "Product_Type",
               values_to = "Consumption")


ui <- fluidPage(
  titlePanel("Dairy Production and Consumption Data"),
  h3("1. What is the average production of milk per capita in each state of United States from 1970 to 2017"),
  fluidRow(
        selectInput("milk_production",
                    "Select State:",
                    choices = unique(mydata1$state)),
        mainPanel(
          plotOutput("plot1")
        )
  ),
  h3("2. What is consumption of different dairy products per capita from 1975 to 2017"),
  fluidRow(
    selectInput("Consumption",
                "select Product:",
                choices = unique(mydata2$Product_Type)),
    mainPanel(
      plotOutput("plot2")
    )
  )

)

server <- function(input, output, session) {

}

shinyApp(ui, server)
