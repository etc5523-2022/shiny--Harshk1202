library(shiny)
library(tidyverse)
library(plotly)


mydata1<- read_csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv"))

mydata2 <- read_csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/milk_products_facts.csv")) %>%
  pivot_longer(cols = fluid_milk:dry_whey,
               names_to = "Product_Type",
               values_to = "Consumption")


ui <- fluidPage(
  titlePanel("Dairy Production and Consumption Statistics"),
  h3("1. What is the average production of milk per capita in each state of United States from 1970 to 2017"),
  fluidRow(
        radioButtons("productionRegion",
                    "Select Region",
                    choices = unique(mydata1$region)),
        selectInput("production",
                    "Select State:",
                    choices = unique(mydata1$state)),
        mainPanel(
          plotlyOutput("plot1")
        )
  ),
  h3("2. What is consumption of different dairy products per capita from 1975 to 2017"),
  fluidRow(
    selectInput("consumption",
                "Select Product:",
                choices = unique(mydata2$Product_Type)),
    mainPanel(
      plotlyOutput("plot2")
    )
  ),
  includeCSS("styles.css")

)

server <- function(input, output, session) {
  observeEvent(input$productionRegion,
               updateSelectInput(session,
                                 "production",
                                 choices = mydata1$state[mydata1$region == input$productionRegion]))
  output$plot1 <- renderPlotly({
    production_plot <- mydata1 %>%
      filter(state == input$production)

    p <- ggplot(production_plot, aes(year, milk_produced/1000000)) +
      geom_col(stat = 'identity') +
      theme_bw(base_size = 14) +
      labs(x = "Year", y = "Milk Production (million lbs)") +
      scale_x_continuous(
        breaks = seq(min(production_plot$year),
                     max(production_plot$year), by=1)) +
      theme(
        axis.text.x = element_text(angle = 90,
                                   hjust = 1))

    ggplotly(p)
  })

  output$plot2 <- renderPlotly({
    consumption_plot <- mydata2 %>%
      filter(Product_Type == input$consumption)

    p1 <- ggplot(consumption_plot, aes(year, Consumption)) +
      geom_col(stat = 'identity') +
      theme_bw(base_size = 14) +
      labs(x = "Year", y = "Consumption (lbs per person)") +
      scale_x_continuous(
        breaks = seq(min(consumption_plot$year),
                     max(consumption_plot$year), by=1)) +
      theme(
        axis.text.x = element_text(angle = 90,
                                   hjust = 1))

    ggplotly(p1)
  })

}

shinyApp(ui, server)
