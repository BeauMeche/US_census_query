library(shiny)
library(tidyverse)
library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(leaflet)

youth_2017 <- read_rds("young_people_locations")

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("A Smoother Look at the US Census"),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Young People",
                   plotOutput("youth")))
      )
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$youth <- renderPlot({
     ggplot(youth_2017) +
       geom_bar(stat = "identity", aes(x = geography, y = total_estimate_population_18_to_24_years, 
                                       fill = geography), show.legend = FALSE) +
       coord_flip() + theme_fivethirtyeight() +
       labs(
         title = "Where are the young people?",
         subtitle = "Shown: the 10 states with the most people aged 18 to 24", 
         caption = "Source: the U.S. Census")
   })
   
   output$big__map <- renderLeaflet()
}

# Run the application 
shinyApp(ui = ui, server = server)

