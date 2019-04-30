library(shiny)
library(tigris)
library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(leaflet)
library(tidyverse)

youth_2017 <- read_rds("young_people_locations")
agg_total_pop_2 <- read_rds("aggregate_pops")
states <- states()
all_us <- geo_join(states, agg_total_pop_2, "NAME", "geography")

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("A Smoother Look at the US Census"),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("About this Project",
                   htmlOutput("about")),
          tabPanel("Young People",
                   plotOutput("youth")),
          tabPanel("Census Totals '17",
                   leafletOptions("pop_17")),
          tabPanel("Population Flux",
                   leafletOutput("big_map")),
          tabPanel("Tabled Data",
                   gt_output("table"))
        )
      )
   )

# Define server logic
server <- function(input, output) {
  
  output$about <- renderText({
    '<h3><b>About this Project</b></h3>
    <br/>
    The data for this project comes from the U.S. Census Bureau via FactFinder. The data sets employed here can be found under educational attainment.
    <br/><br/>
    <a href="https://factfinder.census.gov/faces/nav/jsf/pages/guided_search.xhtml">Check out FactFinder here.</a>
    <br/><br/>
    <a href="_____________________">See the Github repository for this project</a>'
    
  })
   
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
   
   output$big_map <- renderLeaflet({

     bins <- c(-45000, -15000, 0, 15000, 30000,
               45000, 100000, 200000, Inf)

     pal <- colorBin("YlOrRd", domain = all_us$change, bins = bins)

     labels <- sprintf(
       "<strong>%s</strong><br/>%g people",
       all_us$geography, all_us$change
     ) %>% lapply(htmltools::HTML)

     leaflet(all_us) %>%
       setView(-96, 37.8, 3) %>%
       addProviderTiles("MapBox", options = providerTileOptions(
         id = "mapbox.light",
         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
       addPolygons(
         fillColor = ~pal(change),
         weight = 2,
         opacity = 1,
         color = "white",
         dashArray = "3",
         fillOpacity = 0.7,
         highlight = highlightOptions(
           weight = 5,
           color = "#750",
           dashArray = "",
           fillOpacity = 0.7,
           bringToFront = TRUE),
         label = labels,
         labelOptions = labelOptions(
           style = list("font-weight" = "normal", padding = "3px 8px"),
           textsize = "15px",
           direction = "auto")) %>%
       addLegend(pal = pal, values = ~all_us$change, opacity = 0.7, title = NULL,
                 position = "bottomright")
   })
   
   output$pop_17 <- renderLeaflet({
     
     pal_1 <- colorBin("YlOrRd", domain = all_us$totalpop.17)
     
     labels <- sprintf(
       "<strong>%s</strong><br/>%g people",
       all_us$geography, all_us$totalpop.17
     ) %>% lapply(htmltools::HTML)
     
     leaflet(all_us) %>%
       setView(-96, 37.8, 3) %>%
       addProviderTiles("MapBox", options = providerTileOptions(
         id = "mapbox.light",
         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
       addPolygons(
         fillColor = ~pal(totalpop.17),
         weight = 2,
         opacity = 1,
         color = "white",
         dashArray = "3",
         fillOpacity = 0.7,
         highlight = highlightOptions(
           weight = 5,
           color = "#750",
           dashArray = "",
           fillOpacity = 0.7,
           bringToFront = TRUE),
         label = labels,
         labelOptions = labelOptions(
           style = list("font-weight" = "normal", padding = "3px 8px"),
           textsize = "15px",
           direction = "auto")) %>%
       addLegend(pal = pal_1, values = ~all_us$totalpop.17, opacity = 0.7, title = NULL,
                 position = "bottomright")
   })
   
   output$table <- render_gt({
     
     agg_total_pop_2 %>% gt() %>% 
       cols_label(geography = "State",
                  totalpop.17 = "2017",
                  totalpop.16 = "2016",
                  change = "Change") %>% 
       tab_header("Change in Total Population Counts") %>% 
       tab_spanner(label = "In the first year of Trump's Presidency",
                   columns = vars(geography, totalpop.17, totalpop.16, change)) %>% 
       tab_source_note("Source: the U.S. Census")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

