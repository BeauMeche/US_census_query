library(shiny)
library(tigris)
library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(leaflet)
library(DT)
library(tidyverse)
library(shinythemes)

# look into DT Table

# Read in the rds files that were written for this app in the script. In order,
# they are for the bar plot, then the map then the table.

youth_2017 <- read_rds("young_people_location")
agg_total_pop_2 <- read_rds("aggregate_pops")
states <- states()
all_us <- geo_join(states, agg_total_pop_2, "NAME", "geography")

# Define UI for application, this inccludes the tabs to be selected and what to
# call them.

ui <- fluidPage( theme = shinytheme("united"),
   
   # Application title
   titlePanel("A Smoother Look at the US Census"),
      
      # This is the main consumer-facing aspect of the app. Make it nice.
   
      mainPanel(
        
        # Allow for multiple tabs
        
        tabsetPanel(
        #   sidebarLayout(
        #     sidebarPanel(
          
          # # In each panel, name it something indicative of the info, but make it
          # # sound interesting. In order: moving map, bar chart, gt table, and
          # # the backround stuff.
          
          
          tabPanel("Population Flux",
                  # selectInput("vars",
                  #             label = "Choose an Option Below:",
                  #             choices = list("Census Totals 2016" = "totalpop.16",
                  #                            "Census Totals 2017" = "totalpop.17",
                  #                            "Census Change '16-'17" = "change"))
                # )
              # ),                  
                  
                   leafletOutput("big_map")),
          tabPanel("Young People",
                   plotOutput("youth")),
          
          tabPanel("Tabled Data",
                   DTOutput("table")),
          tabPanel("About this Project",
                   htmlOutput("about"))
        )
      )
)  
      
      

# Define server logic, watch the curl braces and  the quotes in the html.
# Otherwise, it is the same code as in the script.

server <- function(input, output) {
  
   q <- reactive ({ input$vars })
  
  # This is the about tab code: I used html for the formatting abilities.
  # Include github link, contact link, acknowledgements, and data references.
  
  output$about <- renderText({
    
    # Can't comment between lines here bc of the quotes, but html is very picky.
    
    '<h3><b>About this Project</b></h3>
    <br/>
    <h5><b>Population Flux:</b></h5>
    This map shows, per state, the net population flow (flux) in the time span between 2016-2017.<br/><br/>
    <h5><b>Young People:</b></h5>
    Where are the young people? Are they in places with positive or negative net change? Any ideas regarding why?<br/><br/>
    <h5><b>Tabled Data:</b></h5>
    Here is the data table behind some of these graphics. Take a look!<br/><br/>
    <h5><b>Data</b></h5>
    The data for this project comes from the U.S. Census Bureau via FactFinder. The data sets employed 
    here can be found under educational attainment.<br/> Other data courtesy of the "tigris" package. 
    <br/><br/>
    <a href="https://factfinder.census.gov/faces/nav/jsf/pages/guided_search.xhtml">Check out FactFinder here.</a>
    <br/><br/>
    <a href="https://github.com/BeauMeche/US_census_query">See the Github repository for this project</a>
    <br/><br/>
    <h5><b>Acknoweldgements:</b></h5>
    Thanks to Preceptor, Albert, Claire, Dillon, Charles, Debora, Celine, and Stephanie for feedback, 
    advice, and input throughout the creation of this project.<br/>
    I would also like to thank Rstudio and Rstudio Community for providing so many exceptional resources.<br/><br/>
    Author: Beau Meche<br/>
    Contact: beau_meche@college.harvard.edu<br/>
    <a href="https://www.linkedin.com/in/beaumeche22/">Connect on LinkedIn</a>'
  })
   
  output$youth <- renderPlot({
     
     # Shooting for a horizontal bar chart of states with the most 18-24 people.
     
    ggplot(youth_2017) +
   
       # Don't forget the stat argument... this can take a lot of time to
       # figure out otherwise.
       
       geom_bar(stat = "identity", aes(x = geography, y = rat,
                                       fill = geography), show.legend = FALSE) +
      # Per Healey, invversion is cool.
      scale_y_continuous(labels=function(y) paste0(y,"%")) +
      coord_flip() +
      theme_economist_white() +
      
      # Always label and cite, otherwise you have worked for nothing.
      
      labs(
        title = "Where are the young people?",
        subtitle = "States with the highest contingent of people age 18 to 24", 
        caption = "Source: the U.S. Census",
        x = "", y = "Percent of the States' Population")
   })
   
  output$big_map <- renderLeaflet({ 

     # I edited this a bit to have a more even color scheme distribution, it
     # pertains to the legend's bracketing.
     
     bins <- c(-45000, -25000, -15000, 0, 45000, 125000, 200000, Inf)
     
     # Color palette, and domain by variable, bins arg goes here.

     pal <- colorBin("RdYlGn", domain = all_us$q, bins = bins)
     
     # Make the labels bold, assign the variable shown by  state.
     # Thanks to Rstudio for the help with this!

     labels <- sprintf(
       "<strong>%s</strong><br/>%g people",
       all_us$geography, all_us$change
     ) %>% lapply(htmltools::HTML)
     
     # Create the actual leaflet plot:

     leaflet(all_us) %>%
       
       # Set the opening view range and zoom and add in the tiles. 
       # This resource requires an "access token" evidently. 
       
       setView(-96, 37.8, 3) %>%
       addProviderTiles("MapBox", options = providerTileOptions(
         id = "mapbox.light",
         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
       
       # These are all asthetic edits, most are self-explanatory, so I won't bore
       # you with the details.
       
       addPolygons(
         fillColor = ~pal(change),
         weight = 2,
         opacity = 1,
         color = "white",
         dashArray = "3",
         fillOpacity = 0.7,
         highlight = highlightOptions(
           weight = 5,
           
           # Color of the hover-select outline
           
           color = "#350",
           dashArray = "",
           fillOpacity = 0.7,
           bringToFront = TRUE),
         
         # Intake label assignment from above.
         
         label = labels,
         
         # Hover label asthetics. 
         
         labelOptions = labelOptions(
           style = list("font-weight" = "normal", padding = "3px 8px"),
           textsize = "15px",
           direction = "auto")) %>% 
       
       # The legend is mandatory here, otherwise the colors are pointless and the
       # map is less interesting.

       addLegend(pal = pal, values = ~all_us$change, opacity = 0.7, title = NULL,
                 position = "bottomright")
   })
  

   output$table <- renderDT({
     
     # table of agg total pop attempt
     
     datatable(agg_total_pop_2,
               colnames = c("State / Territory", "Pop_2017", "Pop_2016", "Change"))
   })
}

# Run the application 

shinyApp(ui = ui, server = server)

