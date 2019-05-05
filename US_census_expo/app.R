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

# look into DT Table

# Read in the rds files that were written for this app in the script. In order,
# they are for the bar plot, then the map then the table.

youth_2017 <- read_rds("young_people_locations")
agg_total_pop_2 <- read_rds("aggregate_pops")
states <- states()
all_us <- geo_join(states, agg_total_pop_2, "NAME", "geography")

# Define UI for application, this inccludes the tabs to be selected and what to
# call them.

ui <- fluidPage(
   
   # Application title
   titlePanel("A Smoother Look at the US Census"),
      
      # This is the main consumer-facing aspect of the app. Make it nice.
   
      mainPanel(
        
        # Allow for multiple tabs
        
        tabsetPanel(
          
          # In each panel, name it something indicative of the info, but make it
          # sound interesting. In order: moving map, bar chart, gt table, and
          # the backround stuff.
          
          tabPanel("Population Flux",
                   varSelectInput(vars, "Choose an Option Below:",
                                  agg_total_pop_2, selected = "change",
                                  multiple = FALSE),
                   leafletOutput("big_map")),
          tabPanel("Young People",
                   plotOutput("youth")),
          
          # tabPanel("Census Totals '17",
          #          leafletOptions("pop_17")),
          # This is commented pending further construction
          
          tabPanel("Tabled Data",
                   gt_output("table")),
          tabPanel("About this Project",
                   htmlOutput("about"))
        )
      )
   )

# Define server logic, watch the curl braces and  the quotes in the html.
# Otherwise, it is the same code as in the script.

server <- function(input, output) {
  
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
       theme_fivethirtyeight() + 
       coord_flip() +
       
       # Always label and cite, otherwise you have worked for nothing.
       
       labs(
         title = "Where are the young people?",
         subtitle = "States with the highest contingent of people age 18 to 24", 
         caption = "Source: the U.S. Census")
   })
   
   output$big_map <- renderLeaflet({

     # I edited this a bit to have a more even color scheme distribution, it
     # pertains to the legend's bracketing.
     
     bins <- c(-45000, -25000, -15000, 0, 45000, 125000, 200000, Inf)
     
     # Color palette, and domain by variable, bins arg goes here.

     pal <- colorBin("RdYlGn", domain = all_us$change, bins = bins)
     
     # Make the labels bold, assign the variable shown by  state.
     # Thanks to Rstudio for the help with this!

     labels <- sprintf(
       "<strong>%s</strong><br/>%g people",
       all_us$geography, all_us$vars
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
         fillColor = ~pal(vars),
         weight = 2,
         opacity = 1,
         color = "white",
         dashArray = "3",
         fillOpacity = 0.7,
         highlight = highlightOptions(
           weight = 5,
           
           # Color of the hover-select outline
           
           color = "#750",
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
       
       addLegend(pal = pal, values = ~all_us$vars, opacity = 0.7, title = NULL,
                 position = "bottomright")
   })
   
   # This is a script in progress, it will run in the environment but it won't
   # project in shiny. It doesn't throw an error, but it doesn't map either.
   # Shall consult with Kane / Albert later. Skipping the re-explanation for
   # brevity, I trust the reader's ability to infer from above. 
   
   # output$pop_17 <- renderLeaflet({
   # 
   #   pal_1 <- colorBin("YlOrRd", domain = all_us$totalpop.17)
   # 
   #   labels <- sprintf(
   #     "<strong>%s</strong><br/>%g people",
   #     all_us$geography, all_us$totalpop.17
   #   ) %>% lapply(htmltools::HTML)
   # 
   #   leaflet(all_us) %>%
   #     setView(-96, 37.8, 3) %>%
   #     addProviderTiles("MapBox", options = providerTileOptions(
   #       id = "mapbox.light",
   #       accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
   #     addPolygons(
   #       fillColor = ~pal(totalpop.17),
   #       weight = 2,
   #       opacity = 1,
   #       color = "white",
   #       dashArray = "3",
   #       fillOpacity = 0.7,
   #       highlight = highlightOptions(
   #         weight = 5,
   #         color = "#750",
   #         dashArray = "",
   #         fillOpacity = 0.7,
   #         bringToFront = TRUE),
   #       label = labels,
   #       labelOptions = labelOptions(
   #         style = list("font-weight" = "normal", padding = "3px 8px"),
   #         textsize = "15px",
   #         direction = "auto")) %>%
   #     addLegend(pal = pal_1, values = ~all_us$totalpop.17, opacity = 0.7, title = NULL,
   #               position = "bottomright")
   # })

   output$table <- render_gt({
     
     # table of agg total pop attempt
     # Throwback to gt()
     
     # Taking the same data from beofre, I want the reader to see their state
     # aggregate and change on 'paper'. 

     agg_total_pop_2 %>% gt() %>%
       
       # Re-name the cols to something nicer
       
       cols_label(geography = "State",
                  totalpop.17 = "2017",
                  totalpop.16 = "2016",
                  change = "Change") %>%
       
       # Data is straight-forward, but labeling and citing is still a must.
       
       tab_header("Change in Total Population Counts") %>%
       tab_spanner(label = "States and Territories in the first year of Trump's Presidency",
                   columns = vars(geography, totalpop.17, totalpop.16, change)) %>%
       tab_source_note("Source: the U.S. Census")
   })
}

# Run the application 

shinyApp(ui = ui, server = server)

