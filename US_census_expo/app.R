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

# Read in the rds files that were written for this app in the script. In order,
# they are for the bar plot, then the map then the table.

youth_2017 <- read_rds("young_people_location")
percent_educ <- read_rds("percent_educated")
agg_total_pop_2 <- read_rds("aggregate_pops")
frame_with_degrees <- read_rds("aggregate_plus_degrees")
states <- states()
all_us <- geo_join(states, frame_with_degrees, "NAME", "geography")

# Define UI for application, this inccludes the tabs to be selected and what to
# call them.

ui <- navbarPage("A Smoother Look at the US Census",
                 theme = shinytheme("united"),
      
      # This is the main consumer-facing aspect of the app. Make it nice.

            tabPanel("Population Flux",
                     
            sidebarPanel(
          
          # # In each panel, name it something indicative of the info, but make it
          # # sound interesting. In order: moving map, bar chart, gt table, and
          # # the backround stuff.
          
          
              selectInput("vars",
                          label = "Choose an Option Below:",
                          choices = list("Census Change '16-'17" = "change",
                                         "Census Totals 2016" = "totalpop.16",
                                         "Census Totals 2017" = "totalpop.17",
                                         "Degrees Held in 2017" = "total_degrees.17",
                                         "Degrees Held in 2016" = "total_degrees.16",
                                         "Change in Degree-Holder Location" = "deg_change"))),
                  
                  mainPanel(htmlOutput("map_text"), 
                            leafletOutput("big_map"))),
      
          tabPanel("Static Contrast",
                   
                   # tabset inside of a tab allows tabs inside of a tab...
                   # tab-ception
                   
                   tabsetPanel(
                     tabPanel("Where are the Young People?",
                              plotOutput("youth")),
                     tabPanel("Where are the Educated People?",
                              plotOutput("degree_states")))),
          
          tabPanel("Tabled Data",
                   mainPanel(DTOutput("table"))),
      
          tabPanel("About this Project",
                   mainPanel(htmlOutput("about")))
        )
    
      

# Define server logic, watch the curl braces and  the quotes in the html.
# Otherwise, it is the same code as in the script.

server <- function(input, output) {
  
  # This is the about tab code: I used html for the formatting abilities.
  
  output$about <- renderText({
    
    # Can't comment between lines here bc of the quotes, but html is very picky.
    # Including a link to my github ad linkedin, and also my ackowledgement
    # section. 
    
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
  
  # This is the static bar chart about percentage of people 18-24
   
  output$youth <- renderPlot({
     
     # Shooting for a horizontal bar chart of states with the most 18-24 people.
     
    ggplot(youth_2017) +
   
       # Don't forget the stat argument... this can take a lot of time to
       # figure out otherwise.
       
       geom_bar(stat = "identity", aes(x = geography, y = rat,
                                       fill = geography), show.legend = FALSE) +
      
      # Per Healey, invversion is cool. I had to use paste because the
      # continuous scale was picky about my lack of "continuous data"
      
      scale_y_continuous(labels=function(y) paste0(y,"%")) +
      coord_flip() +
      
      # Themes are nice-looking. 
      
      theme_economist_white() +
      
      # Always label and cite, otherwise you have worked for nothing.
      
      labs(
        title = "Where are the young people?",
        subtitle = "States with the highest contingent of people age 18 to 24", 
        caption = "Source: the U.S. Census Bureau",
        
        # x-axis is labeled intrinsically
        
        x = "", y = "Percent of the States' Population")
   })
  
  # This is the static bar plot about percentage of degree holders by state
  
  output$degree_states <- renderPlot({
    
    # Pass the data to ggplot to avoid pipe actions in the app
    
    ggplot(percent_educ) +
      
      # Don't forget about 'stat =' 
      # see the previous bar plot for the geom_bar() formatting comments.
      
      geom_bar(stat = "identity", aes(x = geography, y = percent_deg_17,
                                      fill = geography), show.legend = FALSE)+
      scale_y_continuous(labels=function(y) paste0(y,"%")) +
      coord_flip() +
      theme_economist_white() +
      labs(
        x = "", y = "Percent of Population with a Bachelor's Degree or Higher",
        title = "States with the Highest Contingent of People with Degrees",
        caption = "Source: US Census Bureau"
      )
    
  })
  
  output$map_text <- renderText({
    
    # Because the map is reactive, each selection is relevant to different data
    # and information. Thus, I had to create reactive labels. The HTML code
    # below is run through an if-else loop system so that it coordinates with
    # the display. I don't have to worry about an initial selection because the
    # 'selectInput' tool does this implicitly.
    
    if(input$vars == "totalpop.16"){
      "<h3><b>All Adults in the 2016 Census<b/></h3>
      Explore the map! Hover cursor over the map to view the data.<br/>
      Each numeric representation relates to one response to the census.<br/><br/>"
    }
    else if(input$vars == "totalpop.17"){
      "<h3><b>All Adults in the 2017 Census<b/></h3>
      Explore the map! Hover cursor over the map to view the data.<br/>
      Each numeric representation relates to one response to the census.<br/><br/>"
    }
    else if(input$vars == "change"){
      "<h3><b>The Net Change in Adult Population by State<b/></h3>
      Explore the map! Hover cursor over the map to view the data.<br/>
      Is this what you would expect?<br/>
      Each numeric representation relates to one response to the census.<br/><br/>"
    }
    else if(input$vars == "total_degrees.17"){
      "<h3><b>Adults with a Bachelor's Degree in 2017<b/></h3>
      Explore the map! Hover cursor over the map to view the data.<br/>
      Each numeric representation relates to one response to the census.<br/><br/>"
    }
    else if(input$vars == "total_degrees.16"){
      "<h3><b>Adults with a Bachelor's Degree in 2016<b/></h3>
      Explore the map! Hover cursor over the map to view the data.<br/>
      Each numeric representation relates to one response to the census.<br/><br/>"
    }
    else{
      "<h3><b>The Net Change in Degree-Holding Adult Population by State<b/></h3>
      This data shows the flux of individuals with degrees throughout the U.S.<br/>
      Explore the map! Hover cursor over the map to view the data.<br/>
      Is this what you would expect?<br/>
      Each numeric representation relates to one response to the census.<br/><br/>"
    }
  })
   
  output$big_map <- renderLeaflet({ 
    
    if(input$vars == "change") {

     # I edited this a bit to have a more even color scheme distribution, it
     # pertains to the legend's bracketing.
     
     bins <- c(-Inf, -15000, 0, 45000, 125000, 200000, Inf)
     
     # Color palette, and domain by variable, bins arg goes here.

     pal <- colorBin("RdYlGn", domain = all_us$change, bins = bins)
     
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
                 position = "bottomright")}
    
    else if (input$vars == "totalpop.17"){
      
      bins <- c( 0, 500000, 1000000, 5000000, 10000000, 20000000, Inf)
      
      # Color palette, and domain by variable, bins arg goes here.
      
      pal <- colorBin("Oranges", domain = all_us$totalpop.17, bins = bins)
      
      # Make the labels bold, assign the variable shown by  state.
      # Thanks to Rstudio for the help with this!
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g million people",
        all_us$geography, round(all_us$totalpop.17 / 1000000, 1)
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
          fillColor = ~pal(totalpop.17),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            
            # Color of the hover-select outline
            
            color = "#600",
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
        
        addLegend(pal = pal, values = ~all_us$totalpop.17, opacity = 0.7, title = NULL,
                  position = "bottomright")
      
    }
    
    else if (input$vars == "totalpop.16") {
      
      bins <- c( 0, 500000, 1000000, 3000000, 5000000, 10000000, 20000000, Inf)
      
      # Color palette, and domain by variable, bins arg goes here.
      
      pal <- colorBin("Greens", domain = all_us$totalpop.16, bins = bins)
      
      # Make the labels bold, assign the variable shown by  state.
      # Thanks to Rstudio for the help with this!
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g million people",
        all_us$geography, round(all_us$totalpop.16 / 1000000, 1)
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
          fillColor = ~pal(totalpop.16),
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
        
        addLegend(pal = pal, values = ~all_us$totalpop.16, opacity = 0.7, title = NULL,
                  position = "bottomright")
      
    }
    
    else if (input$vars == "total_degrees.16") {
      
      bins <- c( 0, 50000, 100000, 500000, 1000000, 1500000, 2500000, 3500000, Inf)
      
      # Color palette, and domain by variable, bins arg goes here.
      
      pal <- colorBin("GnBu", domain = all_us$total_degrees.16, bins = bins)
      
      # Make the labels bold, assign the variable shown by  state.
      # Thanks to Rstudio for the help with this!
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g million people",
        all_us$geography, round(all_us$total_degrees.16 / 1000000, 2)
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
          fillColor = ~pal(total_degrees.16),
          weight = 2,
          opacity = 1,
          color = "grey",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            
            # Color of the hover-select outline
            
            color = "#550",
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
        
        addLegend(pal = pal, values = ~all_us$total_degrees.16, opacity = 0.7, title = NULL,
                  position = "bottomright")
    }
    
    else if(input$vars == "total_degrees.17") {
      
      bins <- c( 0, 50000, 100000, 500000, 1000000, 1500000, 2500000, Inf)
      
      # Color palette, and domain by variable, bins arg goes here.
      
      pal <- colorBin("Reds", domain = all_us$total_degrees.17, bins = bins)
      
      # Make the labels bold, assign the variable shown by  state.
      # Thanks to Rstudio for the help with this!
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g million people",
        all_us$geography, round(all_us$total_degrees.17/1000000, 2)
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
          fillColor = ~pal(total_degrees.17),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            
            # Color of the hover-select outline
            
            color = "#500",
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
        
        addLegend(pal = pal, values = ~all_us$total_degrees.17, opacity = 0.7, title = NULL,
                  position = "bottomright")
    }
    
    else {
      
      bins <- c(-Inf, 0, 5000, 10000, 50000, 100000, 150000, 200000, Inf)
      
      # Color palette, and domain by variable, bins arg goes here.
      
      pal <- colorBin("RdYlGn", domain = all_us$deg_change, bins = bins)
      
      # Make the labels bold, assign the variable shown by  state.
      # Thanks to Rstudio for the help with this!
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g people",
        all_us$geography, all_us$deg_change
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
          fillColor = ~pal(deg_change),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            
            # Color of the hover-select outline
            
            color = "#500",
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
        
        addLegend(pal = pal, values = ~all_us$deg_change, opacity = 0.7, title = NULL,
                  position = "bottomright")
    }
   })
  
  output$table <- renderDT({
     
     # table of agg total pop attempt
     
     datatable(frame_with_degrees,
               colnames = c("State / Territory", "Population 2017", "Population 2016", "Change '16-'17",
                            "Degree Holders 2017", "Degree Holders 2016", "Degree Change '16-'17")) %>% 
       formatCurrency(c("totalpop.17", "totalpop.16", "change", "total_degrees.17", "total_degrees.16", "deg_change"), 
                      currency = "", interval = 3, mark = ",", digits = 0)
   })
}

# Run the application 

shinyApp(ui = ui, server = server)

