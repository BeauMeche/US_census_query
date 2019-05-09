library(shiny)
library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(leaflet)
library(DT)
library(tigris)
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
      
          tabPanel("Static Comparison",
                   
                   # tabset inside of a tab allows tabs inside of a tab...
                   # tab-ception
                   
                   tabsetPanel(
                     
                     # Fill out tabs as usual
                     
                     tabPanel("Where are the Young People?",
                              plotOutput("youth")),
                     tabPanel("Where are the Educated People?",
                              plotOutput("degree_states")))),
      
          # this is the DT searchable table, simply a DT output call inside the
          # main panel
          
          tabPanel("Tabled Data",
                   mainPanel(DTOutput("table"))),
      
          # Another tab of tabs, this is the HTML descriptions and citations and
          # stuff for reference. 
      
          tabPanel("About this Project",
                   
                   tabsetPanel(
                   tabPanel("More Info",
                            htmlOutput("about")),
                   
                   tabPanel("Acknowledgements",
                            htmlOutput("acknowledgements"))))
        )
    
      

# Define server logic, watch the curl braces and  the quotes in the html.
# Otherwise, it is the same code as in the script.

server <- function(input, output) {
  
  # This is the about tab code: I used html for the formatting abilities.
  
  output$about <- renderText({
    
    # Can't comment between lines here bc of the quotes, but html is very picky.
    # Including a link to my github ad linkedin, and also my ackowledgement
    # section. I have a section describing my reasoning for the use of each
    # graphic and each tab with further explanations and clarification of the
    # relevant respective details.
    
    '<h2><b>Context, Explanation, and Citation</b></h2>
      <br/>
      <h4><b>Population Flux:</b></h4>
        <ul>
          <li>These maps show, per state or territory, the net population flow (flux) of that region in the time span between 2016-2017.
              My reason for this was based in my interest in being able to visualize the gross population flow versus that of educated
              individuals to see if there were noticeable trends. The term "brain drain" is popular in this context. The color schemes 
              are aimmed at highlighting various aspects depending of the data selected, but I have included keys in case of confusion.</li>
          <li>All numeric values are a sum of single responnses in which one resopnse relates to one adult respondent.</li>
        </ul>
    
    <h4><b>Static Comparison - Where are the Young People?:</b></h4>
      <ul>
        <li>This is intended to show, as evidenced by the axes, the areas with the highest concentrations of individuals 
            betwen 18 and 24. It is my hypothesis that younger individuals are generally more frequently and easily mobile, 
            so this chart is intended to facilitate a comparison between where younger adults are congragated and states with
            a high flux value.</li>
        <li>All numeric values are a sum of single responnses in which one resopnse relates to one adult respondent.</li>
      </ul>

    <h4><b>Static Comparison - Where are the Educated People?:</b></h4>
      <ul>
        <li>This is intended to show, as evidenced by the axes, the regions with the highest concentrations of individuals 
            in the U.S. with degrees. It is my hypothesis that individuals with a college degree are more mobile than those without one, 
            so this chart is intended to further facilitate a comparison between where younger adults are congragated, where
            those with a college degree are ccongreagetd, and states with a high popuation and/or flux value.</li>
        <li>All numeric values are a sum of single responnses in which one resopnse relates to one adult respondent.</li>
     </ul>

    <h4><b>Tabled Data:</b></h4>
      <ul>
        <li>This is an interactive version of the data that I used in the various maps and charts. If you are interested in a certain state,
            want to see the numbers in closer proximity, or generally enjoy spreadsheets, this is your spot.</li>
        <li>For purposes of clarification, the final column is the net change in degree holder population.<br/>This was too cumbersome a title
            for a single table column.</li>
        <li>All numeric values are a sum of single responnses in which one resopnse relates to one adult respondent.</li>
      </ul>


    <h4><b>Data Sources:</b></h4>
      <ul>
        <li>The data for this project comes from the U.S. Census Bureau via FactFinder. The data sets employed 
            here can be found under educational attainment.<br/> Geographic data courtesy of the "tigris" and "leaflet" packages.</li>
        <li><a href="https://factfinder.census.gov/faces/nav/jsf/pages/guided_search.xhtml">Check out FactFinder here.</a></li>
        <li><a href="https://github.com/BeauMeche/US_census_query">Check out the Github repository for this project here.</a></li>
      </ul>'
  })
  
  # Name is self-explanatory, but this is where I give credit to whom it is due.
  
  output$acknowledgements <- renderText({
    
    # This is the thanks portion. The project is mine, but I must give credit
    # where it is due and these people aided me either in design or overcomming
    # a technical difficulty. 
    
    '<h4><b>Acknoweldgements:</b></h4>
        <ul>
          <li>Thanks to Dr. David Kane, Albert Rivero, Claire Fridkin, Dillon Smith, Charles Flood, Debora Gonzalez, Celine Vendler, 
              <br/>Christopher Onesti, and Stephanie Yao for feedback, advice, and input throughout the creation of this project.</li>
          <li>I would also like to thank Rstudio and Rstudio Community for providing so many exceptional resources.</li>
        </ul><br/><br/><br/><br/><br/>
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
        x = "", y = "Percent of State Population",
        title = "States with the Highest Contingent of People with Degrees",
        subtitle = "Measuring individuals with a Bachelor's degree or more.",
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
      
      # This is the title/description for the map of all adults in 2016.
      
      "<h3><b>All Adults in the 2016 Census<b/></h3>
      <ul>
        <li>Explore the map! Hover cursor over the map to view the data.</li>
        <li>Note: each numeric representation relates to a sum of responses to the census.</li>
      </ul><br/><br/>"
    }
    else if(input$vars == "totalpop.17"){
      
      # This is the title/description for the map of all adults in 2017.
      
      "<h3><b>All Adults in the 2017 Census<b/></h3>
      <ul>
        <li>Explore the map! Hover cursor over the map to view the data.</li>
        <li>Note: each numeric representation relates to a sum of responses to the census.</li>
      </ul><br/><br/>"
    }
    else if(input$vars == "change"){
      
      # This is the title/description for the map of total population flux.
      
      "<h3><b>The Net Change in Adult Population by State<b/></h3>
      <ul>
        <li>This data shows the flux of all adult individuals into and between states in the U.S. from the year 2016 to 2017.</li>
        <li>Explore the map! Hover cursor over the map to view the data.<br/>
            Is this what you would expect? Can you explain any of th trends?</li>
        <li>Note: each numeric representation relates to a sum of responses to the census.</li>
      </ul><br/><br/>"
    }
    else if(input$vars == "total_degrees.17"){
      
      # This is the title/description for the map of all adults with degrees in
      # 2017.
      
      "<h3><b>Adults with a Bachelor's Degree in 2017<b/></h3>
      <ul>
        <li>Explore the map! Hover cursor over the map to view the data.</li>
        <li>Npte: each numeric representation relates to a sum of responses to the census.</li>
      </ul><br/><br/>"
    }
    else if(input$vars == "total_degrees.16"){
      
      # This is the title/description for the map of all adults with degrees in
      # 2016.      
      
      "<h3><b>Adults with a Bachelor's Degree in 2016<b/></h3>
      <ul>
        <li>Explore the map! Hover cursor over the map to view the data.</li>
        <li>Note: each numeric representation relates to a sum of responses to the census.</li>
      </ul><br/><br/>"
    }
    else{
      
      # This is the title/description for the map of total flux in population of
      # degree-holders.
      
      "<h3><b>The Net Change in Degree-Holding Adult Population by State<b/></h3>
      <ul>
        <li>This data shows the flux of individuals with degrees into and between states in the U.S. from the year 2016 to 2017.</li>
        <li>Explore the map! Hover cursor over the map to view the data.<br/>
      Is this what you would expect? How does it compare to the general population?</li>
      <li>Note: each numeric representation relates to a sum of responses to the census.</li>
      </ul><br/><br/>"
    }
  })
  
  # This(these) is the plot code for the main interactive maps. I only have 6
  # plots so I fed the different options into a large if-else loop with a
  # selection filter. I commented extensively on the first one, but for brevity
  # left the subsequent plots alone. The only differences are the selected
  # variabe, color asthetic choices, and the bin breaks. If you are looking into
  # this, good luck! PS: thanks to Rstudio for the leads on this method.
   
  output$big_map <- renderLeaflet({ 
    
    # Plot of total population change (without respect to education).
    
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
    
    # Plot of total adult population 2017.
    
    else if (input$vars == "totalpop.17"){
      
      bins <- c( 0, 500000, 1000000, 5000000, 10000000, 20000000, Inf)
      
      pal <- colorBin("Oranges", domain = all_us$totalpop.17, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g million people",
        all_us$geography, round(all_us$totalpop.17 / 1000000, 1)
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
            
            color = "#600",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          
          label = labels,
          
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>% 
        
        addLegend(pal = pal, values = ~all_us$totalpop.17, opacity = 0.7, title = NULL,
                  position = "bottomright")
      
    }
    
    # Plot of total adult population (w/o respect to education) 2016.
    
    else if (input$vars == "totalpop.16") {
      
      bins <- c( 0, 500000, 1000000, 3000000, 5000000, 10000000, 20000000, Inf)
      
      pal <- colorBin("Greens", domain = all_us$totalpop.16, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g million people",
        all_us$geography, round(all_us$totalpop.16 / 1000000, 1)
      ) %>% lapply(htmltools::HTML)
      
      leaflet(all_us) %>%
        
        setView(-96, 37.8, 3) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
        
        addPolygons(
          fillColor = ~pal(totalpop.16),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            
            color = "#350",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          
          label = labels,
          
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>% 
        
        addLegend(pal = pal, values = ~all_us$totalpop.16, opacity = 0.7, title = NULL,
                  position = "bottomright")
      
    }
    
    # Plot of total population with degrees 2016.
    
    else if (input$vars == "total_degrees.16") {
      
      bins <- c( 0, 50000, 100000, 500000, 1000000, 1500000, 2500000, 3500000, Inf)
      
      pal <- colorBin("GnBu", domain = all_us$total_degrees.16, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g million people",
        all_us$geography, round(all_us$total_degrees.16 / 1000000, 2)
      ) %>% lapply(htmltools::HTML)
      
      leaflet(all_us) %>%
        
        setView(-96, 37.8, 3) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
        
        addPolygons(
          fillColor = ~pal(total_degrees.16),
          weight = 2,
          opacity = 1,
          color = "grey",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#550",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          
          label = labels,
          
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>% 
        
        addLegend(pal = pal, values = ~all_us$total_degrees.16, opacity = 0.7, title = NULL,
                  position = "bottomright")
    }
    
    # Plot of total population with degrees 2017.
    
    else if(input$vars == "total_degrees.17") {
      
      bins <- c( 0, 50000, 100000, 500000, 1000000, 1500000, 2500000, Inf)
  
      pal <- colorBin("Reds", domain = all_us$total_degrees.17, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g million people",
        all_us$geography, round(all_us$total_degrees.17/1000000, 2)
      ) %>% lapply(htmltools::HTML)
      
      leaflet(all_us) %>%
        
        setView(-96, 37.8, 3) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
        
        addPolygons(
          fillColor = ~pal(total_degrees.17),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            
            color = "#500",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          
          label = labels,
          
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>% 
        
        addLegend(pal = pal, values = ~all_us$total_degrees.17, opacity = 0.7, title = NULL,
                  position = "bottomright")
    }
    
    # Plot of total population change with respect to education 16 -> 17.
    
    else {
      
      bins <- c(-Inf, 0, 5000, 10000, 50000, 100000, 150000, 200000, Inf)
      
      pal <- colorBin("RdYlGn", domain = all_us$deg_change, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g people",
        all_us$geography, all_us$deg_change
      ) %>% lapply(htmltools::HTML)
      
      leaflet(all_us) %>%
        
        setView(-96, 37.8, 3) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  
        addPolygons(
          fillColor = ~pal(deg_change),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            
            color = "#500",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          
          label = labels,
          
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>% 
        
        addLegend(pal = pal, values = ~all_us$deg_change, opacity = 0.7, title = NULL,
                  position = "bottomright")
    }
   })
  
  output$table <- renderDT({
     
     # table of the 'frame_with_degrees' dataframe. I made it interactive with
     # the DT package, which was a realtively straight-forward process because I
     # had already cleaned up thei information.
     
     datatable(frame_with_degrees,
               
               # Rename the columns to be more concise and formatted. 
               
               colnames = c("State / Territory", "Population 2017", "Population 2016", "Change '16-'17",
                            "Degree Holders 2017", "Degree Holders 2016", "Degree Change '16-'17")) %>% 
      
      # Using currency formatting here (without the decimal places) as a
      # conenient way to add commas to the numbers. I think this makes the table
      # easier to read on the fly. No need to specify currency because I don't
      # want a denomination marker.
      
       formatCurrency(c("totalpop.17", "totalpop.16", "change", "total_degrees.17", "total_degrees.16", "deg_change"), 
                      currency = "", interval = 3, mark = ",", digits = 0)
   })
}

# Run the application with my 'unique and creative' ui and server names.

shinyApp(ui = ui, server = server)

