# Choose your tools wisely, tidyverse at the end to avoid overwriting I did a
# lot of testing and experimenting here, and not all of the code here has been
# utilized in the app. When/ if I expand the app, I may add these in at that
# point. That said, this is where I create the RDS files to use in the app for
# brevity and efficiency's sake.

library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(tigris)
library(tidyverse)
library(scales)

# I have previously acquired the sourcce files and they are in my personal
# GitHub, so linking them here is safe. Also census data doesn't change.

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_2017_edu_attainment.csv",
              destfile = "2017_data.csv",
              mode = "wb")

# Don't forget to input destination. 

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_edu_attainment_2016.csv",
              destfile = "2016_data.csv", 
              mode = "wb")

# Read in the data: census data is terribly formatted, there are up to 7 or 8
# different things in some columns, so attampting to clean has taken well over
# 30 hours at this point.

# Janitor package is a great first step to avoid syntax issues.

data_2017 <- read_csv(file = "2017_data.csv", skip = 1) %>% 
  clean_names()

# This method of subsetting removed all of the columns (you'll notice that there
# are originally over 700 columns) that contained confidence interval
# information that are irrelevant to my purposes here. I repeated the same
# process for 2016 as well.

census_2017_nomargin <- data_2017[, ! str_detect(names(data_2017), pattern = "margin_of_error")] 

data_2016 <- read_csv(file = "2016_data.csv", skip = 1) %>% 
  clean_names()

census_2016_nomargin <- data_2016[, ! str_detect(names(data_2016), pattern = "margin_of_error")]

# table for the first plot in the app

# Again, the subsetting method treats the data much like a giant matrix, which
# is convenient. Here I want only totals, hence the first query, and I want to
# keep the state column for reference and graphing.

young_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "total_estimate_population")|
                                     str_detect(names(census_2017_nomargin), pattern = "geography")] %>% 
  
  # I am graphing young people, so selecting states and the 18-24 gap produces a
  # skinny data format.
  
  
  # I want to show the most populated areas, so arrnaging in descending order
  # and grabbing the top 10 is the process.
  
  mutate(totalpop = total_estimate_population_18_to_24_years + total_estimate_population_25_to_34_years +
           total_estimate_population_35_to_44_years + total_estimate_population_45_to_64_years +
           total_estimate_population_65_years_and_over) %>% 
  
  mutate(rat = (total_estimate_population_18_to_24_years / totalpop)*100) %>% 
  
  select(geography, rat) %>%
  
  arrange(desc(rat)) %>% 
  head(10) %>% 
  
  # Write this specific dataframe to an rds file so that it can be passed to the
  # shiny app later.
  
  write_rds("young_people_location")

# This creates a dataframe of percentages of people with degrees by state. 

percent_edu <- total_plus_degs %>% 
  mutate(percent_deg_17 = (total_degrees.17/totalpop.17)*100) %>% 
  select(geography, percent_deg_17) %>% 
  arrange(desc(percent_deg_17)) %>% head(10) %>% 
  write_rds("percent_educated")

# This is the test code for the bar plot of percentages of educated people.
# As the alighty Healey says, inversion is a good tool. 

ggplot(percent_edu) +
  geom_bar(stat = "identity", aes(x = geography, y = percent_deg_17,
                                  fill = geography), show.legend = FALSE)+
  scale_y_continuous(labels=function(y) paste0(y,"%")) +
  coord_flip() +
  theme_economist_white() 


# extract the populations in each age group with bachelors or higher degrees and
# add them up to see the "educated" people in 2017

bach_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "bachelors_degree_or_higher")&
                               str_detect(names(census_2017_nomargin), pattern = "total_estimate")|
                               str_detect(names(census_2017_nomargin), pattern = "geography")] %>% 
  mutate(total_degrees = total_estimate_population_65_years_and_over_bachelors_degree_or_higher +
                         total_estimate_population_45_to_64_years_bachelors_degree_or_higher +
                         total_estimate_population_35_to_44_years_bachelors_degree_or_higher +
                         total_estimate_population_25_to_34_years_bachelors_degree_or_higher +
                         total_estimate_population_18_to_24_years_bachelors_degree_or_higher) %>% 
  select(geography, total_degrees)

# repeat for 2016

bach_2016 <- census_2016_nomargin[, str_detect(names(census_2017_nomargin), pattern = "bachelors_degree_or_higher")&
                                    str_detect(names(census_2017_nomargin), pattern = "total_estimate")|
                                    str_detect(names(census_2017_nomargin), pattern = "geography")] %>% 
  mutate(total_degrees = total_estimate_population_65_years_and_over_bachelors_degree_or_higher +
           total_estimate_population_45_to_64_years_bachelors_degree_or_higher +
           total_estimate_population_35_to_44_years_bachelors_degree_or_higher +
           total_estimate_population_25_to_34_years_bachelors_degree_or_higher +
           total_estimate_population_18_to_24_years_bachelors_degree_or_higher) %>% 
  select(geography, total_degrees)


# this is the first plot in the app

      # Shooting for a horizontal bar chart of states with the most 18-24 people.

      ggplot(young_2017) +
        
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

# Total populations and  aggregate table creation: now I want to see the total
# population data, so in the same way that I pulled one age group, I will pull
# all of them for 2017. THis is slightly annoying, but I am not yet experienced
# enough to clean all the columns without corrupting some of the data.
      
  total_pop_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "total_estimate_population")|
                                           str_detect(names(census_2017_nomargin), pattern = "geography")] %>%
    
    # Here we have a giant addition problem. 
    
    mutate(totalpop = total_estimate_population_18_to_24_years + total_estimate_population_25_to_34_years +
             total_estimate_population_35_to_44_years + total_estimate_population_45_to_64_years +
             total_estimate_population_65_years_and_over) %>%
    
    # Select the new column and the state column to get rid of the long names
    # that we don't need anymore.
    
    select(geography, totalpop)
  
  # Repeat the last 3-4 steps for 2016
  
  total_pop_2016 <- census_2016_nomargin[, str_detect(names(census_2016_nomargin), pattern = "total_estimate_population")|
                                           str_detect(names(census_2016_nomargin), pattern = "geography")] %>%
    mutate(totalpop = total_estimate_population_18_to_24_years + total_estimate_population_25_to_34_years +
             total_estimate_population_35_to_44_years + total_estimate_population_45_to_64_years +
             total_estimate_population_65_years_and_over) %>%
    select(geography, totalpop)
  
  # Calculating the change: this data shows population flux by state from
  # 2016-2017. Percentage change is negligible (ie. .22%), so whole numbers seem
  # preferable here
  
  agg_total_pop <- left_join(by = "geography", total_pop_2017, total_pop_2016,
                             suffix = c(".17", ".16")) %>% 
    
    # [Final - initial], not the other way!!
    
    mutate(change = ((totalpop.17 - totalpop.16))) %>% 
    write_rds("aggregate_pops")
  
  # create an interim dataframe because I need to join 2 more frames to
  # agg_total_pop now that I grabbed the degree holder data
  
  total_pop_deg_1 <- left_join(by = "geography", agg_total_pop, bach_2017, suffix = c("", ".17"))
  
  # This dataframe is agg_total_pop with degree holder data joined to it by
  # state. Writing a new rds file for the shiny app to access. 
  
  total_plus_degs <- left_join(by = "geography", total_pop_deg_1, bach_2016, suffix = c(".17", ".16")) %>% 
    mutate(deg_change = (total_degrees.17 - total_degrees.16)) %>% 
    write_rds("aggregate_plus_degrees")
  
  
# create the interactive plot for the app
  # This is the tigris shape file download
  
  states <- states()
  
  # This attaches the shapefile to my data by state. This is cool!
  
  all_us <- geo_join(states, agg_total_pop, "NAME", "geography")
  
  # I edited this a bit to have a more even color scheme distribution, it
  # pertains to the legend's bracketing.
  
  bins <- c(-45000, -15000, 0, 15000, 30000,
            45000, 100000, 200000, Inf)
  
  # Color palette, and domain by variable, bins arg goes here.
  
  pal <- colorBin("YlOrRd", domain = all_us$change, bins = bins)
  
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
    
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addPolygons(
      
      # These are all asthetic edits, most are self-explanatory, so I won't bore
      # you with the details.
      
      fillColor = ~pal(change),
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
    
    addLegend(pal = pal, values = ~all_us$change, opacity = 0.7, title = NULL,
              position = "bottomright")
  
  # This is a script in progress, it will run in the environment but it won't
  # project in shiny. It doesn't throw an error, but it doesn't map either.
  # Shall consult with Kane / Albert later. Skipping the re-explanation for
  # brevity, I trust the reader's ability to infer from above. 
  
  pal <- colorBin("YlOrRd", domain = all_us$totalpop.17)
  
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
    addLegend(pal = pal, values = ~all_us$totalpop.17, opacity = 0.7, title = NULL,
              position = "bottomright")
  
  
  # table of agg total pop attempt
  
  # Taking the same data from beofre, I want the reader to see their state
  # aggregate and change on 'paper'. 
  
  agg_total_pop %>% gt() %>% 
    
    # Re-name the cols to something nicer
    
    cols_label(geography = "State",
               totalpop.17 = "2017",
               totalpop.16 = "2016",
               change = "Change") %>% 
    
    # Data is straight-forward, but labeling and citing is still a must.
    
    tab_header("Change in Total Population Counts") %>% 
    tab_spanner(label = "In the first year of Trump's Presidency",
                columns = vars(geography, totalpop.17, totalpop.16, change))
  
  
