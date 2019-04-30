library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(tigris)
library(tidyverse)

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_2017_edu_attainment.csv",
              destfile = "2017_data.csv",
              mode = "wb")

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_edu_attainment_2016.csv",
              destfile = "2016_data.csv", 
              mode = "wb")

data_2017 <- read_csv(file = "2017_data.csv", skip = 1) %>% 
  clean_names()

census_2017_nomargin <- data_2017[, ! str_detect(names(data_2017), pattern = "margin_of_error")] 

data_2016 <- read_csv(file = "2016_data.csv", skip = 1) %>% 
  clean_names()

census_2016_nomargin <- data_2016[, ! str_detect(names(data_2016), pattern = "margin_of_error")]

# table for the first plot in the app

young_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "total_estimate_population")|
                                     str_detect(names(census_2017_nomargin), pattern = "geography")] %>% 
  select(geography, total_estimate_population_18_to_24_years) %>% 
  arrange(desc(total_estimate_population_18_to_24_years)) %>% 
  head(10) %>% 
  write_rds("young_people_locations")

# this is the first plot in the app

      ggplot(young_2017) +
      geom_bar(stat = "identity", aes(x = geography, y = total_estimate_population_18_to_24_years, 
                                      fill = geography), show.legend = FALSE) +
      coord_flip() + theme_fivethirtyeight() +
      labs(
        title = "Where are the young people?",
        subtitle = "Shown: the 10 states with the most people aged 18 to 24", 
        caption = "Source: the U.S. Census")

# total populations and  aggregate table creation
      
  total_pop_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "total_estimate_population")|
                                           str_detect(names(census_2017_nomargin), pattern = "geography")] %>%
    mutate(totalpop = total_estimate_population_18_to_24_years + total_estimate_population_25_to_34_years +
             total_estimate_population_35_to_44_years + total_estimate_population_45_to_64_years +
             total_estimate_population_65_years_and_over) %>%
    select(geography, totalpop)
  
  
  total_pop_2016 <- census_2016_nomargin[, str_detect(names(census_2016_nomargin), pattern = "total_estimate_population")|
                                           str_detect(names(census_2016_nomargin), pattern = "geography")] %>%
    mutate(totalpop = total_estimate_population_18_to_24_years + total_estimate_population_25_to_34_years +
             total_estimate_population_35_to_44_years + total_estimate_population_45_to_64_years +
             total_estimate_population_65_years_and_over) %>%
    select(geography, totalpop)
  
  # this data shows population flux by state from 2016-2017. Percentage change is
  # negligible (ie. .22%), so whole #s seems preferable here
  
  agg_total_pop <- left_join(by = "geography", total_pop_2017, total_pop_2016,
                             suffix = c(".17", ".16")) %>% 
    mutate(sum = totalpop.17 + totalpop.16) %>% select(-sum) %>% 
    mutate(change = ((totalpop.17 - totalpop.16))) %>% 
    write_rds("aggregate_pop") %>% 
    write_rds("aggregate_pops")
  
  
# create the interactive plot for the app
  
  states <- states()
  all_us <- geo_join(states, agg_total_pop, "NAME", "geography")
  
  bins <- c(-45000, -15000, 0, 15000, 30000,
            45000, 100000, 200000, Inf)
  
  pal <- colorBin("YlOrRd", domain = all_us$change, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g people",
    all_us$geography, all_us$change
  ) %>% lapply(htmltools::HTML)
  
  leaflet(all_us) %>%
    setView(-96, 37.8, 4) %>%
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
  
  agg_total_pop %>% gt() %>% 
    cols_label(geography = "State",
               totalpop.17 = "2017",
               totalpop.16 = "2016",
               change = "Change") %>% 
    tab_header("Change in Total Population Counts") %>% 
    tab_spanner(label = "In the first year of Trump's Presidency",
                columns = vars(geography, totalpop.17, totalpop.16, change))
  
  
