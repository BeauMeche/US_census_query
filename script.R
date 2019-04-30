library(tidyverse)
library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)
library(ggplot2)

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


total_pop_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "total_estimate_population")|
                                         str_detect(names(census_2017_nomargin), pattern = "geography")] %>%
  mutate(totalpop = total_estimate_population_18_to_24_years + total_estimate_population_25_to_34_years +
           total_estimate_population_35_to_44_years + total_estimate_population_45_to_64_years +
           total_estimate_population_65_years_and_over) %>%
  select(geography, totalpop)
