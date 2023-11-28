library(tidyverse)
library(tidycensus)

# data from https://home.chicagopolice.org/statistics-data/isr-data/

stops <- read_csv('~/Downloads/2022-ISR.csv')

stops %>% glimpse()

stops %>% select(JUVENILE_I, CREATED_DATE, AGE, SEX_CODE_CD, RACE_CODE_CD, ZIP_CD, SEARCH_PROPERTY_I) %>%
  group_by(ZIP_CD) %>% summarise(total_stops = n(), avg_age_stoped = mean(AGE), 
                                 white_stops = sum(RACE_CODE_CD %in% c('WHI','WHT')),
                                 black_stops = sum(RACE_CODE_CD == 'BLK'),
                                 latine_stops = sum(RACE_CODE_CD %in% c('WHI','WHT')),
                                 men_stops = sum(SEX_CODE_CD == 'M'), women_stops = sum(SEX_CODE_CD == 'F'), 
                                 nb_stops = sum(SEX_CODE_CD == 'X'),
                                 first_stop = min(CREATED_DATE))
