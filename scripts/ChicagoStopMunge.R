library(tidyverse)
library(tidycensus)
library(ggeffects)

# data from https://home.chicagopolice.org/statistics-data/isr-data/

stops <- vroom::vroom('data/2022-ISR.csv')

stops %>% glimpse()

stops_sum <- stops %>% select(JUVENILE_I, CREATED_DATE, AGE, SEX_CODE_CD, RACE_CODE_CD, ZIP_CD, SEARCH_PROPERTY_I) %>%
  drop_na() %>%
  mutate(ZIP_CD = as.character(ZIP_CD)) %>%
  group_by(ZIP_CD) %>% summarise(total_stops = n(), 
                                 avg_age_stoped = mean(AGE), 
                                 white_stops = sum(RACE_CODE_CD %in% c('WHI','WHT')),
                                 black_stops = sum(RACE_CODE_CD == 'BLK'),
                                 latine_stops = sum(RACE_CODE_CD %in% c('WBH','WWH')),
                                 aapi_stops = sum(RACE_CODE_CD == 'API'),
                                 men_stops = sum(SEX_CODE_CD == 'M'), 
                                 women_stops = sum(SEX_CODE_CD == 'F'), 
                                 nb_stops = sum(SEX_CODE_CD == 'X'),
                                 first_stop = min(CREATED_DATE))


acs21 <- load_variables('2021', 'acs5')

vars <- c('total_pop' = 'B01001A_001', 
          'white' =  'B03002_003',
          'black' = 'B03002_004',
          'asian' = 'B03002_006',
          'nhpi' = 'B03002_007',
          'latine' = 'B03002_012',
          'med_hh_income' = 'B19119_001',
          'below_poverty' = 'B17001A_002',
          'o25_bachelors_degree' = 'B27019_018',
          'o25_total' = 'B27019_002')
acs21_data <- get_acs('zcta', variables = vars)

acs21_data <- acs21_data %>% select(GEOID, variable, estimate) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(prop_white = white/total_pop,
         prop_black = black/total_pop,
         prop_aapi = (asian+nhpi)/total_pop,
         prop_latine = latine/total_pop,
         prop_below_poverty = below_poverty/total_pop,
         prop_o25ba = o25_bachelors_degree/o25_total)

stops_acs <- stops_sum %>% left_join(acs21_data, by = c('ZIP_CD' = 'GEOID')) %>%
  mutate(log_stops = log(total_stops))

cor(stops_acs %>% drop_na() %>% select(total_stops, total_pop, prop_black, prop_below_poverty, prop_o25ba))

stops_acs %>% ggplot(aes(total_stops))+
  geom_histogram()

stops_acs %>% ggplot(aes(prop_below_poverty, total_stops))+
  geom_point()

stops_acs %>% ggplot(aes(prop_o25ba, total_stops))+
  geom_point()

stops_acs %>% ggplot(aes(prop_black, total_stops))+
  geom_point()

stops_acs %>% ggplot(aes(prop_white, total_stops))+
  geom_point()

lm1 <- lm(total_stops ~ prop_o25ba, data = stops_acs)
summary(lm1)


lm2 <- lm(total_stops ~ prop_o25ba + prop_white, data = stops_acs)
summary(lm2)


lm3 <- lm(total_stops ~ prop_o25ba + prop_white + med_hh_income, data = stops_acs)
summary(lm3)

lm4 <- lm(total_stops ~ prop_o25ba + prop_white + med_hh_income*prop_white, data = stops_acs)
summary(lm4)

ggp3 <- ggeffect(lm3, terms = c('prop_white', 'med_hh_income'))
plot(ggp3)

ggp4 <- ggeffect(lm4, terms = c('prop_white', 'med_hh_income'))
plot(ggp4)


vars <- c('total_pop' = 'B01001A_001', 
          'white' =  'B03002_003',
          'black' = 'B03002_004',
          'asian' = 'B03002_006',
          'nhpi' = 'B03002_007',
          'latine' = 'B03002_012',
          'med_hh_income' = 'B19119_001',
          'below_poverty' = 'B17001A_002',
          'o25_bachelors_degree' = 'B27019_018',
          'o25_total' = 'B27019_002')
acs21_tract <- get_acs('tract', state = 'IL', variables = vars)


acs21_tract <- acs21_tract %>% select(GEOID, variable, estimate) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(prop_white = white/total_pop,
         prop_black = black/total_pop,
         prop_aapi = (asian+nhpi)/total_pop,
         prop_latine = latine/total_pop,
         prop_below_poverty = below_poverty/total_pop,
         prop_o25ba = o25_bachelors_degree/o25_total)

lm1 <- lm(med_hh_income ~ prop_o25ba, data = acs21_tract)
summary(lm1)

ggp1 <- ggeffect(lm1, terms = c('prop_o25ba'))
plot(ggp1)

lm2 <- lm(med_hh_income ~ prop_o25ba + prop_white, data = acs21_tract)
summary(lm2)

ggp2 <- ggeffect(lm2, terms = c('prop_o25ba','prop_white'))
plot(ggp2)

lm3 <- lm(med_hh_income ~ prop_o25ba + prop_white + prop_white*prop_o25ba, data = acs21_tract)
summary(lm3)

ggp3 <- ggeffect(lm3, terms = c('prop_o25ba', 'prop_white'))
plot(ggp3)


