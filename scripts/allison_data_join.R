library(tidyverse)
library(tigris)
library(sf)

us_counties <- counties(year = 2017)

pollution <- read_csv('data/county_pollution.csv') %>% filter(year == 2017) %>%
  mutate(statefips = if_else(statefips<10, str_c('0', statefips), as.character(statefips)),
         countyfips = if_else(countyfips<100, if_else(countyfips<10,str_c('00', countyfips), str_c('0', countyfips)), as.character(countyfips))) %>%
  group_by(statefips, countyfips) %>% summarise(across(contains('PM25'), mean))

health <- read_csv('data/city_health.csv') %>% filter(Year == 2017)


health_geo <- health %>% mutate(lat = str_extract(GeoLocation, '\\d*\\.\\d*'),
                  lon = str_extract(GeoLocation, '-\\d*\\.\\d*')) %>% 
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = st_crs(us_counties))


health_joined <- us_counties %>% transmute(statefips = STATEFP, countyfips = COUNTYFP, name = NAMELSAD) %>%
  st_join(health_geo, st_contains)

full_joined <- health_joined %>% st_drop_geometry() %>% drop_na(Year) %>% left_join(pollution)
full_joined %>% count(MeasureId, Data_Value_Type)

county_mean <- full_joined %>% filter(DataValueTypeID == 'CrdPrv') %>%
  group_by(statefips, countyfips, StateAbbr, StateDesc, name, MeasureId) %>%
  summarise(Data_Value = mean(Data_Value, na.rm = TRUE), across(contains('PM25'), function(x) mean(x, na.rm = TRUE)))

county_mean %>% pivot_wider(names_from = MeasureId, values_from = Data_Value) %>%
  write_csv('data/county_health_ppm.csv')
