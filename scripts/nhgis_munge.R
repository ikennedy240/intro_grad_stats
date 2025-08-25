ethno_race <- read_csv('~/Downloads/nhgis0019_csv/nhgis0019_ds248_2020_county.csv')


edu_inc <- read_csv('~/Downloads/nhgis0019_csv/nhgis0019_ds254_20215_county.csv')

ethno_race <- ethno_race %>% transmute(STATE, COUNTY, GISJOIN, 
                                       total_population = U7C001,
                                       latinx = U7C002,
                                       white  = U7C005,
                                       black = U7C006,
                                       asian = U7C008)
edu_inc <- edu_inc %>% transmute(STATE, COUNTY, GISJOIN,
                                 less_than_hs = AOP8E002+AOP8E003+AOP8E004+AOP8E005+AOP8E006+AOP8E007+AOP8E008+AOP8E009+AOP8E010+AOP8E011+AOP8E012+AOP8E013+AOP8E014+AOP8E015+ AOP8E016,
                                 high_school_diploma = AOP8E017+AOP8E018,
                                 Some_college = AOP8E019+AOP8E020,
                                 Associates_degree = AOP8E021,
                                 Bachelors_degree = AOP8E022,
                                 Masters_degree = AOP8E023,
                                 Professional_school_degree = AOP8E024,
                                 Doctorate_degree = AOP8E025,
                                 median_hh_income = AOQIE001)

merged_df <- ethno_race %>% left_join(edu_inc)

merged_df %>% write_csv("~/Downloads/counties.csv")

merged_df %>% 
  filter(STATE %in% c('New York', 'Illinois')) %>%
  group_by(STATE) %>% sample_n(40) %>% write_csv("~/Downloads/il_ny_sample.csv")

hist(merged_df$median_hh_income)

slope = -1.2
intercept = 3

ggplot(tibble(x = seq(-5,5, by = 0.01)) %>% 
         mutate(y = slope*x + intercept) %>%
         filter((y>-5),(y<5)), aes(x,y))+
  geom_line()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_minimal()+
  ylim(-5,5)+
  xlim(-5,5)

clean_places <- PLACES_NaNDA_sample %>% filter(!is.na(count_open_parks), !is.na(asthma)) %>%
  mutate(cat_parks = if_else(count_open_parks ==0, 'no parks','some parks'))

clean_places %>% group_by(cat_parks) %>% summarize(mean = mean(asthma), sd = sd(asthma))

clean_places <- clean_places %>%
  mutate(cat_cancer = case_when(cancer <= mean(cancer) ~ 'low cancer',
                                cancer > mean(cancer) ~ 'high cancer'),
         cat_cancer = ordered(cat_cancer, levels = c('low cancer', 'high cancer')),
         cat_pov = case_when(ppov13_17<=quantile(ppov13_17, .25) ~'low poverty',
                             ppov13_17>quantile(ppov13_17, .75) ~ 'high poverty'))


