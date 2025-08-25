library(tidyverse)

# marriage data from https://github.com/fivethirtyeight/data/blob/master/marriage/both_sexes.csv
# shares of never married people by deomographic
marriage <- read_csv('data/marriage.csv')

# measles data from https://github.com/owid/owid-datasets/blob/master/datasets/US%20Measles%20Cases%20and%20Deaths%20(OWID%2C%202017)/US%20Measles%20Cases%20and%20Deaths%20(OWID%2C%202017).csv
measles <- read_csv('data/measles.csv')

m_m <- measles %>% transmute(year = Year, measles = `Reported Measles Case Rate (OWID, 2017)`) %>%
  filter(!is.na(measles)) %>% full_join(marriage %>% transmute(year, never_married = all_2534))

m_m %>% pivot_longer(-year) %>% filter(year>=1960, !is.na(value)) %>%
  ggplot(aes(year, value, color = name))+
  geom_line()+
  facet_wrap(~name, scales = 'free')

m_m %>% filter(!is.na(never_married)) %>% cor()

m_m %>% filter(!is.na(never_married)) %>% 
  mutate(measles = log(measles), never_married = never_married*100) %>%
  ggplot(aes(measles, never_married))+
  geom_point()+
  scale_x_continuous(breaks = c(-2.5,0,2.5,5), labels = round(exp(c(-2.5,0,2.5,5)),2))+
  scale_y_continuous(labels = function(x) paste0(x,'%'))+
  theme_classic()+
  ggtitle('Never Marriage Rate by Measles Rate 1960-2020 USA')+
  xlab('Measles (Log Scale)')+
  ylab('Never Married')


lm(never_married*100 ~ measles, data = m_m) %>% summary()
