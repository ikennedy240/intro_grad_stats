library(tidyverse)
load('data/PLACES_NaNDA_sample.Rdata')

clean_places <- PLACES_NaNDA_sample %>% filter(!is.na(asthma),!is.na(popden13_17), !is.na(ppov13_17)) %>%
  select(LocationID, asthma, popden13_17,ppov13_17)


clean_places %>% ggplot(aes(popden13_17, asthma))+
  geom_point()

# doesn't seem like much of an association

# let's try a regression

lm1 <- lm(asthma ~ popden13_17, data = clean_places)
summary(lm1)


# let's look at asthma and poverty
clean_places %>% ggplot(aes(ppov13_17, asthma))+
  geom_point()

# wow, that's totally different

# let's look at the regression



lm2 <- lm(asthma ~ ppov13_17, data = clean_places)
summary(lm2)
intercept <- lm2$coefficients[['(Intercept)']]
slope <- lm2$coefficients[['ppov13_17']]

# plot the points and the line
clean_places %>% ggplot(aes(ppov13_17, asthma))+
  geom_point()+
  geom_abline(slope = slope, intercept = intercept, size = 2, color = 'blue')

plot(lm2)

