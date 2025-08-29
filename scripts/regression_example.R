library(tidyverse)
load('data/PLACES_NaNDA_sample.Rdata')

clean_places <- PLACES_NaNDA_sample %>% 
  filter(!is.na(asthma),!is.na(popden13_17), !is.na(ppov13_17)) %>%
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


lm3 <- lm(asthma ~ ppov13_17 + popden13_17, data = clean_places)
summary(lm3)
# plot the points and the line
clean_places %>% ggplot(aes(ppov13_17, asthma))+
  geom_point()+
  geom_abline(slope = slope, intercept = intercept, linewidth = 2, color = 'blue')

# we can also use the `ggeffects` package to make a cool predictions plot
library(ggeffects)

predictions <- ggeffect(lm2, terms = 'ppov13_17')
predictions # check this out, predicted values across various levels fo poverty, with confidence intervals

plot(predictions) # easy, but I'm not sure about the y-axis

plot(predictions) + ylim(0,18)
# plot shows that the model expects increased asthma as poverty increases. It also shows that the model is 
# slightly more uncertain about high-poverty neighborhoods


x <- 1:100
y <- rnorm(100, 500, 300)
plot(x,y)

clean_places %>% sample_n(150) %>%
  ggplot(aes(ppov13_17, asthma))+
  geom_point()+
  theme_classic()+
  labs(title = 'Asthma by Poverty', x = 'Poverty Proportion', y = 'Asthma Prevalence')


slope <- -3
intercept <- 4
x <- seq(-5,5, by = 0.1)
y = slope*x + intercept

ggplot(tibble(x,y), aes(x,y))+
  geom_line(color = 'red')+
  scale_x_continuous(breaks = seq(-5,5,by = 2), lim = c(-5,5))+
  scale_y_continuous(breaks = seq(-5,5,by = 2), lim = c(-5,5))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_minimal()

