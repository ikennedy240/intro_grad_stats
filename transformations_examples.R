library(tidyverse)
library(huxtable)
library(ggeffects)

options(scipen = 999)
### Simulation Example

n = 1e3
# dummy variables

# x is a random series with these three values
x <- sample(c('red','blue','lavender'), size = n, replace = TRUE)

# y takes on different values depending on x, with some noise
y <- case_when(x == 'red' ~ -10,
               x == 'blue' ~ 0,
               x == 'lavender' ~ 20) + rnorm(n, 0, 20)

plot(as_factor(x), y)
lm1 <- lm(y ~ x)
summary(lm1)

red = x=='red'
blue = x=='blue'
lavender = x=='lavender'

lm2 <- lm(y ~ blue + red + lavender)
summary(lm2)

lm3 <- lm(y ~ red + lavender)
summary(lm3)


huxreg(lm1, lm2, lm3)

# polynomial transformations

x <- rnorm(n)
hist(x)
y <- 50 - 34*x + 12*rnorm(n)
plot(x,y)
summary(lm(y ~ x))

# but we could imagine that the effect of x changes with its value
# ie, that there's a change in the slope

y <- 50 - 34*x + -10*x^2 + 12*rnorm(n)
plot(x,y)
lm1 <- lm(y ~ x)
summary(lm1)
plot(x,y)
abline(a = lm1$coefficients['(Intercept)'], b = lm1$coefficients['x'], col = 'blue')


lm2 <- lm(y ~ poly(x,2,raw=TRUE))
summary(lm2)
plot(x,y)
lines(seq(min(x), max(x), by = .3), predict(lm2, newdata = list('x' = seq(min(x), max(x), by = .3))), col = 'red')
huxreg(lm1,lm2)

# we can also have higher-order polynomials
y <- 50 - 34*x + -4*x^2 + 4*x^3 + 6*rnorm(n)
plot(x,y)

lm1 <- lm(y ~ x)
lm2 <- lm(y ~ poly(x,2))
lm3 <- lm(y ~ poly(x,3))
huxreg(lm1, lm2, lm3)
plot(x,y)
abline(a = lm1$coefficients['(Intercept)'], b = lm1$coefficients['x'], col = 'blue')
lines(seq(min(x), max(x), by = .3), predict(lm2, newdata = list('x' = seq(min(x), max(x), by = .3))), col = 'red')
lines(seq(min(x), max(x), by = .3), predict(lm3, newdata = list('x' = seq(min(x), max(x), by = .3))), col = 'purple')


# log transformations

## level - level
x <- rnorm(n)
hist(x)
y <- -3 + 2*x

ggplot(data = tibble(x= seq(-5,5,by=1)), aes(x))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_abline(intercept = 0, slope = 1)+
  ylim(-5,5)+
  xlim(-5,5)

summary(lm(y ~ x))


## Level - Log

x <- x - min(x)+1
y <- 4*log(x) + rnorm(n)

plot(x,y)
plot(log(x), y)

lm1 <- lm(y ~ x)
lm2 <- lm(y ~ log(x))
huxreg(lm1, lm2)

plot(x,y)
abline(a = lm1$coefficients['(Intercept)'], 
       b = lm1$coefficients['x'], col = 'blue')
lines(seq(min(x), max(x), by = .3), 
      predict(lm2, 
              newdata = list('x' = seq(min(x), max(x), by = .3))), 
      col = 'red')


# interpretation : a 1 percent increase in X results in a b/100 unit increase in Y
# in this case, if x increases by 1%, we expect y to increase by .0399 (or .04 really)

## Log - Level
x <- rnorm(n)
y <- exp(.8*x + rnorm(n)) 

plot(x,y)
plot(x, log(y))
lm1 <- lm(y ~ x)

lm2 <- lm(log(y) ~ x)
huxreg(lm1, lm2)

# interpretation : a unit increase in X results in a (exp(b)-1)*100 percent increase in Y
# when b < .15 then (exp(b)-1)*100 is roughly equal to b*100
# in this case exp(.8)-1 = 1.22, so a one unit increase in x means we expect a 122% increase in y

## Log - Log

x <- x - min(x)+1
y <- exp(7*log(x) + 2*rnorm(n)) 

plot(x,y)

plot(x, log(y))
plot(log(x), log(y))

lm1 <- lm(y ~ x)

lm2 <- lm(log(y) ~ x)

lm3 <-  lm(log(y) ~ log(x))

huxreg(lm1, lm2, lm3)


# interpretation : a 1 percent increase in X results in a b% increase in Y
# in this case, a 1 percent increase in x means we expect a 4% increase in y

### Real Example
load('data/CPS_2016_2021.Rdata')


cps_clean <- CPS_2016_2021_new %>% select(AGE, INCTOT, NCHILD) %>%
  filter(INCTOT< 1e8, AGE<=81) %>% drop_na() %>%
  mutate(log_age = log(AGE), log_inc = log(INCTOT+abs(min(INCTOT))+1))


## dummy and categorical variables

lm1 <- lm(INCTOT ~ NCHILD, data = cps_clean)
summary(lm1)

cps_clean <- cps_clean %>% mutate(any_child = as_factor(NCHILD>0), child_cat = as.character(NCHILD))

lm2 <- lm(INCTOT ~ any_child, data = cps_clean)
summary(lm2)


lm3 <- lm(INCTOT ~ child_cat, data = cps_clean)
summary(lm3)


huxreg(lm1, lm2, lm3)

plot(ggeffect(lm1, terms = 'NCHILD'))+ylim(0,1e5)
plot(ggeffect(lm2, terms = 'any_child'))+ylim(0,1e5)
plot(ggeffect(lm3, terms = 'child_cat'))+ylim(0,1e5)

## log and polynomial transformations

cps_clean %>% pivot_longer(c(AGE,INCTOT,log_age,log_inc)) %>%
  ggplot(aes(value, color = name, fill = name))+
  geom_density()+
  facet_wrap(~name, scales = 'free')+
  theme_minimal()





cps_clean %>% sample_n(1e4) %>% ggplot(aes(AGE, INCTOT))+
  geom_point(alpha = 0.2)+
  theme_minimal()+
  scale_y_continuous(labels = function(x) paste0('$',x))


## explicitly drop people with negative income
cps_clean_noneg <- cps_clean %>% filter(INCTOT>0)
cps_samp <- cps_clean_noneg %>% sample_n(2e4)

print(paste('We lost', nrow(cps_clean) - nrow(cps_clean_noneg), 'rows, which is', round((nrow(cps_clean) - nrow(cps_clean_noneg))/nrow(cps_clean) * 100,2), 'percent'))


cps_samp %>% ggplot(aes(AGE, INCTOT))+
  geom_point(alpha = 0.2)+
  scale_x_log10()+
  scale_y_log10(labels = function(x) paste0('$',x))+
  theme_minimal()

cps_samp %>% filter(INCTOT>=0) %>% ggplot(aes(AGE, INCTOT))+
  geom_point(alpha = 0.2)+
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal()

cps_clean_noneg %>% pivot_longer(c(AGE,INCTOT,log_age,log_inc)) %>%
  ggplot(aes(value, color = name, fill = name))+
  geom_density()+
  facet_wrap(~name, scales = 'free')+
  theme_minimal()

lm1 <- lm(INCTOT ~ AGE, data = cps_clean_noneg)
summary(lm1)


lm2 <- lm(INCTOT ~ log(AGE), data = cps_clean_noneg)
summary(lm2)

# note that from here on, I'm applying a transformation to the income variable to keep the 
# negative values therein

lmsq <- lm(INCTOT ~ poly(AGE, 2), data = cps_clean_noneg)
summary(lmsq)

lm3 <- lm(log(INCTOT) ~ (AGE), data = cps_clean_noneg)
summary(lm3)

lm4 <- lm(log(INCTOT) ~ log(AGE), data = cps_clean_noneg)
summary(lm4)

lm5 <- lm(log(INCTOT) ~ poly(log(AGE),2), data = cps_clean_noneg)
summary(lm5)


huxreg(lm1,lm2,lm3, lmsq,lm4, lm5)



huxreg(lm1)
pred_1 <- ggpredict(lm1, terms = 'AGE')
plot(pred_1)+geom_point(aes(AGE, INCTOT), data = cps_samp, alpha = .05)+
  scale_x_log10()+
  scale_y_log10()

huxreg(lm2)
pred_2 <- ggpredict(lm2, terms = 'AGE')
plot(pred_2)+geom_point(aes(AGE, INCTOT), data = cps_samp, alpha = .05)+
  scale_x_log10()+
  scale_y_log10()

huxreg(lm4)
pred_4 <- ggpredict(lm4, terms = 'AGE')
plot(pred_4)+geom_point(aes(AGE, INCTOT), data = cps_samp, alpha = .05)+
  scale_x_log10()+
  scale_y_log10()

huxreg(lmsq)
pred_sq <- ggpredict(lmsq, terms = 'AGE')
plot(pred_sq)+geom_point(aes(AGE, INCTOT), data = cps_samp, alpha = .05)+
  scale_x_log10()+
  scale_y_log10()

huxreg(lm5)
pred_5 <- ggpredict(lm5, terms = 'AGE')
plot(pred_5)+geom_point(aes(AGE, INCTOT), data = cps_samp, alpha = .05)+
  scale_x_log10()+
  scale_y_log10()




### version using a transformation to maintain negative numbers
cps_samp %>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1) %>% ggplot(aes(AGE, INCTOT))+
  geom_point(alpha = 0.2)+
  scale_x_log10()+
  scale_y_log10(lim = c(-min(cps_samp$INCTOT), max(cps_samp$INCTOT)-min(cps_samp$INCTOT)+1),
                labels = function(x) paste0('$',x+min(cps_samp$INCTOT)-1))+
  theme_minimal()


lm1 <- lm(INCTOT ~ AGE, data = cps_clean)
summary(lm1)


lm2 <- lm(INCTOT ~ log(AGE), data = cps_clean)
summary(lm2)

# note that from here on, I'm applying a transformation to the income variable to keep the 
# negative values therein

lmsq <- lm(INCTOT ~ poly(AGE, 2), data = cps_clean %>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1))
summary(lmsq)

lm3 <- lm(log(INCTOT) ~ (AGE), data = cps_clean %>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1))
summary(lm3)

lm4 <- lm(log(INCTOT) ~ log(AGE), data = cps_clean %>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1))
summary(lm4)

lm5 <- lm(log(INCTOT) ~ poly(log(AGE),2), data = cps_clean %>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1))
summary(lm5)


huxreg(lm1,lm2,lm3, lmsq,lm4, lm5)

cps_samp <- cps_clean %>% sample_n(2e4)


pred_1 <- ggpredict(lm1, terms = 'AGE')
plot(pred_1)+geom_point(aes(AGE, INCTOT), data = cps_samp%>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1), alpha = .05)+
  scale_x_log10()+
  scale_y_log10(lim = c(-min(cps_samp$INCTOT), max(cps_samp$INCTOT)-min(cps_samp$INCTOT)+1),
                labels = function(x) paste0('$',x+min(cps_samp$INCTOT)-1))

pred_2 <- ggpredict(lm2, terms = 'AGE')
plot(pred_2)+geom_point(aes(AGE, INCTOT), data = cps_samp%>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1), alpha = .05)+
  scale_x_log10()+
  scale_y_log10(lim = c(-min(cps_samp$INCTOT), max(cps_samp$INCTOT)-min(cps_samp$INCTOT)+1),
                labels = function(x) paste0('$',x+min(cps_samp$INCTOT)-1))

pred_4 <- ggpredict(lm4, terms = 'AGE')
plot(pred_4)+geom_point(aes(AGE, INCTOT), data = cps_samp%>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1), alpha = .05)+
  scale_x_log10()+
  scale_y_log10(lim = c(-min(cps_samp$INCTOT), max(cps_samp$INCTOT)-min(cps_samp$INCTOT)+1),
                labels = function(x) paste0('$',x+min(cps_samp$INCTOT)-1))

pred_sq <- ggpredict(lmsq, terms = 'AGE')
plot(pred_sq)+geom_point(aes(AGE, INCTOT), data = cps_samp %>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1), alpha = .05)+
  scale_x_log10()+
  scale_y_log10(lim = c(-min(cps_samp$INCTOT), max(cps_samp$INCTOT)-min(cps_samp$INCTOT)+1),
                labels = function(x) paste0('$',x+min(cps_samp$INCTOT)-1))

huxreg(lm5)
pred_5 <- ggpredict(lm5, terms = 'AGE')
plot(pred_5)+geom_point(aes(AGE, INCTOT), data = cps_samp%>% mutate(INCTOT = INCTOT+abs(min(cps_clean$INCTOT))+1), alpha = .05)+
  scale_x_log10()+
<<<<<<< HEAD
  scale_y_log10(lim = c(-min(cps_clean$INCTOT), max(cps_samp$INCTOT)-min(cps_clean$INCTOT)+1),
                labels = function(x) paste0('$',x-abs(min(cps_clean$INCTOT))))

huxreg(lm5)




=======
  scale_y_log10()

plot(pred_5)+geom_point(aes(AGE, INCTOT), data = cps_samp, alpha = .01)
>>>>>>> 9484e44490039bb3015f39a811ccac28000c6cc4
