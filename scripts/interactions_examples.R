#### INTERACTIONS IN-CLASS EXAMPLES

library(tidyverse)
library(tidycensus)
library(huxtable)
library(ggeffects)
# I'm going to start with an example we looked at before with the tidycensus example
# I'm gathering data about racial composition and rent for tracts in Illinois:

my_vars <- c(re_total = 'B03002_001',
             white = 'B03002_003',
             black = 'B03002_004',
             aian = 'B03002_005',
             asian = 'B03002_006',
             nhopi = 'B03002_007',
             latinx = 'B03002_012',
             rent = 'B25058_001')

acs_21 <- get_acs('tract', variables = my_vars, year = 2021, state = 'IL')

# then I'm going to clean up the data a bit, so that I have one observation per row
# and one column per variable
acs_21 <- acs_21 %>% 
  # we're dropping the margin of error because we won't use it here
  select(-moe) %>%
  pivot_wider(names_from = 'variable', values_from = 'estimate') %>%
  mutate(across(c(white, black, aian, asian, nhopi,latinx), .names = "{.col}_prop",.fns = function(x) 100*x/re_total)) %>%
  drop_na()

# Now let's look at a polot
acs_21 %>% ggplot(aes(white_prop, rent))+
  geom_point()+
  theme_classic()

# It looks a little messy--maybe there is some kind of curve here?
# Maybe some heteroskedasticity? Are my variables meesed up?
hist(acs_21$white_prop)
hist(acs_21$rent)

# They look strange, but not terrible? Maybe a log transform on rent would help?
acs_21 %>% ggplot(aes(white_prop, log(rent)))+
  geom_point()+
  theme_classic()

# hmm even with the logged rent, things look strange, like there's a curve
# Ok, let's try some regression models and see what we can do

level_level <- lm(rent ~ white_prop, data = acs_21)
log_level <- lm(log(rent) ~ white_prop, data = acs_21)
log_poly <- lm(log(rent) ~ poly(white_prop, 2, raw = TRUE), data = acs_21)

huxreg(list('Level' = level_level, 'Log-level' = log_level, 'Polynomial' = log_poly))

plot(ggeffect(level_level, term = 'white_prop'))+
  geom_point(aes(white_prop, rent), data = acs_21, alpha = 0.2)

plot(ggeffect(log_level, term = 'white_prop'))+
  geom_point(aes(white_prop, log(rent)), data = acs_21, alpha = 0.2)

plot(ggeffect(log_poly, term = 'white_prop'))+
  geom_point(aes(white_prop, log(rent)), data = acs_21, alpha = 0.2)


## what's really going on?

# let's make a new plot that changes the color of the points depending on whether
# they're from cook county
acs_21 %>% mutate(cook_county = str_detect(NAME, 'Cook County')) %>%
  ggplot(aes(white_prop, rent, color = cook_county))+
  geom_point(alpha = .5)+
  theme_classic()

# So, it seems like the relationship between white proportion and rent is DIFFERENT
# depending on whether the tract is in Cook County or not. Hmm. Why would that be?


# It turns out we have a took in our regression toolbox to model this kind of relationship:
# it's called an interaction term, which, mathematically, just means including the product between
# two of our variables in our model

## Let's try a simulated example.

# Imagine two variables, x and z, which contribute to y
n = 1e3

# z is either 0 or 1, with each appearing ~50% of the time
z <- rbinom(n,1,.5)

# x is random normal deviate plus 3z
# so x and z are correlated
x <- rnorm(n) + 3*z

# additive model
y_add <- x - z + rnorm(n)

plot(x, y_add)
plot(z, y_add)
ggplot(tibble(x,y_add,z), aes(x,y_add, color = z))+
  geom_point()

# we know how to model this relationship in regression
lm_add_orig <- lm(y_add ~ x + z)
summary(lm_add)

# estimate expected values
add_orig_fx <- ggeffect(lm_add_orig, terms = c('x','z'))

# plot them with the data to see the fit
add_orig_fx %>% as_tibble() %>% mutate(group_col = if_else(group == '0', '0','1')) %>%
  ggplot(aes(x, predicted, colour = group_col, fill = group_col))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),alpha = 0.2, linewidth = 0)+
  geom_point(aes(x,y_add,color = group_col), data = tibble(x,y_add,group_col = as.character(z)), alpha = 0.15)+
  theme_classic()

# note that even though in this example z causes x (ie x depends on the value of z)
# the mdoel finds x to be more significant! Just a reminder to not interpret significance
# as causality

# But let's imagine a different z, one that influences y not additively, 
# but in interaction -- when z is 1 the slope for x is negative, but when 
# z is zero, the slope for x is positive:
z <- rbinom(n,1,.5)
x <- rnorm(n) + 3*z
y_int <- x + z - x*z + rnorm(n)

df <- tibble(x,y_int,z) %>% mutate(group_col = as.character(z))

df %>% ggplot(aes(as_factor(z)))+
  geom_bar()+
  theme_classic()

df %>% ggplot(aes(x))+
  geom_histogram()+
  theme_classic()


df %>% ggplot(aes(y_int))+
  geom_histogram()+
  theme_classic()


plot(x, y_int)
plot(z, y_int)
ggplot(tibble(x,y_int,z), aes(x,y_int, color = as.character(z)))+
  geom_point(alpha = 0.5)+
  theme_classic()

# this looks more like our cook county colored plot, doesn't it?

# what happens if we try to model this additively
lm_add <- lm(y_int ~ x + z)
summary(lm_add)

# ok, the coefs aren't right (should be 1 and 1!) 
# let's see a plot

# estimate expected values
add_int_fx <- ggeffect(lm_add, terms = c('x','z'))

# plot them with the data to see the fit
add_int_fx %>% as_tibble() %>% mutate(group_col = if_else(group == '0', '0','1')) %>%
  ggplot(aes(x, predicted, colour = group_col, fill = group_col))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),alpha = 0.2, linewidth = 0)+
  geom_point(aes(x,y_int,color = group_col), data = tibble(x,y_int,group_col = as.character(z)), alpha = 0.15)+
  theme_classic()

# this is clearly wrong!!!

# we know there's a group here, but if we didn't, we might be tempted to
# try a squared term in x

lm_sq <- lm(y_int ~ poly(x,2, raw = TRUE), data = df)
lm_sq_z <- lm(y_int ~ poly(x,2, raw = TRUE)+ z, data = df)
huxreg(lm_add,lm_sq,lm_sq_z)

# the polynomial model fits the data much better than the linear model,
# even if it ignores z!

# let's look at a plot
# estimate expected values
sq_int_fx <- ggeffect(lm_sq_z, terms = c('x','z[0,1]'))

# plot them with the data to see the fit
sq_int_fx %>% as_tibble() %>% mutate(group_col = if_else(group == '0', '0','1')) %>%
  ggplot(aes(x, predicted, colour = group_col, fill = group_col))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),alpha = 0.2, linewidth = 0)+
  geom_point(aes(x,y_int,color = group_col), data = df, alpha = 0.15)+
  theme_classic()

# it fits the data _ok_, though note that if we had higher values of x when z was zero
# or lower values of x when Z was 1, we would miss by a lot
# so even though the squared term _seems_ to fit well, it doesn't actually reflect
# the data generating process well -- and we'd only know that if we had access to the 
# grouping column (or other interaction term) and we thought to test it

# if we do have the interaction term, we can run that model

lm_int <- lm(y_int ~ x + z + x*z, data = df)

huxreg(list('add' = lm_add,'squared' = lm_sq_z,'int' = lm_int))
# if we wer eonly going by modle fit, the interactive model isn't much better
# but, in this case we _know_ the correct coefficients (all 1s) and this model
# recovers them

# let's look at a plot
# estimate expected values
int_int_fx <- ggeffect(lm_int, terms = c('x','z[0,1]'))

# plot them with the data to see the fit
int_int_fx %>% as_tibble() %>% mutate(group_col = if_else(group == '0', '0','1')) %>%
  ggplot(aes(x, predicted, colour = group_col, fill = group_col))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),alpha = 0.2, linewidth = 0)+
  geom_point(aes(x,y_int,color = group_col), data = df, alpha = 0.15)+
  theme_classic()

# our interaction term captures the idea that Z moderates the effect of x on y:
# when z is low, x has a positive liner relationship with y. 
# But when z is high, x has no relationship with y - but z increases y slightly.


### Try it out on the Cook Data - Will an interaction work?

#* 1. Fit a model that regresses the rent variable on the white proportion, whether the 
#* tract is in Cook County, and an interaction between those variables.

#* 2. Use the `huxreg` function to compare your model to the previous models fit to that data.
#* Does the interaction model fit worse, about as well, or better than the best previous model?
#* How do you know?

#* 3. Interpret the coefficients in the interaction model, including their sign, magnitude,
#* and significance.

#* 4. Use the `ggeffect` function to estimate expected values, and then create a
#* visualization showing the model fit and the data, using the code from lines
#* 210-220 as a guide.

#* Bonus: Repeat 1-3 but fit log-level model with an interaction


