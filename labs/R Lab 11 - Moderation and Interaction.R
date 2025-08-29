### Moderation and Interactions Examples

#* Consider this simulation an optional 11th lab
#* I'll also use some of this content in our moderations and interactions class


##* Imagine a community of tree frogs. In this community there's competition for 
##* resources which mean that the frogs end up weighing different amounts. However,
##* all else being equal, heavier frogs are able to lay more eggs than lighter frogs
##* (For this example we're only considering frogs who can lay eggs). Of course, other
##* factors than frog weight influence the number of eggs, so there's some noise.
##* We can simulate the relationship between frog weight (in grams) and the number 
##* of eggs they lay (the average is 4,500!!).

library(tidyverse)
library(huxtable)
library(ggeffects)

### PART I

# This time we'll make a data frame
# we have 100 frogs
n = 1000
frog_df <- tibble(
  # average weight is 22.7 with sd of 3
  weight = rnorm(n,22.7,3)
)

# look at the distribution of weights
ggplot(frog_df, aes(weight))+
  geom_histogram()

# we can use mutate to simulate the number of eggs
frog_df <- frog_df %>%
  mutate(eggs = 2230 + 100*weight + rnorm(n,0,400))

# as expected, we see a clear linear relationship
ggplot(frog_df, aes(weight, eggs))+
  geom_point()

# we can fit a regression and then summarize and visualize the results
weight1 <- lm(eggs ~ weight, data = frog_df)
summary(weight1)

pred1 <- ggpredict(weight1, terms = 'weight')
plot(pred1)+
  # adjusting the y-axis to start at zero and go one sd above the max observed value
  ylim(0, max(frog_df$eggs+sd(frog_df$eggs)))+
  xlim(-5,40)+
  geom_hline(yintercept = 0, color = 'red')+
  geom_vline(xintercept = 0, color = 'red')

### PART II

##* Let's redo this, but complicate the relationship. For one, frogs, being amphibians,
##* need access to water. Maybe frogs who lay their eggs closer to water will lay more
##* eggs, since the conditions are better. Also, it's common for relationships of distance
##* to be logarithmic -- being one meter from water is a lot better than being two meters
##* from water, but being 10m from water isn't much better than being 11m from water.

frog_df <- frog_df %>%
  mutate(
    # distance from water in meters -- heavier frogs get to be closer
    water_dist = (1.01-weight/max(weight))*rgamma(n,1),
    # new formulat for eggs, including the distance from water
    eggs = 2230 + 100*weight - 150*log(water_dist) + rnorm(n,0,400))

# now we have two variables
frog_df %>% pivot_longer(-eggs) %>%
ggplot(aes(value, eggs, color = name))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~name, scales = 'free')

# bivariate eggs and weight
weight2 <- lm(eggs ~ weight, data = frog_df)

# bivariate eggs and dist - level-level
level_dist2 <- lm(eggs ~ water_dist, data = frog_df)

# bivariate eggs and dist - level-log
log_dist2 <- lm(eggs ~ log(water_dist), data = frog_df)

# multiple regression with weight and log dist
weight_dist2 <- lm(eggs ~ weight + log(water_dist), data = frog_df)

huxreg(weight2, level_dist2, log_dist2, weight_dist2)

pred2 <- ggpredict(weight_dist2, terms = c('weight', 'water_dist [0.1,0.5,1,2]'))
plot(pred2) + ylim(0, max(frog_df$eggs+sd(frog_df$eggs)))

# Questions:
#* 1. Compare the coefficient for weight in model 1 (weight2) and in model4 (weight_dist2).
#* What accounts for that difference?
#* 2. Why does model 3 (log_dist2) fit better than model 2 (level_dist2)?
#* 3. Looking at the predicted values plot, describe and explain the difference
#* between the lines in the plot.

### PART 3 - Genetic mutation 1

##* Ok, so far, so good. I guess. Imagine that some frogs have a random mutation
##* which increases the number of eggs that they lay by 500.

frog_df <- frog_df %>%
  mutate(
    mutation1 = if_else(rnorm(n)>0,1,0),
    # new formulat for eggs, including mutation 1
    eggs = 2230 + 100*weight - 150*log(water_dist) + 500*mutation1 + rnorm(n,0,400))

frog_df %>% pivot_longer(-eggs) %>%
  ggplot(aes(value, eggs, color = name))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~name, scales = 'free')

# bivariate eggs and weight
weight3 <- lm(eggs ~ weight, data = frog_df)

# multiple regression with weight and log dist
weight_dist3 <- lm(eggs ~ weight + log(water_dist), data = frog_df)

# multiple regression with weight and log dist
full3 <- lm(eggs ~ weight + log(water_dist) + mutation1, data = frog_df)


huxreg(weight3, weight_dist3, full3)

pred3 <- ggpredict(full3, terms = c('weight', 'mutation1'))
plot(pred3) + ylim(0, max(frog_df$eggs+sd(frog_df$eggs)))

# Questions:
#* 1. Compare the coefficients for weight and water_dist in model 3 (weight_dist3) and in model3 (full3).
#* What accounts for that difference?
#* 2. Why does model 3 (full3) fit better than model 2 (weight_dist3)?
#* 3. Looking at the predicted values plot, describe and explain the difference
#* between the lines in the plot.

### Part 4 - Mutation 2

#* In the final part of our simulation, imagine a second mutation. It's just as random
#* as the first one -- ie, it's not associated with weight -- but it works differently.
#* Instead of just increasing the number of eggs laid, this mutation works better 
#* on heavier frogs. When a frog is heavy, mutation 2 produces 
#* a lot of extra eggs. But when a frog is not that heavy, mutation 2 produces only 
#* a few extra eggs. This is an example of an interaction, or moderation. The 
#* weight _moderates_ teh effect of the mutation, so that the effect of the mutation is
#* different at different weights. Let's check it out.


frog_df <- frog_df %>%
  mutate(
    mutation2 = if_else(rnorm(n)>0,1,0),
    # new formulat for eggs, including mutation 1
    eggs = 2000 + 100*weight - 150*log(water_dist) + 500*mutation1 + 80*mutation2*weight + rnorm(n,0,400))

frog_df %>% pivot_longer(-eggs) %>%
  ggplot(aes(value, eggs, color = name))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~name, scales = 'free')

# bivariate eggs and weight
weight4 <- lm(eggs ~ weight, data = frog_df)

# additive effect of mutation 2
addative4 <- lm(eggs ~ weight + log(water_dist) + mutation1 + mutation2, data = frog_df)

# moderation and additive effect of mutation 2
interaction4 <- lm(eggs ~ weight + log(water_dist) + mutation1 + mutation2 + I(mutation2*weight), data = frog_df)


huxreg(weight4, addative4, interaction4)

pred4 <- ggpredict(interaction4, terms = c('weight', 'mutation2'))
plot(pred4) + ylim(0, max(frog_df$eggs+sd(frog_df$eggs)))


# Questions:
#* 1. Compare the coefficients for weight and water_dist in models 1 (weight4),  2 (addative4) and in model3 (interaction4).
#* What accounts for that difference?
#* 2. Why does model 3 (interaction4) fit better than model 2 (addative4)?
#* 3. Looking at the predicted values plot, describe and explain the difference
#* between the lines in the plot.
#* 4. Can you think of an example of a real-life interaction?

