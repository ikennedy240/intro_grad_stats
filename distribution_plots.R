# import our libraries
library(tidyverse)
library(glue)

## NORMAL DATA ##

# we're using `n` as a variable to record the number of samples to take
n = 30
# `rnorm` is a function to take samples from a normal distribution
# by default it has a mean of zero and an sd of 1
rnorm(n) # output is a bunch of numbers!

# we can make this easier to work with by making it into a dataframe with the
# `tibble` function
df <- tibble(x = rnorm(n)) # now df is a dataframe with one column, x, and 30 observations

# now that we have our dataframe we can look at some measures of central tendency
# and variability
mean_value = mean(df$x)
median_value = median(df$x)
stdev = sd(df$x)

# with that information, we can plot the data we've collected
# if you wnat to understand how this plot works, it can be worth it to
# run it one line at a time

# we take our data and pipe it to `ggplot`
df %>% ggplot(aes(x))+
  # then we ask ggplot to make a histogram of our data
  geom_histogram()+
  # `geom_vline` adds a vertical line, this one is at our mean value
  geom_vline(xintercept = mean_value, color = 'green', size = 3, alpha = .5)+
  # and another v line at the median, so we can see if they're different
  geom_vline(xintercept = median_value, color = 'blue', size = 3, alpha = .5)+
  # now a title. I'm using the `glue` function to automatically put the right numbers in the title
  ggtitle(glue("{n} Random Numbers, Normally Distributed (Mean = {round(mean_value,3)}, SD = {round(stdev,3)})"))+
  # these lines set my x and y labels
  xlab("Value")+
  ylab("Frequency")

## you should see with only 30 samples, our graph doesn't look very 'bell shaped'
## what do you think will happen if we increase the sample size?


# try  a sample of 300
# we're just changing the n value, everything else is the same as above
n = 300
df <- tibble(x = rnorm(n))
mean_value = mean(df$x)
median_value = median(df$x)
stdev = sd(df$x)

df %>% ggplot(aes(x))+
  geom_histogram()+
  geom_vline(xintercept = mean_value, color = 'green', size = 3, alpha = .5)+
  geom_vline(xintercept = median_value, color = 'blue', size = 3, alpha = .5)+
  ggtitle(glue("{n} Random Numbers, Normally Distributed (Mean = {round(mean_value,3)}, SD = {round(stdev,3)})"))+
  xlab("Value")+
  ylab("Frequency")

# how about 1k?
# now I'm also going to play with the standard deviation
n = 1000 # we'll always have a sample of 1k
# but one sample will have an sd of 1
df <- tibble(sd_value = '1', x = rnorm(n, mean = 0,sd = 1)) %>%
  # the second sample will have an sd of 2
  bind_rows(tibble(sd_value = '2', x = rnorm(n, mean = 0,sd = 2))) %>%
  # and the last sample will have an sd of 4
  bind_rows(tibble(sd_value = '4', x = rnorm(n, mean = 0,sd = 4))) %>%
  # now I'm going to make the mean and sd values part of the dataframe
  # `group_by` lets me do calculations for each group!
  group_by(sd_value) %>%
  # this mutate will make different values, grouping based on `sd_value`
  mutate(sample_mean = mean(x), sample_median = median(x), sample_sd = sd(x)) %>%
  ungroup()

df <- tibble(sd_value = 'a', x = rnorm(n, mean = 0,sd = .5)) %>%
  # the second sample will have an sd of 2
  bind_rows(tibble(sd_value = 'b', x = rnorm(n, mean = 2,sd = .5))) %>%
  # and the last sample will have an sd of 4
  bind_rows(tibble(sd_value = 'c', x = rnorm(n, mean = 0,sd = 1.5))) %>%
  bind_rows(tibble(sd_value = 'd', x = rnorm(n, mean = 2,sd = 1.5))) %>%
  # now I'm going to make the mean and sd values part of the dataframe
  # `group_by` lets me do calculations for each group!
  group_by(sd_value) %>%
  # this mutate will make different values, grouping based on `sd_value`
  mutate(sample_mean = mean(x), sample_median = median(x), sample_sd = sd(x)) %>%
  ungroup()


# we can use `filter` to limit the data to the sd of 1
# try adjusting this to look at data with an sd of 2 or 4
df %>% filter(sd_value == '1') %>%
  ggplot(aes(x))+
  geom_histogram()+
  geom_vline(aes(xintercept = sample_mean), color = 'green', size = 3, alpha = .5)+
  geom_vline(aes(xintercept = sample_median), color = 'blue', size = 3, alpha = .5)+
  ggtitle(glue("{n} Random Numbers, Normally Distributed (Mean = {round(mean_value,3)}, SD = {round(stdev,3)})"))+
  xlab("Value")+
  ylab("Frequency")


# similar density plot to what I showed in class
# I've just replaced geom_histogram with geom_density and removed the vlines
# once agian, play around with changing the sd_value here
df %>% filter(sd_value == '1') %>% 
  ggplot(aes(x, group = sd_value, color = sd_value, fill = sd_value))+
  geom_density(alpha = .2)+ # alpha = .2 makes it see-through
  xlab("Value")+
  ylab("Frequency")

# now let's plot all three 
# color = sd_value, fill = sd_value is what let's us color each one
df %>% ggplot(aes(x, group = sd_value, color = sd_value, fill = sd_value))+
  geom_density(alpha = .2)+ # setting alpha = .2 makes all of the curves seethrough
  xlab("Value")+
  ylab("Frequency")


#  look what I'm doing to x in the mutate call!!! Try to figure it out, then ask your TA
df <- df %>% mutate(x2=if_else(x>0,3*x,x)) %>% # what would this do when x = 2? -2? 0?
  group_by(sd_value) %>%
  mutate(sd_mean = mean(x2), sd_median = median(x2)) %>%
  ungroup()

# let's plot these new ones -- all spread out
df %>% ggplot(aes(x2, group = sd_value, color = sd_value, fill = sd_value))+
  geom_density(alpha = .2)+
  xlab("Value")+
  ylab("Frequency")

# get the summary stats
df %>% group_by(sd_value) %>%
  summarise(sd_sd = sd(x2), sd_mean = mean(x2), sd_median = median(x2)) %>%
  mutate(sd_sd = round(sd_sd,3), sd_mean = round(sd_mean,3), sd_median = round(sd_median,3))

# use facet wrap to put each on its own, and geom_vline to add vertical lines
df %>% 
  ggplot(aes(x2, group = sd_value, color = sd_value, fill = sd_value))+
  geom_density(alpha = .2)+
  geom_vline(aes(xintercept = sd_mean, color = sd_value))+
  geom_vline(aes(xintercept = sd_median, color = sd_value), linetype = 2)+
  xlab("Value")+
  ylab("Frequency")+
  facet_wrap(~sd_value, nrow = 3)+
  ggtitle("Mean is solid line, Median is dotted")



# LAWYERS AND TEACHERS
## The above is a basic simulation, but we can use similar methods to create something more 
## complicated

## we'll use this library below, you may have to install it
library(scales)
 
# setting our sample size to 100
n <- 100

# We imagine that both lawyer and teacher salaries are based on four variables:
experience <- runif(n, 0, 40) # uniform distribution -- if you randomly sample a teacher
                              # or lawyer they have an equal chance of having 0, 1, 2, .. 40 years
                              # of experience. Prob not exactly right, but decent approximation
skill <- rnorm(n, mean = 3, sd = 1) # normal distribution: skill is the sum of lots of other 
                                    # random variables, so it's usually normal
connections <- rpois(n,1)           # connections are a count variable, so we use the poisson distribution

school <- exp(rnorm(n, mean = 2)) # the status of the school only matters a lot at the high end, so we exponentiate
                                  # a normal distribution

# Now we create our simulation
inc_data <- tibble(experience, skill, connections, school) %>% # fist we put our four variables in a data frame
  # then we can use our ideas about teachers and lawyers to create formulas to produce the salary
  mutate(Teachers = 5 + 1.1^experience + 5*skill + connections + 0.01*school+ exp(rnorm(n, mean =.5)), # skill and experience matter a lot, connections and school very little
         Lawyers = 0 + 1.2^experience + 5*skill + 20*connections + school + exp(rnorm(n, mean =1))) # skill and experience still matter, but connections and school matter more
# note that we had random noise (error) at the end of each row to account for 'unobserved' factors. We figure there's more unobserved for lawyers than teachers
# so we make the average higher there. We also exponentiate the noise because salaries are often unbalanced and right skewed.

# now we can plot
inc_data %>%
  # this adjusts the format of our data to make plotting easier
  pivot_longer(c(Teachers, Lawyers), names_to = 'Profession') %>%
  # our basic plotting line
  ggplot(aes(value, color = Profession))+
  # a density plot like we used above to compare distributions (cuz we're doing that agian)
  geom_density()+
  # This is a nice way to automatically label our scale, since it's in thousands of dollars
  scale_x_continuous(labels = dollar_format(prefix="$", suffix = "K"))+
  # we'll put the plots next to each other and set scales='free' so that they'll have different axes scales
  facet_wrap(~Profession, scales = 'free', nrow = 2)+
  # add title and labels
  ggtitle("Simulated Teacher and Lawyer Salaries")+
  ylab('Density')+
  xlab('Salary')+
  # makes a nice simple plot with no grid lines
  theme_classic()

## Interpret the plots: what looks to be the mean and median for teachers and lawyers?

# a version of the plot just for teachers
inc_data %>%
  ggplot(aes(Teachers))+
  geom_density(color = 'Purple')+
  geom_vline(xintercept = median(inc_data$Teachers), color = 'purple', linetype = 2)+
  geom_vline(xintercept = mean(inc_data$Teachers), color = 'purple', linetype = 3)+
  scale_x_continuous(labels = scales::dollar_format(prefix="$", suffix = "K"))+
  ggtitle("Simulated Teacher Salaries")+
  ylab('Density')+
  xlab('Salary')+
  theme_classic()

# the line below will save the last plot as .png image
# you might have to make a 'plots' directory inside your working directory
ggsave(glue('plots/teachers_lines.png'), width = 5, height = 4)

# a version just for lawyers
inc_data %>%
  ggplot(aes(Lawyers))+
  geom_density(color = 'green')+
  scale_x_continuous(labels = scales::dollar_format(prefix="$", suffix = "K"))+
  ggtitle("Simulated Lawyers Salaries")+
  geom_vline(xintercept = median(inc_data$Lawyers), color = 'darkgreen', linetype = 2)+
  geom_vline(xintercept = mean(inc_data$Lawyers), color = 'green', linetype = 2)+
  ylab('Density')+
  xlab('Salary')+
  theme_classic()
ggsave(glue('plots/lawyers_lines.png'), width = 5, height = 4)

