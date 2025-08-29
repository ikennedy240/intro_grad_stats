# R LAB 5 - The standard normal and sampling distributions
# SOC 502, Kennedy

### LAB 5 MILESTONES ###
# *THESE ARE THINGS YOU SHOULD BE ABLE TO DO BY THE END OF THE LAB*

##* Plot a normal distribution given its moments
##* Calculate and plot a given area under a normal distribution
##* Calculate the probability of observing a particular value, given a distribution
##* Draw samples and plot them as part of the whole distribution
##* Use repeated sampling to draw a plot of the sampling distribution


# ****************
# INTRODUCTION
# In this lab we will:
#   1) Plot the standard normal distribution
#   2) Plot critical values of normal distributions, shade the area under the curve
#   3) Standardize data to the standard normal
#   4) Explore how well samples approximate the population mean
#   5) Plot the sampling distribution and the population distribution

#* As we're moving towards doing statistical inference, we want to gain facility
#* moving between our data -- which is normally a sample -- and other related distributions.
#* The distribution of our sample is the sample distribution.
#* One related distribution is the standard normal distribution - with mean 0 and sd 1.
#* Another is the sampling distribution - which is imaginary
#* And finally, the population distribution, which is (usually) unobserved

## Part 1: Plotting the Standard Normal (and variations)

#* 1.1 We'll start by defining the moments of our distribution: mu, the mean and
#* sigma, the standard deviation
mu <- 0
sigma <- 1

#* 1.2 then we'll prepare the x values which we'll use to pull from the normal distribution
#* this code uses the `seq` function which makes a sequence of numbers
#* staring at start, going to end, and having length length.out
#* Instead of length, you can specify 'by' to set a step size
#* For us, we want to start 5 sd below the mean and go to 5sd above
#* And we want a lot of observations to make a smooth curve, so we've asked for
#* 10k (1e4)

value = seq(from = mu - 5*sigma, to = mu + 5*sigma, length.out = 1e4)

# 1.3 note that a histogram shows a uniform distribution going from around
# -5 to +5
hist(value)

#* 1.4 These values will be the x-axis of our normal probability density function plot
#* For the y-axis, we need the density of the pdf at those values
#* We use the dnorm function d for density and norm for normal distribution
pdf = dnorm(value, mu, sigma)

#* 1.5 We want those as a dataframe, so I'll joing them in a tibble:
plot_df <- tibble(value = value, pdf = pdf)

#* 1.6 now we'll plot (you might get an error running this, check the hints to fix)
plot_df %>% ggplot(aes(value, pdf))+
  geom_line()+
  theme_classic()

# that's the standard normal!

##* 1.7 Write code below to plot a normal distribution with mean = 8 and sd = 2


## Part 2: Plotting Critical Values
#* we'll start by considering a question from class: What proportion of men
#* fall between Ian's dad and Ian's brother in height
#* We'll start by just plotting those below Ian's brother
#* and then build on that to plot the area between them

# 2.1 define the distribution

# mean - this should be the population mean for men's heights
mu <- 70
# sd - this should be the population standard deviation
sigma <- 3


# 2.2 Define our critical values and find their lower-tail probabilities
dad <- 67 # Ian's dad's height
brother <- 69 # Ian's brother's height
pdad <- pnorm(dad, mu, sigma) # this is the probability of being at or below Dad's height
pbro <- pnorm(brother, mu, sigma) # and at or below brother's height
# subtract to find the area under the curve between them
between <- pbro-pdad
# we can already output a clean answer to the question (using glue!):
# note using functions inside the {} is AOK!
print(glue("The proportion of men between Ian's dad and Ian's brother's height is {round(between, 2)}"))

# 2.3 Plot just the area under Ian's brother's height
plot_df <- tibble(
  # calculating for values from 5 sd below the mean to 5 sd above, with 1000 values
  # same as in part 1 above, but using our population mean and sd
  value = seq(mu - 5*sigma, mu + 5*sigma, length.out = 1e4)
) %>% mutate(pdf = dnorm(value, mu, sigma), # this is just getting the  probability density function for each value
             fill = if_else((value <= brother), "Shorter than Ian's brother","Taller than Ian's brother")) # using text here will make the legend automatically

# now that we've prepared our data, we plot
plot_df %>% ggplot(aes(value, pdf))+
  # this plots the pdf
  geom_line()+
  # this puts a line at each of our critical values
  geom_segment(x = brother, y = 0, xend = brother, yend = dnorm(brother, mu, sigma))+
  geom_ribbon(aes(ymax=pdf, fill = fill),ymin=0, alpha = .7)+
  theme_classic()

# 2.4 Make a plot showing the area between the heights
plot_df <- tibble(
  # calculating for values from 5 sd below the mean to 5 sd above, with 1000 values
  # same as in part 1 above, but using our population mean and sd
  value = seq(mu - 5*sigma, mu + 5*sigma, length.out = 1e4)
) %>% mutate(pdf = dnorm(value, mu, sigma), # this is just getting the  probability density function for each value
             fill = if_else((value <= brother) & (value>=dad), 'Between Heights','Outside the Heights')) 

# now that we've prepared our data, we plot
plot_df %>% ggplot(aes(value, pdf))+
  # this plots the pdf
  geom_line()+
  # this puts a line at each of our critical values
  geom_segment(x = dad, y = 0, xend = dad, yend = dnorm(dad, mu, sigma))+
  geom_segment(x = brother, y = 0, xend = brother, yend = dnorm(brother, mu, sigma))+
  # this fills the area based on the tibble
  geom_ribbon(aes(ymax=pdf, fill = fill), ymin=0, alpha = .5)+
  # and this specifies the colors
  scale_fill_manual(values = c('Between Heights' = 'lavender', 'Outside the Heights' ='white'))+
  theme_classic()


# 2.5 Make another plot
# Calculate and plot the middle 50% of the US women's height distribution, which 
# has a mean of 64.5 and a sd of 2.5


## Part 3: Standardize data 
#* Download 'tract21_inc_rent.csv' from blackboard and move it to your data
#* directory. This dataset includes the median gross rent ("rent") and the median 
#* household income ("hh_inc") estimates for each census tract in the US from
#* the 2017-2021 ACS 5-year estimates.
#* We're going to use this data to compare the normal distribution to the 
#* observed data.

##* 3.1 Load the data and call it acs_df (look at hints for help)


#* 3.2 I'm going to focus on the rent data and plot the distribution of median rents

acs_df  %>%
  ggplot(aes(rent))+
  geom_density(color = 'red', fill = 'red', alpha = .7)+
  theme_classic()

#* 3.3 We can see that this plot doesn't look particularly normal, but we can still
#* plot a normal distribution with the same mean and sd

# using na.rm = TRUE here because some tracts are missing rent medians
rent_mu <- mean(acs_df$rent, na.rm = TRUE)
rent_sigma <- sd(acs_df$rent, na.rm = TRUE)

# this makes a normal distribution with 10K rows
rent_norm <- tibble(
  values = seq(rent_mu - 5*rent_sd, rent_mu + 5*rent_sigma, length.out = 10000)
) %>% mutate(pdf = dnorm(values, rent_mu, rent_sigma))

# 3.4 looking at the plot, which do you think is higher, the median or the mean, why?

# 3.5 now I'll remake the plot with the normal curve included. What do you notice?

acs_df  %>%
  ggplot(aes(rent))+
  geom_density(color = 'red', fill = 'red', alpha = .7)+
  geom_line(data = rent_norm, aes(x = values, y = pdf))+
  theme_classic()

#* 3.6 What's the z-score for a neighborhood with a median rent of $2000?
#* Put another way, how many standard deviations above the mean is a neighborhood
#* with a median rent of 2k?
#* Remember, z = x-\mu/\sigma
z = (2000 - rent_mu)/rent_sigma
z
# This suggests that a neighborhood with 2K is 1.37 standard deviations above the mean

#* 3.7Based on the normal distribution, what's the probability of observing an neighborhood with
#* a median rent of $2k or more?
#* 
pnorm(2000,rent_mu, rent_sigma, lower.tail = FALSE)

# 3.8 Based on the normal distribution, around 8.6 percent of neighborhoods should have
# rent >= 2000

# 3.9 But what proportion of neighborhoods actually have such a high median? We have
# all the neighborhoods, so we can just count.
acs_df %>% filter(!is.na(rent)) %>% 
  count(rent>=2000) %>% mutate(prop = n/sum(n))

# 3.10 the actual data show that 9.5% of neighborhoods actually have a median 
# rent higher than $2k. Look at the plot and explain this discrepancy

##* 3.11 Based on the normal distribution, calculate the probability of observing an neighborhood with
##* a median rent of $3k or more. Then calculate the observed proportion of neighborhoods
##* with such a high rent. Explain the difference.

##* 3.12 What do these experiments suggest about the utility of the normal distribution 
##* for approximating real-life distributions that are skewed? What could you do 
##* to improve this comparison? Try plotting the log of the rent distribution.

## Part 4: Explore how well samples approximate the population mean

#* Ok, so, it seems like there are some issues with just assuming that data
#* is normally distributed. So, we shouldn't do that. Repeat: we cannot just
#* assume that our data is normal and act accordingly. Instead, we have this cool
#* trick in the Central Limit Theorem, which relies on the sampling distribution. 
#* We'll look at that distribution in Part 5, but first, let's get a sense
#* for how well random samples predict population parameters

#* 4.1 Let's take some random samples from our tract median rents. We'll start by 
#* sampling 10 tracts at a time, so n=10

## remember tract mean = $1237
n = 10
rent_sample <- acs_df %>% filter(!is.na(rent)) %>%
  sample_n(n)

# caluclate our sample mean
smean <- mean(rent_sample$rent)
smean # this was off by about 30 when I tried it

# we can plot our sample and its mean
ggplot(rent_sample)+
  geom_vline(aes(xintercept = rent))+
  geom_vline(aes(color = 'Sample Mean',xintercept = smean), size = 2)+
  geom_vline(aes(color = 'Population Mean',xintercept = rent_mu), size = 2)


# and use the same code to plot our sample onto the whole distribution:
acs_df  %>%
  ggplot(aes(rent))+
  geom_density(color = 'purple', alpha = .7)+
  geom_vline(data =rent_sample, aes(xintercept = rent, color = 'Sampled Value'))+
  geom_vline(aes(color = 'Sample Mean', xintercept = smean), size = 2)+
  geom_vline(aes(color = 'Population Mean', xintercept = rent_mu), size = 2)+
  labs(x = "Rent",
       y = "Density",
       color = "Mean Type")+
  theme_classic()+
  theme(legend.position = 'bottom')

##* 4.2 Run the code for 4.1 a few times and notice how the samples change
##* the values we get for the sample mean 


## Part 5: Plot the sampling distribution and the population distribution

#* In part 4 we took single samples and saw that the sample mean was usually 
#* similar to but not exactly the same as the population mean. Moreover, we 
#* probably noticed that sometimes the error--the distance between the sample
#* mean and the population mean--was often quite small, but was sometimes quite 
#* large. That error follows Gauss's insight about the location of Ceres: there
#* will (almost) always be some error, but small errors are more likely than large
#* errors and any particular error K has the same probably as -K. If that's true,
#* then the errors should form a normal distribution around 0, and the sample 
#* means should form a normal distribution around \mu, the population mean.
#* Let's see if that happens.

#* 5.1 A challenge here is that we want to take a whole bunch of samples,
#* like maybe thousands of samples. So we want to tell R to run the same code 
#* lots of times. One way to do that is with a `for` loop. Here's and example

for(i in 1:10){
  print(paste("i equals", i))
  reallys <- paste(rep('really',i), collapse = ' ')
  print(paste("Ian is",reallys,"great"))
}

# look, you can't argue with a computer.

# let's take that code apart line by line
#* the for part makes a variable called i, and does that once for each item
#* in 1:10. 1:10 = c(1,2,3,4,5,6,7,8,9,10), so ten times. The value of i, then,
#* increases with each pass through the loop
for(i in 1:10){
  #* this line prints out our current value of i 
  #* the print function prints its contents
  #* the paste function pastes values together, so it connects the variable i
  #* to the phrase 'i equals', and you can see that i gets larger each time
  print(paste("i equals", i))
  # this uses paste to paste together i repetitions of the word 'really', collapsing
  # them with ' '. This line isn't printed
  reallys <- paste(rep('really',i), collapse = ' ')
  # this line prints again, varying the number of reallys in this phrase
  # as i increases, so do the number of reallys, and, arguably, so does the inaccuracy
  # of the statement
  print(paste("Ian is",reallys,"great"))
}


# 5.2 Now that we have a loop under (around?) our belt, we can apply it to our problem

# First we need to define our sample size, n, and our number of samples, samples
n = 10
samples = 10

# Now I'll build a for loop that graps a sample of 10 and calculates the mean of that sample
# for each of our samples.
for(i in 1:samples){
  # this code is from 4.1
  rent_sample <- acs_df %>% filter(!is.na(rent)) %>%
    sample_n(n)
  # caluclate our sample mean
  smean <- mean(rent_sample$rent)
  print(smean)
}

#* 5.3 But we don't really want to print the mean every time, we'd actually like
#* to keep it around. We'd really like a vector of sample means. So we'll first
#* define an empty vector and then fill it up as the loop goes. 

smean_vector <- c()
for(i in 1:samples){
  # this code is from 4.1
  rent_sample <- acs_df %>% filter(!is.na(rent)) %>%
    sample_n(n)
  # caluclate our sample mean
  smean <- mean(rent_sample$rent)
  smean_vector[i] <- smean
}
smean_vector

#* Note: this is technically ineffienct, and maybe you'll learn a better way
#* later (I'm looking at you, map), but for now this will serve our purposes. Also
#* I often run simulations in loops even if it's slower for two reasons: 1. they're easy 
#* to test, and 2. you can interrupt them in the middle and check how they're goign

# 5.4 Ok, we've collected 10 sample means. Let's plot them as a histogram, along
# with the population mean and the mean of the sample means

smean_mean <- mean(smean_vector)

tibble(smeans = smean_vector) %>%
  ggplot(aes(smeans))+
  geom_histogram()+
  geom_vline(aes(color = 'Population Mean', xintercept = rent_mu), size = 1)+
  geom_vline(aes(color = 'Mean of Smaple Means', xintercept = smean_mean), size = 1, linetype = 2)+
  theme_classic()

#* 5.5 That plot may not be very convincing that the sampling distribution is really
#* distributed around the population mean, but that's just because we only took 10 samples
#* Let's run it again with 500 samples:
n = 10
samples = 500
smean_vector <- c()
for(i in 1:samples){
  # this code is from 4.1
  rent_sample <- acs_df %>% filter(!is.na(rent)) %>%
    sample_n(n)
  # caluclate our sample mean
  smean <- mean(rent_sample$rent)
  smean_vector[i] <- smean
}

smean_mean <- mean(smean_vector)

tibble(smeans = smean_vector) %>%
  ggplot(aes(smeans))+
  geom_histogram()+
  geom_vline(aes(color = 'Population Mean', xintercept = rent_mu), size = 1)+
  geom_vline(aes(color = 'Mean of Smaple Means', xintercept = smean_mean), size = 1, linetype = 2)+
  theme_classic()

# whenever I run this, I get a mean of sample means that's very close to the population mean

#* Not only that, but I get a roughly normal-looking histogram. This is an approximation
#* of a histogram of the sampling distribution -- which, recall, is actually an imaginary 
#* probability density distribution of every possible sample. If you try the bonus below, 
#* you cna remake this sampling distribution with 1 million samples


# 5.6 modify the code above to take 1000 samples. What do you notice about the histogram?

# 5.7 modify the code above to take 500 samples of 50 tracts, instead of 10. What do you notice?



#* BONUS It takes a few seconds to run 1000 samples. 
#* Write some code that can take 1e6 (1 million) samples in less than a minute
#* There's an answer in the hint that can do it, can you go faster than that?
#* You can use this format to time execution:
start.time <- Sys.time()
## ...Relevant code... ##
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


## LAB 5 CHECK-OUT

# 1 Write code below to plot a normal distribution with mean = 8 and sd = 2
# You can plot either a density plot or a histogram

# 2 Calculate and plot the middle 95% of an imaginary nonbinary height distribution, which 
# has a mean of 66.5 and a sd of 4.5

# 3 Based on the normal distribution and the data from part 3 above, calculate the probability of observing an neighborhood with
# a median rent of $0 or less. Then calculate the observed proportion of neighborhoods
# with such a high rent. Explain the difference.

# 4 Rewrite the code from 4.1 to draw and plot samples from the income distribution
# rather than with the rent distribution

# 5 Rewrite the code from 5.5 to work with the income data


##### HINTS
# PART 1
library(tidyverse)
# PART 3
acs_df <- read_csv('data/tract21_inc_rent.csv')


### PART 5 BONUS

# this times the for loop
start.time <- Sys.time()
n = 10
samples = 1e4
smean_vector <- c()
for(i in 1:samples){
  # this code is from 4.1
  rent_sample <- acs_df %>% filter(!is.na(rent)) %>%
    sample_n(n)
  # caluclate our sample mean
  smean <- mean(rent_sample$rent)
  smean_vector[i] <- smean
}

smean_mean <- mean(smean_vector)

tibble(smeans = smean_vector) %>%
  ggplot(aes(smeans))+
  geom_histogram()+
  geom_vline(aes(color = 'Population Mean', xintercept = rent_mu), size = 1)+
  geom_vline(aes(color = 'Mean of Smaple Means', xintercept = smean_mean), size = 1, linetype = 2)+
  theme_classic()
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# 1k: Time difference of 5.431497 secs
# 10K: Time difference of 52.78387 secs

# This is a much faster alternative
start.time <- Sys.time()
n = 10
samples = 1e6
X <- acs_df %>% filter(!is.na(rent)) %>% pull(rent)
smean_vector <- map_dbl(1:samples, function(x) mean(sample(X, n)))
smean_mean <- mean(smean_vector)
tibble(smeans = smean_vector) %>%
  ggplot(aes(smeans))+
  geom_histogram()+
  geom_vline(aes(color = 'Population Mean', xintercept = rent_mu), size = 1)+
  geom_vline(aes(color = 'Mean of Smaple Means', xintercept = smean_mean), size = 1, linetype = 2)+
  theme_classic()
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# 10K: Time difference of 0.7247741 secs
# 1M: Time difference of 59.95774 secs

