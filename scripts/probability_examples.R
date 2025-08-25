
# load libraries
library(tidyverse)
library(glue)

#* we can flip coins, or simulate other events with discrete probabilities
#* using the binomial distribution
#* 
#* The probability of getting heads on a fair coin is .5
#* A simulation of 100 such coin flips is

coin_fips <- rbinom(100,1,.5) # 100 observations of 1 trial with .5 chance of success 
mean(coin_fips)

# probably close to, but not exactly 0.5

# plot the convergence of trials to their probability
# try playing around with different n and p
n <- 10000
p <- .5

# setting up a dataframe version
df <- tibble(
  # an index of the trial number, 1 to 10K
  trial = 1:n,
  # the result of that particular trial
  # this will automatically change as you change n and p above
  result = rbinom(n,1,p)
) %>% 
  # `cumsum` just sums up all of the rows until a particular row
  mutate(cumulative_success = cumsum(result),
         # and the probability of success up to that point is just the 
         # sub divided by the trials so far
             prob_success = cumulative_success/trial)

# now we can make a plot showing that even though the probability of success starts
# out far away from our given 'p' (0.5 to start, but change it around), by the 10Kth 
# trial it's going to be pretty close
df %>%
  # this is why we made the 'trial' column, so that it could be our x variable in the plot
  ggplot(aes(trial, prob_success))+
  # a `geom_line` just draws lines between all of our points
  geom_line()+
  # the `geom_hline` makes a horizontal line, in this case a 'p', our underlying probability
  geom_hline(yintercept = p, linetype = 2)

##!! Run lines 20-43 a few times and notice how the plot changes each time
## but!! the cumulative probability will always end up close to the probability we set as 'p'


#* alternatively, we can get the value for a dice roll with some number
#* of faces

# lets start with a d20
n_faces <- 20
n_rolls <- 100

# define sample space
s <- 1:n_faces # if you look at the object s, it's just an integer vector with all values from 1 to 20

#define our event
# we're playing dnd
# the Rogue wants to pick a lock, its difficult, but she has a lock-picking kit
# so she needs to roll 13 or better than 13 to succeed
# the event is rolling greater than or equal to a 13
difficulty_class = 13
# a is the set of all successful rolls
a <- s[s>=difficulty_class]
# the probability to succeed should be just 8/20 
# since there are 8 ways to roll 13 or higher with a 20 sided die
p <- length(a)/length(s)

### cool trick ###
# let's use glue to print out some info
print(glue("Simulating {n_rolls} trials of a {n_faces} sided die with dc of {difficulty_class} and a {p} probability of success"))
# inside of `glue` we can put variables from our global environment within curly brackets {}
# and glue will transform them to character vectors and create a string for us
# super useful for logging and stuff and to be cool \(-^-)/

# we can use the sample function to pick values from our sample space
# this is rolling the dice once
sample(s,1)

# we can also pull lots of samples, and set replace=TRUE to simulate repeated
# rolls (if you forget replace  = TRUE, then your die is loosing sides every time you roll)
# if you were, say picking marbles from a bag without putting them back, though
# you would set replace = FALSE 
# this is rolling the dice 100 times
sample(s, n_rolls, replace = TRUE)

# let's make it a tibble (a tidverse word for dataframe)

df <- tibble(
  # column for trial number
  trial = 1:n,
  # column for results
  result = sample(s, n, replace = TRUE)
) %>%
  # now we'll mutate to see if it's a success
  # since we defined `a`, above, as the set of successful rolls
  # we can mark any result in `a` as a success
  mutate(sucess = result %in% a,
         # these are the same as with the coin
              cumulative_success = cumsum(sucess),
             prob_success = cumulative_success/trial)

# plot is also the same as above
df %>%
  ggplot(aes(trial, prob_success))+
  geom_line()+
  geom_hline(yintercept = p, linetype = 2)


### check-out
# Now the rogue is trying to disarm a trap. She's good at that, so the dc is much lower
# Set the dc to 8, instead of 13 and re-run the simulation

