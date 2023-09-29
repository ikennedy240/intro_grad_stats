


#* we can flip coins, or simulate other events with descrete probabilities
#* using the binomial distribution
#* 
#* The probability of getting heads on a fair coin is .5
#* A simulation of 100 such coin flips is

coin_fips <- rbinom(100,1,.5) # 100 observations of 1 trial with .5 chance of sucess 
mean(coin_fips)

# probably close to, but not exactly 0.5

# plot the convergence of trials to their probability
# try playing around with different n and p
n <- 10000
p <- .5

df <- tibble(
  trial = 1:n,
  result = rbinom(n,1,p)
) %>% mutate(cumulative_success = cumsum(result),
             prob_success = cumulative_success/trial)

df %>%
  ggplot(aes(trial, prob_success))+
  geom_line()+
  geom_hline(yintercept = p, linetype = 2)



#* alternatively, we can get the value for a dice roll with some number
#* of faces

# lets start with a d20
n_faces <- 20
n_rolls <- 100
# define sample space
s <- 1:n_faces
#define our event
# the Rogue wants to pick a lock, its difficult, but she has a lock-picking kit
# so she needs to roll better than 13 to succeed
# the event is rolling greater than a 13
a <- s[s>13]
# the probability to succeed should be just 6/20
p <- length(a)/length(s)

# we can use the sample function to pick values from our sample space
sample(s,1)

# we can also pull lots of samples, and set replace=TRUE to simulate repeated
# rolls
sample(s, 100, replace = TRUE)

n = 1000
df <- tibble(
  trial = 1:n,
  result = sample(s, n, replace = TRUE)
) %>% mutate(sucess = result %in% a,
              cumulative_success = cumsum(sucess),
             prob_success = cumulative_success/trial)

df %>%
  ggplot(aes(trial, prob_success))+
  geom_line()+
  geom_hline(yintercept = p, linetype = 2)

