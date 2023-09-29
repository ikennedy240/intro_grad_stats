library(tidyverse)
library(glue)
# normal data

# take a sample of only 30
n = 30
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

# try  a sample of 300
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
n = 1000
df <- tibble(sd_value = '1', x = rnorm(n, mean = 0,sd = 1)) %>%
  # actaully making 3 distributions, makred by their sds
  bind_rows(tibble(sd_value = '2', x = rnorm(n, mean = 0,sd = 2))) %>%
  bind_rows(tibble(sd_value = '4', x = rnorm(n, mean = 0,sd = 4)))


# histogram when the sd = 1
df %>% filter(sd_value == '1') %>%
  ggplot(aes(x))+
  geom_histogram()+
  geom_vline(xintercept = mean_value, color = 'green', size = 3, alpha = .5)+
  geom_vline(xintercept = median_value, color = 'blue', size = 3, alpha = .5)+
  ggtitle(glue("{n} Random Numbers, Normally Distributed (Mean = {round(mean_value,3)}, SD = {round(stdev,3)})"))+
  xlab("Value")+
  ylab("Frequency")


# similar density plot to what I showed in class
df %>% filter(sd_value == '1') %>% 
  ggplot(aes(x, group = sd_value, color = sd_value, fill = sd_value))+
  geom_density(alpha = .2)+
  xlab("Value")+
  ylab("Frequency")

# now let's plot all three 
# color = sd_value, fill = sd value is what let's us collor each one
df %>% ggplot(aes(x, group = sd_value, color = sd_value, fill = sd_value))+
  geom_density(alpha = .2)+ # setting alpha = .2 makes all of the curves seethrough
  xlab("Value")+
  ylab("Frequency")


#  look what I'm doing to x in the mutate call
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
  facet_wrap(~sd_value, nrow = 3)

## more advanced simulation on teacher vs lawyer salaries
n <- 100

experience <- runif(n, 0, 40)
skill <- rnorm(n, mean = 3, sd = 1)
connections <- rpois(n,1)
school <- exp(rnorm(n, mean = 2))

inc_data <- tibble(experience, skill, connections, school) %>%
  mutate(Teachers = 5 + 1.1^experience + 5*skill + connections + 0.01*school+ exp(rnorm(n, mean =.5)),
         Lawyers = 0 + 1.2^experience + 5*skill + 20*connections + school + exp(rnorm(n, mean =1)))

inc_data %>%
  pivot_longer(c(Teachers, Lawyers), names_to = 'Profession') %>%
  ggplot(aes(value, color = Profession))+
  geom_density()+
  scale_x_continuous(labels = scales::dollar_format(prefix="$", suffix = "K"))+
  facet_wrap(~Profession, scales = 'free', nrow = 2)+
  ggtitle("Simulated Teacher and Lawyer Salaries")+
  ylab('Density')+
  xlab('Salary')+
  theme_classic()

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
ggsave(glue('plots/teachers_lines.png'), width = 5, height = 4)

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

inc_data %>% summary()

