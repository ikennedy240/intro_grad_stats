
library(tidyverse)

# a functiont hat makes fake data with a particular mean and sd
fake_data <- function(n, t_mean, t_sd){
  x<-rnorm(n)
  x<-t_sd*(x-mean(x))/sd(x)+t_mean
  x
}


sd = 1
df <- tibble(id = 1:200,
       wage = c(fake_data(70, 19.25, sd), 
                fake_data(70, 21.50, sd), 
                fake_data(60, 20, sd)),
       race = c(rep('black', 70),
                rep('white', 70),
                rep('asian', 60))) %>% group_by(race) %>% mutate(mean_wage = mean(wage))

df <- tibble(id = 1:200,
             wage = c(fake_data(70, 19.9, sd), 
                      fake_data(70, 20.15, sd), 
                      fake_data(60, 20, sd)),
             race = c(rep('black', 70),
                      rep('white', 70),
                      rep('asian', 60))) %>% group_by(race) %>% mutate(mean_wage = mean(wage))


df %>% ggplot(aes(wage, color = race, fill = race))+
  geom_density(alpha = .05)+
  geom_vline(aes(xintercept = mean_wage, color = race), linetype = 2)+
  theme_classic()+
  scale_x_continuous(labels = scales::dollar_format())

df %>% ggplot(aes(race, wage, color = race))+
  geom_jitter()+
  geom_hline(aes(yintercept = mean_wage, color = race), linetype = 2)+
  theme_classic()+
  scale_y_continuous(labels = scales::dollar_format(), limits = c(19,22))


oneway.test(wage ~ race,
            data = df,
            var.equal = TRUE # assuming equal variances
)
