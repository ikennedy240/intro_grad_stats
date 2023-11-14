###
n <- 100
x <- rnorm(n)
z <- rgamma(n,1)
y <- exp(x) + log(z) + rnorm(n)
y <- y+abs(min(y))+1
plot(x,y)
plot(z,y)

lm1 <- lm(y ~ x + z)
summary(lm1)
plot(lm1)

lm2 <- lm(log(y) ~ x + log(z))
summary(lm2)
plot(lm2)



cps_clean <- CPS_2016_2021_new %>% select(AGE, INCTOT, NCHILD) %>%
  filter(INCTOT< 1e8, AGE<=81) %>% drop_na()

cps_clean %>% sample_n(1e4) %>% ggplot(aes(AGE, INCTOT))+geom_point()


lm1 <- lm(INCTOT ~ AGE, data = cps_clean)
summary(lm1)


lm2 <- lm(INCTOT ~ log(AGE), data = cps_clean)
summary(lm2)

lmsq <- lm(INCTOT ~ poly(AGE, 2), data = cps_clean)
summary(lmsq)

lm3 <- lm(log(INCTOT) ~ (AGE), data = cps_clean %>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1))
summary(lm3)

lm4 <- lm(log(INCTOT) ~ log(AGE), data = cps_clean %>% mutate(INCTOT = INCTOT+abs(min(INCTOT))+1))
summary(lm4)

library(huxtable)
huxreg(lm1,lm2,lm3, lmsq,lm4)

cps_samp <- cps_clean %>% sample_n(2e4)

pred_sq <- ggpredict(lmsq, terms = 'AGE')
plot(pred_sq)+geom_point(aes(AGE, INCTOT), data = cps_samp, alpha = .05)

pred_2 <- ggpredict(lm2, terms = 'AGE')
plot(pred_2)+geom_point(aes(AGE, INCTOT), data = cps_samp, alpha = .05)

pred_4 <- ggpredict(lm4, terms = 'AGE')
plot(pred_4)+geom_point(aes(AGE, INCTOT), data = cps_samp, alpha = .05)

## dummy and categorical variables

lm1 <- lm(INCTOT ~ NCHILD, data = cps_clean)
summary(lm1)

cps_clean <- cps_clean %>% mutate(any_child = NCHILD>0, child_cat = as.character(NCHILD))

lm2 <- lm(INCTOT ~ any_child, data = cps_clean)
summary(lm2)


lm3 <- lm(INCTOT ~ child_cat, data = cps_clean)
summary(lm3)

huxreg(lm1, lm2, lm3)

plot(ggeffect(lm1, terms = 'NCHILD'))+ylim(0,1e5)
plot(ggeffect(lm12, terms = 'NCHILD'))+ylim(0,1e5)