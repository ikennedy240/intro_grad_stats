library(remotes)

remotes::install_github("apreshill/bakeoff")

library(bakeoff)
library(ggeffects)

clean_challenges <- challenges %>% drop_na(technical, result) %>% 
  mutate(result_bi = result != 'OUT', sb_bi = result == 'STAR BAKER', scal_tech = scale(technical)) %>%
  group_by(series, episode) %>% mutate(max_tech = max(technical), prop_tech = (max(technical) - technical + 1)/max(technical)) %>%
  ungroup() 
 
result_by_tech <- glm(result_bi ~ prop_tech + series + episode, data = clean_challenges, family = binomial())

summary(result_by_tech)

ggeffect(result_by_tech, terms = 'prop_tech') %>%
  plot()


sb_by_tech <- glm(sb_bi ~ prop_tech, data = clean_challenges, family = binomial())
summary(sb_by_tech)
ggeffect(sb_by_tech, terms = 'prop_tech') %>%
  plot()




