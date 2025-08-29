

norm <- tibble(value = seq(-4,4,length.out = 100)) %>%
  mutate(pdf = dnorm(value), mean = mean(value)) %>%
  ggplot(aes(value, pdf))+
  geom_line()+
  geom_vline(aes(xintercept = mean), linetype = 2)+
  theme_void()

gamm <- tibble(value = seq(0,8,length.out = 100)) %>%
  mutate(pdf = dgamma(value, shape = 1.5), mean = mean(value)) %>%
  ggplot(aes(value, pdf,xintercept = mean))+
  geom_line()+
  geom_vline(aes(xintercept = mean), linetype = 2)+
  theme_void()

unif <- tibble(value = seq(0,8,length.out = 100)) %>%
  mutate(pdf = dunif(value,min = 1, max = 7), mean = mean(value)) %>%
  ggplot(aes(value, pdf))+
  geom_line()+
  geom_vline(aes(xintercept = mean), linetype = 2)+
  theme_void()


cowplot::plot_grid(norm, gamm, unif, nrow = 1)


norm <- tibble(value = seq(-3,3,length.out = 100)) %>%
  mutate(pdf = dnorm(value), mean = mean(value)) %>%
  ggplot(aes(value, pdf))+
  geom_line()+
  geom_vline(aes(xintercept = mean), linetype = 2)+
  theme_void()

gamm <- tibble(value = seq(0,16,length.out = 100)) %>%
  mutate(pdf = dgamma(value, shape = 3), mean = mean(rgamma(1e3, shape = 3))) %>%
  ggplot(aes(value, pdf,xintercept = mean))+
  geom_line()+
  geom_vline(aes(xintercept = mean), linetype = 2)+
  theme_void()

beta <- tibble(value = seq(0,1,length.out = 100)) %>%
  mutate(pdf = dbeta(value, 3,2), mean = mean(rbeta(1e3, .8,.8))) %>%
  ggplot(aes(value, pdf))+
  geom_line()+
  geom_vline(aes(xintercept = mean), linetype = 2)+
  theme_void()


cowplot::plot_grid(norm, gamm, beta, nrow = 1)

pbsq <- read_csv('../cool_stuff/data/pbsq_links_ct.csv')

pbsq %>% distinct(area_name,category,sub_category, name) %>%
  count(area_name)

pbsq %>% distinct(area_name,category,sub_category, name) %>%
  filter(str_detect(area_name, '^7')) %>%
  count(area_name, category, sub_category) %>%
  pivot_wider(names_from = area_name, names_prefix = 'zip_', values_from = 'n') %>%
  mutate(diff = abs(zip_79930 - zip_77061)) %>%
  arrange(desc(diff)) %>% head(20)
