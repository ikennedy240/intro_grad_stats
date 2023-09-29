# load the tidyverse
library(tidyverse)

# load movie ratings
# download from here: https://grouplens.org/datasets/movielens/25m/
# Unzip into your data file

ratings <- read_csv('data/ml-25m/ratings.csv')

movies <- read_csv('data/ml-25m/movies.csv')

## This bit is to test find a movie title or titles
movie_title <- 'Sleepless'
# search for movies that match the titel (case sensitive)
movies %>% filter(str_detect(title, movie_title))

# now you can redo that to get some subset of movies and plot their ratings over time
movies %>% 
  # filter the movies to the ones you want
  filter(str_detect(title, "^Matrix,? [TR]")) %>% 
  # join them to the ratings (by 'movieId')
  left_join(ratings) %>% 
  # make some changes to the data
  mutate(rating = rating*2,
         date = as_datetime(timestamp),
         year = floor_date(date, unit = 'year'))%>% 
  # this is a cool bit: gorup by title and year, then summarize
  group_by(title, year) %>% summarize(month_mean = mean(rating), 
                                      high = t.test(rating)$conf.int[[1]], 
                                      low = t.test(rating)$conf.int[[2]]) %>%
  # and then plot
  ggplot(aes(year, month_mean, ymin = low, ymax =high, color = title,fill = title))+
  geom_line(alpha = .7)+
  geom_ribbon(alpha = .2, linewidth = 0)+
  theme_classic()+
  ggtitle(paste("Ratings of",movie_title,"over time"))+
  xlab("Time")+
  ylab("Monthly Average Rating")
