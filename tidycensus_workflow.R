#* READ THIS FIRST :)
#* This script is a worked example for finding census variables and downloading
#* them using the census api and the tidycensus R wrapper on that api.
#* The process works like this:
#* 1) Load the libraries (tidyverse and tidycensus)
#* 2) Decide what census data source you want to use
#*    - There are many choices an overview of available apis is here: 
#*          https://www.census.gov/data/developers/data-sets.html
#*    - An overview of the data available through tidycensus is here: 
#*          https://walker-data.com/tidycensus/reference/load_variables.html
#*    - We'll be looking primarily at American Community Survey 5-year Estimates 
#*        at the Tract level in this example, but you can use a _similar_ workflow
#*        to get data at other levels
#* 3) Decide what variables you want from that data source
#*    - In Lab 9 We got data on income and college education
#*    - Here, we'll look for data on ethnoracial composition and rent
#* 4) Find the variable codes using the load_variables() function
#* 5) Use the get_acs() function to download the data we want from the census
#* 6) Explore our data using some basic plots


# 1) Load the libraries (tidyverse and tidycensus)
library(tidyverse)
library(tidycensus)

# 2) Decide what census data source you want to use
# we're going to use the ACS 5 for this example, but you can use other stuff!

#* 3) Decide what variables you want from that data source
# - Here, we'll look for data on ethnoracial composition and rent

#* 4) Find the variable codes using the load_variables() function

# the load variables function loads a data frame of all of the variables available
# for a particular year and for a particular data source. Here we're asking for 
# the variables from 2021 from the ACS 5-year estimates.

acs_vars <- load_variables(2021, 'acs5')

# glimse what we got
glimpse(acs_vars)

# acs_vars has four columns: name, label, concept, and geography
# we want to find the `name` for the `label`s that match our ideas
# in practice, this means scrolling through the acs_vars to find the right label
# let's start with ethnoracial composition, I'm going to `filter` acs vars for those
# labels that might be useful

# I'm using the function `str_detect` to find rows where the concept starts with
# the phrase 'HISPANIC OR LATINO', the carrot ^ at the beginning tells R to look for
# the phrase at the start of the string
acs_vars %>% filter(str_detect(concept, '^HISPANIC OR LATINO')) %>%
  # then I want to count the available concepts
  count(concept) %>% arrange(desc(n))

# based on this, it seems like I probably want variables from the concept 
# 'HISPANIC OR LATINO ORIGIN BY RACE' let's look at those

# this code filters to only concepts with the exact name 'HISPANIC OR LATINO ORIGIN BY RACE'
acs_vars %>% filter(concept == 'HISPANIC OR LATINO ORIGIN BY RACE') %>%
  # selecting only these vars to make it easy to see
  select(name, label)

# based on that output, I'm going to make a named vector of the variables I want
# I'm just grabbing the total count and counts for white, black, american indian or
# alsakan natives (aian), asian, native hawaiian or pacific islanders, and latinx
# folks. There are actually more counts here that you could use to understand even more
# about the ethnoracial composition of an area.

my_vars <- c(re_total = 'B03002_001',
                     white = 'B03002_003',
                     black = 'B03002_004',
                     aian = 'B03002_005',
                     asian = 'B03002_006',
                     nhopi = 'B03002_007',
                     latinx = 'B03002_012')




#* 5) Use the get_acs() function to download the data we want from the census
#* Ok, now that we have some variables, we can get some data

# I'm using this function to get tract-level data on my variables for the 
# acs five year ending in 2021 (2017-2021), and just for Illinois yep
acs_21 <- get_acs('tract', variables = my_vars, year = 2021, state = 'IL')

#* 5.a)
#* It turns out that we get the data in 'long' format, so we'll have to 
#* move it to wide format to use it well. Also, we generally want the ethnoracial
#* _proportions_ rather than the counts, so we'll also make those in this step

acs_21 <- acs_21 %>% 
  # we're dropping the margin of error because we won't use it here
  select(-moe) %>%
  pivot_wider(names_from = 'variable', values_from = 'estimate') %>%
  mutate(across(c(white, black, aian, asian, nhopi,latinx), .names = "{.col}_prop",.fns = function(x) x/re_total))

#* 6) Explore our data using some basic plots

# Now we can make plots of these variables

# a histogram of latinx population in IL
acs_21 %>% ggplot(aes(latinx_prop))+
  geom_histogram()

# a scatterplot of latinx prop by  black prop
acs_21 %>% ggplot(aes(black_prop, latinx_prop))+
  geom_point()


##* we also wanted data about rent! Try the steps to (a) find the right 
##* variable name for the median contract rent (hint: the concept is also called
##* median contract rent, all caps tho), (b) add that variable to the my_vars list, 
##* (c) re-download the acs data with the rent variable, and (d) make a new scatterplot
##* comparting the proportion of white folks and the rent.


##* BONUS: Look at the plot of white_prop by rent. It looks strange, right? That's 
##* because, I'd argue, it's combining data from two somewhat distinct processes. 
##* You already have data in your acs_21 dataframe that can explain some of that process.
##* Can you color your points by a variable to show the two processes? Hint: you'll
##* probably need to create a new variable with mutate to do the coloring. More hints 
##* farther down if you want spoilers
##* 
##* 














##* Hint 2: The variable you need is related to the location of the census tract

















##* Hint 3: Specifically think about differences by county





















##* Hint 4: look at places w/i or outside of Cook County

















##* Hint 5: you can use this code: acs_21 %>% mutate(cook_county = str_detect(NAME, 'Cook County'))

