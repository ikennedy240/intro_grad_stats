# R LAB 2 - Messy data and Freq tables and More graphical techniques
# SOC 502, Kennedy

# ****************
# INTRODUCTION
# Now that you have a basic understanding of how R works (see Lab 1), we'll focus here on four activities:
#   1) Review the basics of reading, select, and filter with a dataframe;
#   2) Deal with some missingness (selecting cases with valid information)
#   3) Create and analyze a new variable;
#   4) Load and manipulate a larger dataframe; and
#   5) Create a frequency table.

#* For more information on the techniques and concepts illustrated in this lab, 
#* see Hands-On Programming in R at https://rstudio-education.github.io/hopr/basics.html


# ****************
# PART 1) Review the basics of importing, indexing, and subsetting a dataframe


#* The us-states.csv file contains COVID-19 incidence data from the NY Times (https://github.com/nytimes/covid-19-data/blob/master/us-states.csv)
#* that Ian summarized and joined with data about state restrictions (https://github.com/govex/COVID-19/blob/master/data_tables/restrictions_policies_detailed.csv)

#* Remember that we need to load the libraries we need!
library(tidyverse)

# Loading the data:
covid_data <- read_csv("data/covid_state_data_fixed.csv")

#* Now lets look at the basic content of the data
head(covid_data)  # showing the first few rows of data and the column names

#* Notice that when we use the head function, depending on how wide your screen is,
#* you might see that it doesn't actually show you all the variables. There's another 
#* useful function that will let you see a few rows from all of the variables
glimpse(covid_data)

##* what is an example of a character column in this data? a numeric column? a date column?
##* 

#* Now let's focus in on the variable "restrictions" -- a variable indicating 
#* the relative severity of covid restrictions that I made from the `Restriction details include` column
#* States that had stay at home or shelter in place orders I marked as high
#* States that included some buisiness or school closures I marked as medium
#* and I marked other states as low.

##* This shows that the variable is treated as a character variable even though the information is meant to be ordinal
typeof(covid_data$restrictions)   

# Let's make it ordered and set the order
covid_data$restrictions <- ordered(covid_data$restrictions, 
                                   levels = c("low", "medium", "high")) 
is.ordered(covid_data$restrictions)   # Shows that the information in the variable is now "ordered"
unclass(covid_data$restrictions)   # Shows the the numbers used for each level, BUT ALSO SHOWS SOME "NA" values


# ****************
# PART 2) Deal with some missingness (selecting cases with valid information)

#* The analysis above showed that the variable, "restrictions" has some 
#* missingness -- some cases with missing information on the variable. 
#* We want to deal with these cases so they don't mess up our analyses--regression
#* and some other kinds of analysis won't work with missing cases.

##* Look at the cases with missing data. What is the condition that we're using in the `filter`
##* function? Try looking up the documentation for that function by typing `?is.na` in the console
covid_data %>% filter(is.na(covid_data$restrictions)) %>% # you can hit return after a pipe to make code more readable
  select(state,total_cases,restrictions)

#* What do you notice about the missing cases? Why do you think these ones are
#* missing data?


#* There are lots of ways to deal with missingness but let's 
#* take the easy strategy of eliminating cases with missing 
#* information on the variables of interest ("total_cases", "pop_2020", and "restrictions")


##* creating a new dataframe with just the cases (row) we want (but all columns)
clean_covid <- covid_data %>% # remember the arrow here assigns the dataframe to a new name
  filter(!is.na(covid_data$total_cases), # we can pass multiple conditions to filter
         !is.na(covid_data$pop_2020), # here they're separated by commas, and a return
         !is.na(covid_data$restrictions)) # so that the code is more readable




# ****************
# PART 3) Create and analyze a new variable;

#* Let's say we want to look at how covid restictions were related to the number 
#* of covid cases recorded in states (note: the direction of causality here is ambiguous)

# Notice that the values for the number of cases vary widely:
clean_covid$total_cases  

##* we can also plot the distribution of cases:
hist(clean_covid$total_cases) # this is a base R plot, simple and easy to make

#* but we expect the number of cases to be related to the state population
##* so let's plot that:
ggplot(clean_covid, aes(pop_2020, total_cases))+ # giving ggplot the data, the the x, and y in the aes function
  geom_point() # asking for one point per case

#* This plot shows that a lot of the variation in covid cases seems driven 
#* by state population

#* We want to make the number of cases more comparable across states by 
#* converting cases into a RATE (cases per 100,000 population)
#* When we make a new column we can use the `mutate` function

# ****************
# MUTATE
#* inside the `mutate` function, we put our new variable name, then the equals sign
#* and then some instructions on how to calculate the new variable.
#* In this case we want cases per 100K so we take total cases divided by population times 100k

##* this code creates the variable and assigns the whole new thing to the same
##* variable name we had before -- ie, it replaces our old clean_covid object with a
##* new one that includes our new column, `cases_per_100k`
clean_covid <- clean_covid %>% 
  mutate(cases_per_100k = total_cases / (pop_2020/100000))


# Let's create a chart showing the the number of cases by the level of restriction

ggplot(clean_covid, aes(x=restrictions, y=cases_per_100k))+
         geom_boxplot()+
         xlab("level of covid restrictions")+
         ylab("covid cases per 100,000 pop")+
         ggtitle("Covid cases per capita by Level of Restrictions in States")

###* What conclusions can you draw from this plot?
###* 
###* Push Further: 
###* 1. We focused on cases for this analysis. Does it look the same when we 
###* consider total deaths?
###* 
###* 2. We made this plot by creating a variable of the case rate 
###* using the 2020 population. But the data includes 2021 and 2022 population
###* estimates too. Do you think one of those estimates would be a better fit for
###* this analysis? Why or why not?
###* 
###* 3. Redo the analysis using population estimates from 2022. What about the plot 
###* looks different? What do you think accounts for the difference?


# ****************
# PART 5) Create a frequency table


#* There are numerous ways to create frequency tables in `R` and they vary slightly 
#* depending upon what information you want produced. The most basic function you 
#* can call is `table` which will take one or more objects (which can be coerced into factors) 
#* and will build a frequency table.

##* use `table` to make a count of states in each level of restriction
table(clean_covid$restrictions)

#* A somewhat more useful way to show the same info is to use the `count` function.
#* Unlike `table`, `count` returns a dataframe that you can save or pipe to something else

##* Use count to make the same table. Are the results the same?
clean_covid %>% count(restrictions)

##* We can pass multiple variables to `count`:
clean_covid %>% count(restrictions, earliest_case_month)


#* Making a nicer frequency table with package epDisplay
#* R has lots of packages you can use to make fancier stuff
#* For instance, epiDisplay is a package developed for displaying
#* data for epidemiological studies. Let's install it and use it's
#* table function. It's nice because it will give us percent and cumulative
#* percent columnts
##* install the package 
install.packages('epiDisplay')
##* load the package
library(epiDisplay)
##* and make the table
tab1(clean_covid$restrictions, cum.percent = TRUE)

#* a table like this can pe copied and pasted into an r script, say
#* if your professor asks you do to do that for homework. It still looks
#* ~~ok~~ 

# clean_covid$restrictions : 
#         Frequency Percent Cum. percent
# low             3       6            6
# medium          6      12           18
# high           41      82          100
#   Total        50     100          100

# **************** MORE LATER!!!! ****************

