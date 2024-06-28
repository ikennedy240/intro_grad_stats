# R LAB 3 (NEW) - More data prep and summary measures
# SOC 502, Kennedy

### LAB 3 MILESTONES ###
# *THESE ARE THINGS YOU SHOULD BE ABLE TO DO BY THE END OF THE LAB*

## load data from an .Rdata file
## Write code to install and load libraries
## Clean and recode variables
## Idenfity code used to make a function
## Calculate measures of central tendency

# ****************
# INTRODUCTION
# In this lab we will:
#   1) Look at another of the dataframes created for SOC 502;
#   2) Practice cleaning up some variables of interest; and
#   3) Call some measures of central tendency and variability.

# Along the way, we will create a FUNCTION.

#* For more information on the techniques and concepts illustrated in this lab, 
#* see Hands-On Programming in R at https://rstudio-education.github.io/hopr/basics.html


# ****************
# PART 1) Look at another of the dataframes created for SOC 502;

#* For this lab we'll will use the data prepared for the "Race, gender, work, and wages" 
#* topic. these are data from the Current Population Survey, a huge survey of 
#* American adults regularly conducted by the US Census Bureau. See more about the 
#* data in the codebook on the course webpage.

##* Once you download the data to the data directory of your project folder, 
##* you'll see that it's in a different format than what we've used so far. Instead of a '.csv'
##* file, we have an '.Rdata' file. CSV files are just text files where each row is a line, and each column
##* is separated by commas -- CSV stands for "comma-separated values". That means you can easily
##* open a CSV file in a text editor or even in excel. An rdata file, on the other hand,
##* is a compressed record of data stored in an R environment. It's a format specific to R, but 
##* it can be convenient because it's easy to load and can store multiple variables -- including 
##* multiple dataframes. Loading one is easy:
load("data/CPS_2016_2021.Rdata") # loading the data for the "Neighborhoods and Health" project

##* Now load the tidyverse!
##* Look back at labs 1 and 2 and find the code to load the tidyverse
##* If you get stuck, check the hints at the bottom of the file

##* then we can glimpse the new data
glimpse(CPS_2016_2021_new) # get a list of the variables in the dataframe

## if this doesn't work, maybe you didnt' load the tidyverse ?? ^_^

# ****************
# PART 2) Practice cleaning up some variables of interest;

#* For this exercise we'll looking at the association between citizenship and income 
#* The variables of interest are called CITIZEN and INCTOT.

#* the `$` operator let's us select just a single variable
#* Rstudio will try to autocomplete a column name if you type $ after a data frame
##* Try it:
CPS_2016_2021_new

#* Now lets use that to look at the variables we're interested in.

# CITIZEN VARIABLE (from the codebook):
#  1=Born in U.S
#  2=Born in U.S. outlying
#  3=Born abroad of American parents
#  4=Naturalized citizen
#  5=Not a citizen
#  9=NIU (not in universe)

#* That's what the codebook says, but what does it look like in the data frame
#* itself?

str(CPS_2016_2021_new$CITIZEN) # shows that the CITIZEN variable is numeric

#* But the real variable isn't a numeric variable, right? What kind of variable is it?

#* So we'll need to recode it appropriately.

# INCTOT VARIABLE:
str(CPS_2016_2021_new$INCTOT) # investigate the features of INCTOT
#* There are some weirdly large numbers
#* The codebook indicates that 999999998 and 999999999 are used to indicate missing 
#* information for this variable. We'll need to tell R to treat these values as missing.


#* We need to make some changes to both variables so we'll use a new
#* function, `mutate`. Mutate makes a new variable, usually based on one or
#* more existing variables.

##* INCTOT is a bit simpler so we'll do that first

#* Essentially, we want to recode the values of 999999998 and 999999999 to be missing
#* In R we generally represent missing values as `NA`. To do this, we'll use the
#* `if_else` function which tells R to set one value if the condition is true, and a 
#* different value if the condition is false.

##* Practice with if_else by changing the test value, 
##* the condition, or the true/false outputs

# like this:
test_value <- 300
if_else(test_value >100, 'soo big', 'not big')


##* The assignment operater tells R to rename whatever we do to
##* CPS_2016_2021_new. 
##* Take the data and pipe it
CPS_2016_2021_new <- CPS_2016_2021_new %>%
  # Then we use the `mutate` function to assign a new definition to INCTOT
  # By putting false = INCTOT, we tell 
  # R to maintain existing values of INCTOT where INCTOT < 999999998
  mutate(INCTOT = if_else(INCTOT >= 999999998, NA, INCTOT))

# the summary shows us that our new maximum value seems reasonable as an income
summary(CPS_2016_2021_new$INCTOT)

##* We'll do a similar process to recode the CITIZEN variable. In this case,
##* we need to recode lots of values, so we could just use a series of nested
##* if_else statements. But that can be hard to read, so we're going to use a 
##* function called `case_when` instead. Like `if_else`, `case_when` assings values
##* based on conditions, but instead of a 'false' value, you can simply introduce
##* new conditions. Any values that don't meet any conditions will be NA.

#* Both `if_else` and `case_when` work with vectors

##* Play around with the test vector and the `case_when` conditions
##* and outputs 
test_vector <- c(50,150,250, 300,350)
case_when(test_vector < 100 ~ 'little', # within a function
          test_vector < 200 ~ 'medium', # I can press enter after a comma
          test_vector < 300 ~ 'big',    # to make my code easier to read and comment
          test_vector >= 300 ~ 'really big')


## now we can apply it to the variable we want to recode
CPS_2016_2021_new <- CPS_2016_2021_new %>%
  mutate(citizen_chr = case_when(CITIZEN<=3 ~ 'US Born',
                                 CITIZEN==4 ~ 'naturalized citizen',
                                 CITIZEN==5 ~ 'non-citizen',
                                 CITIZEN==9 ~ NA))


#* When you recode like this, it's easy to make mistakes with the levels
#* So it's a good idea to look at a table of BOTH the old and new variables
#* To make sure that everything's in the right spot:

CPS_2016_2021_new %>% count(CITIZEN, citizen_chr)
#* great! this looks like all the values ended up where we wanted



#* CREATING A CLEAN DATAFILE CONTAINING ONLY THOSE CASES WITH VALID INFORMATION 
#* ON MY VARIABLES OF INTEREST
#* Side note: this process is called 'list-wise deletion' and while it's a popular
#* way to deal with missing data, it can result in bias, especially if there's 
#* some reason cases have missing data that's associated with one of your variables
#* of interest. There are interesting better ways to deal with this, most notably
#* a process called 'multiple imputation', but they are beyond the scope of the course.
#* At the very least you should take note of how many cases were lost in this process

full_data_rows <- nrow(CPS_2016_2021_new)
clean_cps <- CPS_2016_2021_new %>% filter(!is.na(citizen_chr),
  !is.na(INCTOT))
lost_rows <- full_data_rows - nrow(clean_cps)

print(paste("We lost",lost_rows,"to list-wise deletion"))


# ****************
# PART 3) Creating some measures of central tendency and variability.

# INCTOT is an interval variable so I can do any measures of central tendency and variability

# MEASURES OF CENTRAL TENDENCY
#calculating the mean (with a redundant option to remove missing cases)
mean(clean_cps$INCTOT,na.rm = TRUE)

#calculating the median (with a redundant option to remove missing cases)
median(clean_cps$INCTOT,na.rm = TRUE)

#there is no built-in function to get the MODE, so lets create one:

#* here the assignment operator is assigning our function to the name
#* `ourmode`
#* function(v) is a special R syntax that says "I'm making a function with 
#* one input argument, called v" You can specifiy as many arguments with whatever
#* names you want. So burger <- function(bun, patty, toppings) tells R that you're
#* making a function called `burger` with arguments called bun, patty, and toppings
ourmode <- function(v) {
  # this takes the input argument v and finds all the unique values
  uniqv <- unique(v)
  # this selects the unique value(s) which have the highest number of observations
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

ourmode(clean_cps$INCTOT) # use our new function to get the mode of income


# MEASURES OF VARIABILITY
sd(clean_cps$INCTOT) # calling the standard deviation of our INCTOT variable
range(clean_cps$INCTOT) # calling the range of our INCTOT variable


# new_citizen is a nominal variable so I'll just pull up a frequency table
##* uncomment the next line (delete the hashtag) if you need to install the package
# install.packages('epiDisplay')
library(epiDisplay)
tab1(clean_cps$citizen_chr, cum.percent = FALSE)

ourmode(clean_cps$citizen_chr) # using our new mode function to confirm mode of the citizen variable

### LAB 3 CHECKOUT

# Complete these exercises to move on to the next lab
# if you don't know what to do, review the content above and then ask your TA

# 1. Write code that would load data from an rdata file saved to 'data/fake.rdata' -- it won't do anything though
# 2. Write code that will install and then load a package called 'stargazer' (this will work)
# 3. The UNION variable in the CPS_2016_2021_new dataframe, has four values: 0,1,2 and 3. 0 corresponds
#    to respondents who didn't receive that question. 1 is for people who aren't in and 
#    aren't covered by a union. 2 is for people who are in a union. And 3 is for people who
#    are 'covered' by a union. Recode the variable UNION to be a character vector with descriptive names for 
#    each value, then show your results with a table.
# 4. Find and copy below the code above that creates a new function
# 5. Find the mean, median, mode, standard deviation, and range of HHINCOME - the 
#    household income variable. How are those values different than for INCTOT?


