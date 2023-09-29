# R LAB 1 - Introductions and Getting Started in R
# SOC 401, Crowder


# ****************
# INTRODUCTION
# The goal of this lab is to introduce you to R and R Studio, and to get you to 
# think about some basic terminology used in R programming. As described in class,
# we take a practical/applied approach to R in SOC 401; my goal is to get you using 
# the program as quickly as possible to do some useful statistical analyses. 
# If you approach this with an open mind, you will pick up a lot about general 
# operations of R programming and quickly develop some useful and highly marketable skills.

# For more information on the techniques and concepts illustrated in this lab, 
# see Chapters 2 and 3 of Hands-On Programming in R at https://rstudio-education.github.io/hopr/basics.html
# A note about citations
# [Sections of these labs have been sourced from various freely-available online resources. 
# They are cited in the superscripted hyperlinks.]


# ****************
# What is R?
# (https://www.r-project.org/about.html)

# R is a programming language and environment for statistical computing and graphics. 
# It is open-source (free!), popular (so many resources!), and community-driven 
# (new features are being added all the time!). Unlike statistical software you may 
# have used in the past (Excel, SPSS, SAS, Stata, etc.) R is an actual programming language, 
# similar to the S language, which means you'll need to learn how to program in R. 
# But don't worry - these labs have been created to help you understand what's going on,
# where to find additional resources for specific topics, and ultimately, 
# how to analyze data with the statistical techniques you'll be learning. 
# First things first, you'll need to download a copy of R on the computer you'll 
# be using to complete your homework throughout the quarter/year. 


# ****************
# What is R Studio
# (https://rstudio-education.github.io/hopr/starting.html#starting)
# R isn't a program that you can open and start using, like Microsoft Word or Internet Explorer. 
# Instead, R is a programming language, like C++, python, or julia. 
# You use R by writing commands in the R language and asking your computer to interpret them. 
# In the old days, people ran R code in a UNIX terminal window-as if they were hackers 
# in a movie from the 1980s. Now almost everyone uses R with an application called 
# RStudio and it's what we'll use in this class as well. RStudio **is** an application 
# like Microsoft Word-except that instead of helping you write in English, RStudio helps you write in R.

# If you are reading this, you have already accomplished the first step of 
# installing and accessing R. Take moment to pat yourself on the back. Now onto the first lab...


# ****************
# Scripts and running commands
# A note here about basic workflow techniques: It is possible, and sometimes convenient, 
# to run R commands straight from the Console in R-Studio. However, code that you run 
# from the console is not saved, making it harder for you to replicate and build on your work later. 
# Best practice is to write all of your code in a script (like the one you are reading now) 
# and then run the code from your script. You can then save the script to use and/or recycle later.

# There are a couple of ways to run R code from a script, but the easiest is to highlight 
# lines in the editor window and click Run at the top or hit Ctrl+Enter or Cmd+Enter to run them all.
# You can press Ctrl+Enter or Cmd+Enter at any time to run the line where your cursor is.


##* Try running the line of code below:

print("Hello World")

# the words 'Hello World' should have printed to the console.
##* Now try changing the words that print by changing "Hello World" to "Stats Rock" or something else
##* Then try running your new line again. 

#* A lot of the code that we give you in lab will be set up to do something specific,
#* like to print the words 'Hello World'. But that same code can also be used as a template
#* to do something else, like to print 'Stats Rock' or 'Get me out of here' or something else.
#* Unlike when writing prose (or when writing code for a computer science course), in this class--
#* and in Sociology generally--it is acceptable to copy and modify code from one of these labs, or from
#* online, or from a textbook, or from another source, and to modify it to suit your purposes.
#* Of course, when you do that it is your responsibility to make sure that the new code does what
#* you expect. Our intention is that these labs serve as a cookbook for your homework, for your
#* final project, and for basic data analysis and statistics in the future, especially paired with what
#* you learn in 402.  


# ****************
# Setting your working directory
# A second note about basic workflow techniques: R scripts work like any other file in your 
# computer in that they are saved in a directory format (i.e. in a specified folder). 
# One added complexity is that every time you start an R session (open R Studio) you 
# are working from something aptly called the "working directory." Preferably you 
# want the folder location of the script you're working on and the working directory 
# to be one and the same. This is because life is easier if anything you reference in an R script 
# (i.e. a dataset you want to import, another r script with functions you wrote, etc.) 
# is in the same folder as said referencing R script in order to properly execute whatever 
# commands you tell R to carry out. Therefore, it is wise to be very clear and organized 
# in the way you set up your folders and files to prevent future headaches. 
# For instance, you can save your homework scripts in specific folders for each homework assignment, 
# which will all be within a folder specifically for this class. 
# You can read more about this at https://rstudio-education.github.io/hopr/dataio.html#working-directory.

#* To make things easier for this class, we've already set up something called an
#* 'R project' file, called '401Labs.Rproj'. As long as you open your work using
#* That R project file, it will automattically set your working directory.
#* But sometimes you'll need to do that manually. Here's how:

##* use the command below to see your current working directory
getwd()

##* you can use this command to se the working directory to something else
setwd("/some/file/path")

### A note about Errors
#* Unless you have a file saved to '/some/file/path', you'll get an error when you try 
#* to run the code above. When you're starting out with programming and statistical software
#* it's common to get worried about errors--it can feel like you've broken something (even though
#* everything is probably totally fine!). As you get more experienced, errors will become 
#* useful signals about what you need to improve in your code, or at least what you need to google
#* in order to find out what you need to improve. 

#* Here's the key thing: errors are normal, and common. They don't mean that anything is broken, 
#* and they certianly aren't a judgement about you, your code, or your ability. Both coding
#* and statistics are hard, and complicated, and especially difficult when you're starting out.
#* Errors aren't something that only happen to new coders--I get errors all the time. The trick is 
#* to trust that as you gain experience, you'll understand more and more about what errors mean
#* and they'll become useful. For now, it's fine if it's just scary red text. Show it to me or to 
#* Mariya and we'll help you figure out what's going on.

#* In the case of trying to use 'setwd' above, that command won't work unless it has a _valid directory path_
#* as an argument. A _valid directory path_ just means a way to point the computer to a real file that
#* exists on your system.


# ****************
# OBJECTS
# R works by creating and manipulating OBJECTS of a variety of kinds (vectors, matrices, dataframes, etc). 
# These objects are just named bundles of infomration that can range from the very simple to really complex.

# Below we are creating a super simple VECTOR object of length 1 called "thing" that takes a value of 4
# Notice that the less-than sign "<" combined with the minus "-" sign make an arrow. point this arrow at 
# the object that you want to create to tell R what bundle of information you want to  

##* Run the line below to assing a value of 4 to an object named 'thing'
thing <- 4

##* print out the contents of this object
thing 

# Once you have defined an object, you can manipulate it and analyze it in R.

##* use "thing" in a calculation
thing*11 

##* create a vector called "cool" that contains 6 numbers between 3 and 8 (the length of this vector is 6)
cool <- 3:8  # here the ":" is telling R to include everything including and between the two numbers

##* print this vector out to confirm it looks right
cool  

##* create another vector called "beans" that contains 2 numbers: 1 and 2. 
##* Note here that the c() syntax is telling R to COMBINE the 
##* numbers 1 and 2 into an object called "beans." Without the c() syntax, 
##* R will get confused about what you want to do.
beans <- c(2,4)

##* print this vector out to confirm it looks right
beans 

##* create a new vector called "coolbeans" that draws on information 
##* from the objects "cool" and "beans" by multiplying the objects together.
coolbeans=cool*beans 

##* print all three objects
cool
beans
coolbeans

# Notice here that R is RECYCLING of matrix "beans" during the multiplication with 
# "cool;" the first element of "beans" gets multiplied by the first element of "cool," 
# the second is multiplied by the second, but then "beans" runs out of elements 
# so starts over (i.e., re-cycles) to mulitply the first element of "beans" to 
# the third element of "cool" and so on. This recycling is different from basic
# matrix algebra and is important to keep in mind as you combine objects in R.


# ********************
#* DATA TYPES: Objects can contain different TYPES (kinds) of information. 
#* The most common types of information is:

#* 1) NUMERIC: The values in the object are numbers ready for mathy stuff. 
#* Numeric variables may be either integers (whole numbers) or 
#* "double" (a way to store numbers with decimals)

##* check the type of the objects `cool` and `beans`
typeof(cool)  # the vector "cool" contains numeric values (specifically "integer")
typeof(beans)  # the vector "beans" contains numeric values (specifically "double" because of the way we created it)

# 2) LOGICAL: Sometimes called BOOLEAN, the values are "TRUE" and "FALSE" indicating agreement or disagreement with some logical statement.
# creating a logical vector (imagine a variable that is "true" if the person is a senior, false otherwise) 
senior <- c(TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE) 
typeof(senior)

# CHARACTER: The elements of the object are words or letters (not usebul for mathy stuff)
# Character data types are also often called strings. R puts character data in double quotation marks
#* But in general, you can use single or double quotations. Note that Rstudio automatically puts open and 
#* closed quotation marks when you type the key only once. You can also select a word and press the key for quotation
#* marks, parentheses, tickmarks, or brackets and Rstudio will enclose the selected area with that character.

select this text and put it in double quotation marks

marbles <- c("red", "blue", "green", "blue", "green", "red", "red", "blue", "red")
typeof(marbles)


#* IMPORTANT: VECTORS (and matrices) can contain information of only one type 
#* ("lists" can have different types of values, but that is for another time). 
#* For example, if you try to create cector that contains both numbers and words. 
#* R will treat everything in the vector as a character value (this is called "coercion")

##* create a vector with all kinds of stuff in it
mixer <- c(1:4, "jeff", "crackers", TRUE, 4.3, 16L)  
mixer  # notice that everything is now in quotes which means it is character information
typeof(mixer)  # here R will confirm that the matrix contains a character variable


# ****************
#* FACTORS are a strange in-between type of information that can be super useful 
#* for dealing with categorical information. In a FACTOR, the values look like 
#* characters but R stores the information as numbers in the background. 
#* This makes it easier to include the information in statistical analyses, but you have to be careful. 
#* In essence, a factor uses numbers to distinguish QUALITATIVELY different categories 
#* (think about that for a moment); in some cases the numbers assigned indicate something 
#* about the order of the values but in other cases are arbitrary (don't reveal anything 
#* about the order of values).

# Let's see how this works out with a vector containing a NOMINAL variable for a set of nine marbles

##* creat the factor variable--it looks just like a character vector, but we add `as.factor`
##* to turn it into a factor
marbles <- as.factor(c("red", "blue", "green", "blue", "green", "red", "red", "blue", "red")) # telling R to treat the values in "marbles" as a FACTOR variable (i.e., assign numbers in the background).
typeof(marbles)  #here we see that "marbles" is now a numeric integer variable
attributes(marbles)   #this tells us that "marbles" is a "factor" with "levels" of blue, green, and red (note that here "levels" doesn't make much sense since we are dealing with a nominal variable of marble color)
unclass(marbles) # We can force R to tell us the values assigned to the qualitative categories (notice that R went in alphabetical order to assign values)

##* Now let's see how this works with a vector containing an ORDINAL variable 
##* indicating the level of satisfaction with R among a set of first-time users
sat_R <- as.factor(c("high", "low", "low", "medium", "medium", "medium", "high", "low", "high", "low")) 
unclass(sat_R) 
#* notice that R went in alphabetical order again, producing an order of numbers 
#* that is inconsistent with the ordering of level of satisfaction indicated by the 
#* responses (medium should not be the highest category)

##* Now we'll create a new version of the variable with the correct ordering
##* 
sat_R_ordered <- ordered(sat_R, levels = c("low", "medium", "high")) 
sat_R_ordered # notice the 'less than' symbols showing the order
unclass(sat_R_ordered)  # That's better!


# ****************
# MATRICES
#* To this point we have been dealing with vectors, a type of object with just 
#* a single dimension; it is a row of information that has a length (i.e., width) 
#* but no height In contrast, a MATRIX is two-dimensional; it has both width and 
#* height since the information is stored in ROWS and COLUMNS.

# Let's illustrate this by converting a vector into a simple matrix

##* tell R to create a vector made up of 1-10, four times over
vector1 <- rep(c(1:10), times = 4)  
vector1 # just a long numeric vector

##* now we'll tell R to take the vector vector1 and arrange its numbers into a matrix 
##* with 5 rows and 8 columns. The "byrow" thing tells R to fill up a full row and then go on to the next.
matrix1 <- matrix(vector1, nrow=5, ncol=8, byrow=TRUE) 
matrix1  # printing out the matrix (notice 5 rows and 8 columns)
attributes(matrix1)  # here we get the basis attributes of the matrix -- its dimensions

##* here is another version with 6 columns which results in a warning from R, reminding us about recycling
##* R still _makes_ the matrix for us, but the last row is now the same as the first row
##* So the warning reminds us just in case we didn't expect that to happen.
matrix2 <- matrix(vector1,nrow=6,ncol=8,byrow=TRUE) 
matrix2

# ****************
# INDEXING
#* Indexing is the process of referring to a value in a specific location in a matrix. 
#* For example, we might want to know the value of the element in the 2nd row of 
#* 6th column of our matrix. To do this, we tell R the name of the matrix we want 
#* to know about and the specific element we want to know about. In indexing, 
#* remember the convention ROW,COLUMN -- you call an element of a matrix by 
#* specifying its row and then its column.

##* this line calls the value in the second row, sixth column of an object(matrix) called matrix1
matrix1[2, 6] 


# ****************
# DATAFRAMES
# now that we have some basic operations down, let's mess with some more powerful stuff: DATAFRAMES.
#* Dataframes are essentially matrices with (potentially) lots of extra information packed in, 
#* and lots more flexibility. Most importantly, dataframes can have columns that each contain different types of data. 
#* For example, you can have a dataframe that has information for a bunch of individual 
#* cases defining the rows and each column containing a different variable describing 
#* the attributes of those cases. Some of the variables/columns could be numeric, 
#* others characters or factors, others logical. This makes dataframes really useful 
#* for importing and analyzing complex data. But let's start simple...

# ****************
# READING DATA
#* A lot of the data we analyze in R is imported from surveys and other 
#* data-collection efforts. To illustrate, let's __import some data__!
#* We'll import (or read) a text file containing comma-separated values, or a 'csv' file) 
#* containing information about eight mammal friends.
#* This file should already be in the 'data' folder in the '401labs' directory
#* But if you can't find it, you can also download it from here: 
#* https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/MASS/mammals.csv

# ****************
# PACKAGES
#* R has a number of 'built-in' functions, but often we want to move beyond those
#* basics. When we do that, we need to use a 'package'--also called 'libraries' or 'modules'
#* Packages are sets of code written and tested by somebody else. They have lots of helpful 
#* functions that we can use. In this class, we'll use a set of packages, called the `tidyverse`
#* to do a lot of our data work--loading data, cleaning it, subsetting it, modifying it, etc.
#* Before we can use a package we have to _load_ it. And before we can load it, 
#* we need to _install_ it. 

##* Install the `tidyverse` packages by running the next line:
install.packages('tidyverse')

#* once you install a package onto a system one time, you don't need to do it 
#* agian (until you update your R version or something). So we can "comment out"
#* The install line above by putting a '#' in front of it. You'll see the code
#* change color in Rstudio, and that way the code won't run again if you
#* work your way through this lab later on.


##* load the `tidyverse` packages
library(tidyverse)
#* note that loading the tidyvers introduces some conflicts. Specifically the `dplyr`
#* functions for `filter` and `lag` _replace_ the default fuctions from the `stats` package.
#* That's fine for us--we want to use the `dplyr` versions. But it's worth taking note
#* of these kinds of warnings when you load a package.

##* Run this line to read the data
mammal_data <- read_csv("data/mammals.csv")
mammal_data  #look at the data

##* with big datasets we don't want to look at the whole thing, 
##* so we can look at just the top using the head command
head(mammal_data) 
tail(mammal_data) # Can also look at just the end of the dataframe with tail
attributes(mammal_data) # can also look at the basic content of the data -- the column names are particularly helpful

#* We can also investigate the various kinds of information stored in the columns 
#* (variables) of the dataframe. Use the $ after the object(dataframe) name to 
#* specify what column you are interested in looking at.
#* 
typeof(mammal_data$name) # checking the kind of information in the "name" column (variable) of mammal_data
typeof(mammal_data$body) # checking the kind of information in the "body" column (variable) of mammal_data
typeof(mammal_data$brain) # checking the kind of information in the "body" column (variable) of mammal_data

# we can index a dataframe just like in a matrix
mammal_data[5,3] # looking at the element in the 5th row, 3rd column


# ****************
#* SUBSETTING: Sometimes we want to focus on just certain cases from a dataset 
#* that meet some important criterion (e.g., individuals in a particular age category, 
#* or cases with complete information on an important variable of interest). 
#* The easiest way to do this is to create a new object that contains just the information that you want.

#* Selecting certain rows or columns: For many subsetting tasks, 
#* we will keep just certain rows and/or columns of the datframe. 

# ::: ASIDE :::
#* The tidyverse uses named functions for subsetting. In base R, subsetting happens
#* using a different format in brackets. Our class will mostly use tidyverse format,
#* but you may see base R subsetting. You can read about it here: 
#* https://sparkbyexamples.com/r-programming/r-subset-data-frame-with-examples/

#* Normally, we want to either _select_ certian columns in our data by name, or
#* _filter_ our dataframe to rows which match particular conditions. Thankfully
#* the tidyverse has functions that do those things are are named after what they do.

## select

##* Select one or more columns like this:
mammal_data %>% select('name','brain')

#* the line of code above uses `%>%`, which is called the 'pipe'.
#* The pipe takes whatever goes into it, from the left, and passes it to the
#* function on the right (as the first argument). So here we're telling R to take 
#* mammal_data and pass it to the select function. 

##* We can assign the output of the select function to a new object:
name_brain <- mammal_data %>% select('name','brain')
##* This new object only has the name and the brain from before
name_brain

###* use the above code to make a new object called `name_body` that has the 'name' and 'body' columns


## filter
#* filter is a little more... involved than select
#* It asks us to define a condition, and it filters the dataframe to only rows
#* that meet the conditions.

##* Let's filter mammal data to only mammals with brains bigger than 100.
mammal_data %>% filter(brain > 100)

###* building from the work above, create a new object called `big_brain` that
###* has all of the animals with brains larger than 500. How many animals are there 
###* in that new dataframe?



# ****************
# MAKING A GRAPH
# Now that you are starting to get the hang of basic R programming, lets remember where this is heading: towards some cool data-analysis capabilities. To illustrate, let's make a graph.

# Making graphs in tidyverse is simple.

##* make a simple dot plot
mammal_data %>%               # take the data
  ggplot(aes(brain, body))+   # use the ggplot function, and tell it we wan to plot 'brain' x 'body'
  geom_point()                # specify a 'geom', in this case `geom_point` since we want a dotplot


##* make a bar blot
mammal_data %>%               # take the data
  filter(brain > 500) %>%     # filter to big brains
  ggplot(aes(name, body))+    # use the ggplot function, and tell it we wan to plot 'name' x 'body'
  geom_col()                  # specify a 'geom', in this case `geom_col` since we want a bar plot




# **************** MORE LATER!!!! ****************