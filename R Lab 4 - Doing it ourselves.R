# R LAB 4 - Doing it ourselves
# SOC 502, Kennedy

# ****************
# INTRODUCTION
# In this lab we will:
#   0) Write code based on previous examples
#   1) Make plots of simulated normal data
#   2) Make plots of simulated probability data
#   3) Download, read into R, and summarize a new dataset

#* So far the labs have generally been following along code that's been written
#* But for your HW2 you'll have to start writing R code that will do the things
#* you need to answer your own questions with your own data. To scaffold your success
#* there, this lab is a chance to start writing code in a (generally) blank script
#* You'll have 3 tasks, each _analogous_ to stuff you've already done or stuff
#* that I've already done for you in anothe script. Your task is to use that code
#* written elsewhere to perform the tasks outlined below.

#* For more information on the techniques and concepts illustrated in this lab, 
#* see Hands-On Programming in R at https://rstudio-education.github.io/hopr/basics.html


# Task 1: Make plots of simulated normal data
#* First, work through the 'Distribution Plots.R' file on blackboard
#* That file has example code that creates a dataframe of normal data and 
#* plots that data in a density plot. Look to the hints for help.

##* Make a dataframe that includes 5,000 draws from a normal distribution with a 
##* mean of 4 and a standard deviation of 4. Print the mean and standard deviation
##* to make sure that your simulation worked. Plot the simulated data in a density plot. 

# Task 2: Make plots of simulated probability data
#* First, work through the 'probability_examples.R' file on blackboard
#* You're going to simulate drawing socks from a sockdrawer
##* 2.1 define a sample space with the following sock colors: blue,
##* white, black, purple, lavender, pink, and grey


##* 2.2 Make a sock drawer that has 4 lavender socks and 1 sock of each other color

##* 2.3 Define an event, L, as drawing a lavender sock. Calculate p - P(L) the probability
##* of drawing a lavender sock.


##* 2.4 use the sample function to draw 1 sock from the sock drawer

##* 2.5 Simulate a year of picking socks from that drawer by making a dataset of 
##* 365 random draws (with replacement) from the sock drawer. 

##* 2.6 Plot the cumulative probability of success by the number of trials,
##* where success is defined, of course, as the event L - drawing a lavender sock

##* 2.BONUS Plot the probability density function (ie a bar graph of observed 
##* probabilities)) of all of the sock colors for the 365 days. 

# Task 3: Download, read into R, and summarize a new dataset

##* There is a .csv file on Blackboard called Drag Race Contestants. You're going to 
##* download it, modify it, and make some plots with it. 

##* 3.1 Download the `dr_contestants.csv` data from blackboard and move it into the
##* data directory of your rproject folder.

##* 3.2 Read the data into r, call the r object 'drc'
##* 
##* 3.3 Make a new dataframe called 'worthy_queens' that only includes queens
##* who have appeared in a finale. 

##* 3.4 Make a new variable in drc called `score` which combines a queens wins 
##* and bottoms. A queen should get 2 points for every win and -4 points for every bottom.
##* Make sure to update drc with that new column

##* 3.5 Make and interpret a histogram of the new score variable you just made

##* 3.6 calculate and interpret the mean, median, and standard deviation of the 
##* score variable. 


### Hints
#* Task 1: 
#* Try copying and modifying lines 35-51 in the Distribution Plots.R file.
#* Note that that code actually makes three distributions and you only need one
#* It should work fine though.
#* 
#* 
#* Task 2:
#* Try starting with lines 33-67 from the probability_examples.R file
#* 
#* Task 3:
#* 3.2 use the read_csv function
#* 3.3 use the filter function
#* 3.4 use the mutate function
#* 3.5 use ggplot() and geom_histogram() or base R hist()
#* 3.6 use the mean, median, and sd functions, or the summary function
