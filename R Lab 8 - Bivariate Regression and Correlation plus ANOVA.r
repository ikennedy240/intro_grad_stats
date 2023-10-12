# INTRODUCTION
# In this lab we will:
#   1) Use correlation and regression to look at the association between two interval-level variables;
#   2) Use a combination of ANOVA and regression to look at the effect of a categorical independent variable on an interval-level dependent variable.

# For more information on the techniques and concepts illustrated in this lab, see Hands-On Programming in R at https://rstudio-education.github.io/hopr/basics.html


# ****************
# Familiar prep work

# For this lab I will use the data prepared for the "Neighborhoods and Health" topic. This file combines data from the PLACES data which contains information on the health characteristics of populations within census tracts (neighborhoods) and NaNDA data which brings together information on the demographic, economic, social, and ecological characteristics of these neighborhoods. The datafile for this class includes a random sample of 3,000 census tracts drawn from the population of all 74k census tracts in the US. See more about the data using the resources on the course webpage.

# Specify your working directory
#setwd("C:/Users/kylecrow/Desktop/504_R_work")
setwd("C:/Users/Kyle/OneDrive/Desktop/504_R_work")
getwd()

# Once you download the data to your working directory, opening the file is easy:
load("PLACES_NaNDA_sample.Rda") # loading the data for the "Neighborhoods and Health" project

ls(PLACES_NaNDA_sample) # get a list of the variables in the dataframe


# In this example I am interested in looking at the predictors of the prevalence of ASTHMA in the area.
# Do any necessary cleaning on our variables of interest
# some variables I'm interested in from the data:

# asthma = # people with asthma per 1,000 population
# pov13_17 = Proportion people w/ income past 12 months below the federal poverty line
# count_open_parks = the total number of parks within the census tract
summary(PLACES_NaNDA_sample$ppov13_17)
summary(PLACES_NaNDA_sample$asthma)
summary(PLACES_NaNDA_sample$count_open_parks)

# Looks like there are some cases with missing information on my key variables so I am going to create a clean version of the dataset that includes only those ROWS with non-missing (!is.na) on the variables included in my analysis.
# Note that here I am also just keeping the variables (COLUMNS) I am interested in analyzing, just for efficiency
clean_PLACES <- PLACES_NaNDA_sample[
  !is.na(PLACES_NaNDA_sample$ppov13_17) &
      !is.na(PLACES_NaNDA_sample$asthma) &
        !is.na(PLACES_NaNDA_sample$count_open_parks),
          c("ppov13_17", "asthma", "count_open_parks")]


# ****************
# PART 1) Use correlation and regression to look at the association between two interval-level variables

# Looking at the correlation between my focal variables.
# Specifying a Pearson Correlation coefficient (rather # than a Spearman or other rank-order type).
# Following line requests the correlation AND an hypothesis test for my focal association.  Here the null hypothesis is that the correlation in the population (rho) = 0.
cor.test(clean_PLACES$asthma, clean_PLACES$ppov13_17, method="pearson")


# VISUALIZING THE ASSOCIATION (optional)
# Because I'm fancy, I want to visualize the association between my variables using a scatterplot.
# Here I'll install and use a simple function called "ggscatter" within the package called "ggpubr"
install.packages("ggpubr")
library("ggpubr")
ggscatter(clean_PLACES, x = "ppov13_17", y = "asthma", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Poverty Rate", ylab = "Asthma/1000")


#fit simple linear regression model
reg_results <- lm(clean_PLACES$asthma ~ clean_PLACES$ppov13_17)  #estimating a linear regression model (lm) and storing the results as "reg_results") 
summary(reg_results)  #view the results


# ****************
# PART 2) Use a combination of ANOVA and regression to look at the effect of a categorical independent variable on an interval-level dependent variable.

# For this part I will investigate the association between the concentration of local parks and the levels of asthma across tracts.
# For theoretical reasons, I believe that the important contrast is between three groups of tracts: 1) those with no parks; 2) those with a single park; and 3) those with more than one park. In this case, my inpendent variable will be categorical (specifically, ORDINAL, but the techniques below also work for nominal categorical variables).

# Create the categorical variable that aligns with my theory.
clean_PLACES$parks_cat <- as.factor(
  ifelse(clean_PLACES$count_open_parks==0, 'no parks',
      ifelse(clean_PLACES$count_open_parks==1, 'one park',
          ifelse(clean_PLACES$count_open_parks>1, 'multiple parks',
                       clean_PLACES$count_open_parks))))

table(clean_PLACES$parks_cat)  #checking the distribution of the new variable


# Using ANOVA to look at the basic association between my focal variables.
# Note that I am not using correlation here since one of my variables is categorical.
anova_results <- aov(asthma ~ parks_cat, data = clean_PLACES)
summary(anova_results)

# Using BIVARIATE REGRESSION to look at the basic association between my focal variables.
# Since I have a categorical IV I need to use DUMY VARIABLES to incorporate it into the regression analysis.  I will create separate dummy variables for each of my categories of the IV. Each dummy variable will take a value of "1" for those cases (tracts) in the specific category and "0" for those not in the specific category.

# Creating 3 dummy variables, one for each of the categories of my IV
clean_PLACES$no_parks <- ifelse(clean_PLACES$parks_cat=='no parks',1,0)
clean_PLACES$one_park <- ifelse(clean_PLACES$parks_cat=='one park',1,0)
clean_PLACES$multiple_parks <- ifelse(clean_PLACES$parks_cat=='multiple parks',1,0)

# checking to make sure the variables are working the way they should
table(clean_PLACES$parks_cat) # original ordinal variable
table(clean_PLACES$no_parks)  # dummy variable for NO PARKS
table(clean_PLACES$one_park)  # dummy variable for ONE PARK
table(clean_PLACES$multiple_parks)  # dummy variable for MULTIPLE PARKS

# Adding the dummy variables to my regression analysis. 
# Need to include in my regression analysis k-1 dummy variables, where k = the number of categories in my IV. By omitting the dummy variable for one of the categories of my IV I am setting that category as the REFERENCE CATEGORY. The coefficient for each of the dummy variables that are included in the regression model then represent the average difference in the DV for cases (tracts) in that specific category and cases in the reference category.
# For example, I will exclude the dummy variable for "no parks" and include the dummy variables for "one park" and "multiple parks." This makes "no parks" the reference category. The coefficient for "one park" will indicate the difference between the number of asthma cases (per 1,000 population) in tracts that have one park versus those that have no parks, and the coefficient for "multiple parks" will indicate the difference in the number of asthma cases (per 1,000 population) between tracts that have multiple parks versus those that have no parks.
reg_results2 <- lm(clean_PLACES$asthma ~ clean_PLACES$one_park + clean_PLACES$multiple_park)  #estimating a linear regression model (lm) and storing the results as "reg_results2") 
summary(reg_results2)  #view the results


# ************ MORE LATER ************ #