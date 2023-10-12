# R LAB 7 (NEW) - Multivariate Regression
# SOC 504, Crowder

# ****************
# INTRODUCTION
# In this lab we will run three regression models to ELABORATE on a basic focal association:
#   1) Model 1 will assess the basic bivariate association.
#   2) Model 2 will account for soe potential sources of spuriousness and redundancy (EXCLUSIONARY MODEL)
#   3) Model 3 is an example of an INCLUSIONARY MODEL in which we introduce a theoretically iplicated mechanism.

# At the end we will also look at one strategy for summarizing these regression results in a nice looking table.

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


# ****************
# In this example my focal relationship focuses on the effects of NEIGHBORHOOD POVERTY LEVELS on RATES OF ASTHNA IN THE NEIGHBORHOOD

# I have done all of the planning of my basic association, the exclusionary strategy, and the inclusionary strategy.
# This planning leads to the conclusion that I will need to take into consideration a bunch of variables for my analysis:
#   asthma = # people with asthma per 1,000 population -- DV
#   ppov13_17 = Proportion people w/ income past 12 months -- IV
#   no_physical_activity = % of people in the tract who do not get regular physical activity
#   curr_smoking = % of adults in the area who smoke
#   popden13_17 = population density (people per sq mile)
#   pownoc08_12 = proportion of housing units occupied by owners
#   count_tri_facilities_2018 = # of industrial polution sites within 0.5 miles of the tract
#   prop_park_area_tract = proportion of the tract that is covered by park space

summary(PLACES_NaNDA_sample$asthma)
summary(PLACES_NaNDA_sample$ppov13_17)
summary(PLACES_NaNDA_sample$no_physical_activity)
summary(PLACES_NaNDA_sample$curr_smoking)
summary(PLACES_NaNDA_sample$popden13_17)
summary(PLACES_NaNDA_sample$p18yr_08_12)
summary(PLACES_NaNDA_sample$pownoc08_12)
summary(PLACES_NaNDA_sample$count_tri_facilities_2018)
summary(PLACES_NaNDA_sample$prop_park_area_tract)

# --------
# EXCLUDING CASES WITH MISSING VALUES ON ANY OF THE VARIABLES I WILL USE IN THE ANALYSIS
# Looks like there are some cases with missing information on my key variables so I am going to create a clean version of the dataset that includes only those ROWS with non-missing (!is.na) on the variables included in my analysis.
# Note that here I am keeping ALL variables (COLUMNS)
clean_PLACES <- PLACES_NaNDA_sample[
  !is.na(PLACES_NaNDA_sample$asthma) &
    !is.na(PLACES_NaNDA_sample$ppov13_17) &
     !is.na(PLACES_NaNDA_sample$no_physical_activity) &
      !is.na(PLACES_NaNDA_sample$curr_smoking) &
       !is.na(PLACES_NaNDA_sample$popden13_17) &
        !is.na(PLACES_NaNDA_sample$p18yr_08_12) &
         !is.na(PLACES_NaNDA_sample$pownoc08_12) &
          !is.na(PLACES_NaNDA_sample$count_tri_facilities_2018) &
            !is.na(PLACES_NaNDA_sample$prop_park_area_tract),  ]


# --------
# CREATING NICER VERSIONS OF SOME OF MY VARIABLES TO FACILITATE INTERPRETATION AND ALIGN WITH MY THEORETICAL IDEAS

# converting proprtion in poverty to % in poverty
clean_PLACES$pct_poverty=clean_PLACES$ppov13_17*100

#creating a variable to measure level of rentership 
clean_PLACES$pct_renter=(1-clean_PLACES$pownoc08_12)*100

#rescaling the density variable to indicate number of THOUSANDS of people per square mile
clean_PLACES$popden1000=clean_PLACES$popden13_17/1000

#rescaling the parks variable to represent a % instead of a proportion (for ease of interpretation)
clean_PLACES$pct_park_area_tract=clean_PLACES$prop_park_area_tract*100



# ****************
# ESTIMATING THREE MODELS

# --------
# MODEL 1 - BIVARIATE MODEL.
# Asessing the basic bivariate association between my focal variables

MODEL1 <- lm(asthma ~ pct_poverty, data = clean_PLACES)
summary(MODEL1)


# --------
# MODEL 2 - EXCLUSIONARY MODEL.
# Adding variables representing potential sources of spuripousness/redundancy/suppression: 1) proportion of area residents who currently smoke; 2) proportion of residents who do not get regular physical activity; 3) level of population density; 4) concentration of young people, age 8-12, in the population; and 5) proportion of households occupied by renters (non-owners)

MODEL2 <- lm(asthma ~ pct_poverty + no_physical_activity +      curr_smoking + popden1000 + p18yr_08_12 + pct_renter, data = clean_PLACES)
summary(MODEL2)


# --------
# MODEL 3 - INCLUSIONARY MODEL.
# Adding two variables representing mechanisms implicated in my theoretical argument: 1) area of the tract that is defined by park space (prop_park_area_tract) and the number of industrial polluting sites in the tract (count_tri_facilities_2018)

MODEL3 <- lm(asthma ~ pct_poverty + no_physical_activity +      curr_smoking + popden1000 + p18yr_08_12 + pct_renter +         count_tri_facilities_2018 + pct_park_area_tract, data = clean_PLACES)
summary(MODEL3)


# --------
# Requesting a correlation matrix
# You might get some clues about funky results by looking at the basic associations between all pairs of variables.
cor(clean_PLACES[, c('asthma', 'pct_poverty', 'no_physical_activity', 'curr_smoking', 'popden1000', 'p18yr_08_12', 'pct_renter', 'count_tri_facilities_2018', 'pct_park_area_tract')])


# ****************
# Making a table of regression results

# loading a new package called "stargazer" that can be used to produce nice-looking tables summarizing regression results (also does descriptive statistics, etc.)
# Great overview of the stargazer package at: https://www.princeton.edu/~otorres/NiceOutputR.pdf

#install.packages("stargazer")
library(stargazer)

# USING STARGAZER TO GET A NICE REGRESSION TABLE
# in the example below I am reqesting a table in text format that is easy to modify. It savs straight to my working directory
stargazer(MODEL1,MODEL2,MODEL3, 
    type = "text", 
    out = "Regression.txt", title = "OLS regression results predicting tract asthma levels",
    covariate.labels=c("% in poverty", "% with no physical activity", "% smoking", "1000s of people / sq mile", "% age 8-12", "% renter occupied", "# industrial pollution sites", "% area covered by parks"))


# ****************
# USING STARGAZER TO GET A NICE TABLE OF DESCRIPTIVE STATISTICS

#creating a little dataframe that has just the final versions of my variables
final_vars <- clean_PLACES [ , c("asthma", "pct_poverty",
                "no_physical_activity", "curr_smoking",
                "popden1000", "p18yr_08_12", "pct_renter",
                "count_tri_facilities_2018",
                "pct_park_area_tract")]

# Requesting a descriptives table using just my final set of variables (make sure the order of your variable labels corresponds with the order of the variables in the data set)
stargazer(final_vars, type = "text", title="Descriptive statistics", digits=1, out="Descriptives.txt",
          covariate.labels=c("asthma cases per 1000 pop", "% in poverty", "% with no physical activity", "% smoking", "1000s of people / sq mile", "% age 8-12", "% renter occupied", "# industrial pollution sites", "% area covered by parks"))



# ************ MORE LATER ************ #