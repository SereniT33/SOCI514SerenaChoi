##################################################
# Final Assignment
#
# Green equity: does social vulnerability affect the distribution of green space? Case from Vancouver
#
##################################################

library(tidyverse)
library(plyr)
library(janitor)
park <- read.csv(file="~/Desktop/MCRP Terms/Y2022Winter/Soci514/SOCI514SerenaChoi/FinalAssignment/park.csv")
local <- read.csv(file="~/Desktop/MCRP Terms/Y2022Winter/Soci514/SOCI514SerenaChoi/FinalAssignment/local.csv", check.names=FALSE)

## cleaning data frame
park <- subset(park, select = -OBJECTID)
#local <- subset(local, select = -ID)
local <- local %>%
  filter(Variable!='') %>%
  filter(ID %in% c(1, 27, 53, 1904:1919, 3080:3081)) %>%
  unite(variable, c("ID", "Variable"))

local$variable <- recode(local$variable,"1_Total - Age groups and average age of the population - 100% data" = "total_pop")
  local$variable <-recode(local$variable,"27_Total - Age groups and average age of males - 100% data" ="male")
  local$variable <-recode(local$variable,"53_Total - Age groups and average age of females - 100% data" = "female")
  local$variable <-recode(local$variable,"1904_Total - Total income groups in 2015 for the population aged 15 years and over in private households - 25% sample data" = "total_income_group")
  local$variable <-recode(local$variable,"1905_Without total income"="without_income")
  local$variable <-recode(local$variable,"1906_With total income"="with_income")
  local$variable <-recode(local$variable,"1907_    Under $10,000 (including loss)"="under_10k")
  local$variable <-recode(local$variable,"1908_    $10,000 to $19,999"="10_19k")
  local$variable <-recode(local$variable,"1909_    $20,000 to $29,999"="20_29k")
  local$variable <-recode(local$variable,"1910_    $30,000 to $39,999"="30_39k")
  local$variable <-recode(local$variable,"1911_    $40,000 to $49,999"="40_49k")
  local$variable <-recode(local$variable,"1912_    $50,000 to $59,999"="50_59k")
  local$variable <-recode(local$variable,"1913_    $60,000 to $69,999"="60_69k")
  local$variable <-recode(local$variable,"1914_    $70,000 to $79,999"="70_79k")
  local$variable <-recode(local$variable,"1915_    $80,000 to $89,999"="80_89k")
  local$variable <-recode(local$variable,"1916_    $90,000 to $99,999"="90_99k")
  local$variable <-recode(local$variable,"1917_    $100,000 and over"="over_100k")
  local$variable <-recode(local$variable,"1918_      $100,000 to $149,999"="100_140k")
  local$variable <-recode(local$variable,"1919_      $150,000 and over"="over_150k")
  local$variable <-recode(local$variable,"3080_Total - Visible minority for the population in private households - 25% sample data" ="total_race")
  local$variable <-recode(local$variable,"3081_Total visible minority population"="race")

local <- local %>%
  pivot_longer(!variable)%>%
  pivot_wider(names_from = variable, values_from = value)


## joining
park_local <- join(park,local, by="name")

## creating and recoding columns
park_local %>%
  mutate(park_service = 1000 * SUM_area_ha /  )

## Research questions

## Definitions

1. According to the City of Vancouver, *green spaces* include parks, community gardens and greenways.




# First off, we need to tell R to look in the right
# directory.
#
# File -> Change dir
setwd("C:/Users/User/OneDrive/Teaching/Soci514/data/TXT")

dir()      # see if the data is in the working directory

oecd <- read.table("oecd.txt", header=T)  # load the data


# Let's take a look at what this object contains
names(oecd)

# Let's make the oecd the default data set
attach(oecd)

#Take a look at summary statistics:

summary(oecd)

#Recall that R uses an object-oriented framework; it uses objects to store information

object1 <- mean(decom)
object1

object2 <- min(decom)
object2

object3 <- max(decom)
object3


#-------------------------------------------------#
#          Bivariate Regression                   #
#-------------------------------------------------#

# Let's examine the relationship between gdp and decom:

plot(gdp, decom)

# To obtain a slope estimate summarizing the relationship between x and y,
# we use the following equation: cov(x,y)/var(x)

#Let's compute a slope, B1 for the relationship between gdp (x) and decom (y):
cov(gdp, decom)/var(gdp)
B1<-cov(gdp, decom)/var(gdp)

#Now let's compute the intercept, B0 = ybar - B1 * xbar

mean(decom) - (B1*mean(gdp))
B0 <- mean(decom) - (B1*mean(gdp))
B0

#Let's plot this linear relationship:
plot(gdp, decom)
abline(a=10.04, b=1.56, col="blue")  # a=intercept, b=slope


# We can also obtain slope and intercept estimates using a pre-packaged R function.
# The function for linear regression is lm(y ~ x1).
# See help(lm) for more details.  Here is an example:

lm(decom ~ gdp, data=oecd)

# Since we told R to use the oecd dataset for all operations
# we don't need to include the data command in the model statement.


lm(decom ~ gdp)


# Now let's take advantage of R's object-oriented framework

model1 <- lm(decom ~ gdp)  #estimate the regression, assign output to an object "model1"

# Take a look at the objects in model1.

model1
summary(model1)

# How do we interpret the slope for gdp?
# Evaluate the null hypothesis that B1 = 0.
# What can we conclude about the relationship between gdp and decom?

# We can also look at the residuals

residuals(model1)

resid<-residuals(model1)
plot(resid, type="n")
text(resid, labels=(country))

#What countries have big (absolute) residuals?
#What countries have small residuals?
#What does this mean in terms of model fit?

#-------------------------------------------------#
#             WRITING TO FILES                    #
#-------------------------------------------------#


# Regression results are much easier to interpret when they are
# formatted into nice tables.  This can be done with
# any spreadsheet program (e.g. MS Excel).
# You can always copy and paste your output directly
# from R, but it is often easier to write the results
# directly to a spreadsheet program for manipulation.

#Here's a function that creates a csv file from your
#regression output.
tocsv <- function(model, filename){
  model.summary <- summary(model)
  coefs   <- model.summary$coefficients
  write.csv(coefs, file=filename)
}

# Now lets use this function to save the results.
tocsv(model1, "Model1.csv")




