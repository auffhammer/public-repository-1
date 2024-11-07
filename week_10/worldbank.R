# A little panel data exercise
# Explain variation in life expectancy across countries.
rm(list = ls()) # clear memory
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/private-repository1/week10")
library(ggplot2,lfe)
set.seed(22092008) # set random number generator seed
wb <- read.csv("life.csv")
# Start with a simple OLS regression of Life.Expectancy on Schooling
mod_1 <- felm(Life.expectancy ~ Schooling, data = wb)
summary(mod_1)

# Control for Country Fixed Effects
mod_2 <- felm(Life.expectancy ~ Schooling| Country, data = wb)
summary(mod_2)

# Challenge - demean by country and run a regular regression without fixed effects.
# Do you get the same results?

# Control for Country & Year Fixed Effects
mod_3 <- felm(Life.expectancy ~ Schooling| Country + Year, data = wb)
summary(mod_3)

# Now repeat and run a model with two way fixed effects, but control GDP and Total Health Expenditure What happens? 
# What happens?
mod_4 <- felm(Life.expectancy ~ Schooling + GDP + Total.expenditure| Country + Year, data = wb)
summary(mod_4)

# Take a look at your GDP data? Anything funky?
# Any variables in here that should be included in the regression? What happens if you include them?





