# In Class Week 9
rm(list = ls()) # clear memory
# Put your own working Directory Here
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/public-repository-1/week_8")
set.seed(22092008)
options(htmltools.dir.version = FALSE)
library(pacman)
p_load(car,sandwich,FSM,skedastic,modelsummary, summarytools,lfe, parallel,GGally,broom,kableExtra, estimatr,plm,leaflet, gganimate, ggplot2, ggthemes, viridis, dplyr,plyr, magrittr, knitr,pagedown,tibble,latex2exp,MASS,stargazer)

N  <- 1000
# Some playing around with IV
# Here we do simulated data. Problem Set 3 has real data. 

# In this exercise we will generate some data for an endogenous variable and generate 
# Some valid instruments. You can play with this setup to study a number of things.

# Step 1: Let's generate some data for an IV setup that has one
# one problem rhs variable (y2) and one omitted rhs variable (y3), two exognous included variables, 
# two instruments and a well behaved error terms

# Make some mean for my variables
mu <- matrix(c(2, 3, 7, 1, 5, 2, 0),ncol=1)

# Make the correlation matrix
sigma = matrix(c(
  1,0.7,0,0,0.5,0.5,0,
  0.7,1,0,0,0,0,0,
  0,0,1,0,0,0,0,
  0,0,0,1,0,0,0,
  0.5,0,0,0,1,0,0,
  0.5,0,0,0,0,1,0,
  0,0,0,0,0,0,1),nrow=7)

# Create some data
x = as.data.frame(mvrnorm(n=N,mu, sigma))
# Ues these our lines are ugly. 
names(x)[1] <- "y2"
names(x)[2] <- "y3"
names(x)[3] <- "x1"
names(x)[4] <- "x2"
names(x)[5] <- "z1"
names(x)[6] <- "z2"
names(x)[7] <- "err"
# Generate some true outcomes
x$y1 = 3 - 0.8*x$y2 + 0.7**x$y3 + 0.2*x$x1 - 0.5*x$x2 + x$err


# Run a regression of y1 on  y2 and the x1 and x2 (leave out y3). 
mod_1 = lm(y1 ~ 1+ y2 + x1 + x2, data=x)
summary(mod_1)
# Which direction should the bias go? 

# IV by hand, with just z1. Run a regression of y2 on z1 and the xs. save the predicted values. 

# Now run a regression of y1 on the predicted values you just calculated and x1 and x2. 
# What do you see?


# Now use the command in the slides to do this canned.
iv_robust(y1  ~ y2 + x1 + x2| x1 + x2 + z1, data = x, se_type ="classical")

