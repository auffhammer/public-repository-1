# In Class Week 9
# It is fixed now.
# This had to do with the random number generator. 
# I have set this up as a proper Monte Carlo, where this is run many times and
# then shows you the sampling distribution of the point estiamtes 2SLS gives you.

# Clear memory
rm(list = ls()) # clear memory

# Put your own working Directory Here
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/public-repository-1/week_9")
set.seed(22092008)
options(htmltools.dir.version = FALSE)
library(pacman)
p_load(estimatr,ggplot2,MASS)

# Sample size & number of loops & placeholders
N  <- 1000
numloop <- 1000
g1 <- integer(numloop) 
g2 <- integer(numloop) 
g3 <- integer(numloop) 
g4 <- integer(numloop)

# In this exercise we will generate some data for an endogenous variable and generate 
# Some valid instruments. You can play with this setup to study a number of things.

# Step 1: Let's generate some data for an IV setup that has one
# one problem rhs variable (y2) and one omitted rhs variable (y3), two exognous included variables, 
# two instruments and a well behaved error terms

# Make some mean for my variables
mu <- matrix(c(0,0,0,0,0,0,0),ncol=1)

# Make the covariance matrix
sigma = matrix(c(
  1,0.7,0,0,0.5,0.5,0,
  0.7,1,0,0,0,0,0,
  0,0,1,0,0,0,0,
  0,0,0,1,0,0,0,
  0.5,0,0,0,1,0,0,
  0.5,0,0,0,0,1,0,
  0,0,0,0,0,0,1),nrow=7)

for(i in 1:numloop) {
# Create some data
x = as.data.frame(mvrnorm(n=N,mu, sigma))
# Name some things
names(x)[1] <- "y2"
names(x)[2] <- "y3"
names(x)[3] <- "x1"
names(x)[4] <- "x2"
names(x)[5] <- "z1"
names(x)[6] <- "z2"
names(x)[7] <- "err"
# # Generate some true outcomes
x$y1 = 3 - 0.8*x$y2 - 0.7*x$y3 + 0.2*x$x1 - 0.5*x$x2 + x$err

# Run a regression of y1 on  y2 and the x1 and x2 (leave out y3). 
mod_1 = lm(y1 ~ 1+ y2 + x1 + x2, data=x)
g1[i] <- mod_1$coefficients[2]

# IV by hand, with just z1. Run a regression of y2 on z1 and the xs. save the predicted values. 
stage_1 <- lm(y2 ~ 1 + x1 + x2 + z1, data = x)
x$y_hat <- stage_1$fitted.values
# Now run a regression of y1 on the predicted values you just calculated and x1 and x2. 
# What do you see?
stage_2 <- lm(y1 ~ 1+ y_hat + x1 + x2, data=x)
out <-summary(stage_2)
g3[i] <- out$coefficients[2,2]



# Now use the command in the slides to do this canned.
mod_2 <- iv_robust(y1  ~ y2 + x1 + x2| x1 + x2 + z1, data = x, se_type ="classical")
g2[i] <- mod_2$coefficients[2]
g4[i] <- mod_2$std.error[2]

}

# Make object of coefficients
g1 <-data.frame(g1)
names(g1)[1] <- "Coefficient"
g2 <-data.frame(g2)
names(g2)[1] <- "Coefficient"
g1$model <- 'ols'
g2$model <- 'iv'
coefficients <- rbind(g1, g2)

# Dashed line will show you the true value of the sampling distribution. 
ggplot(coefficients, aes(x=Coefficient, color=model, fill=model)) +
  geom_vline(xintercept=-0.8, linetype="dashed") +
  geom_histogram(alpha=0.3, position="identity", bins=50) +
  labs(title="IV vs. OLS",x="Coefficients", y = "Count") +
  theme_classic(base_size = 15)

# let's compare standard errors from doing this by hand, versus auto. 
# Make object of coefficients
g3 <-data.frame(g3)
names(g3)[1] <- "SE"
g4 <-data.frame(g4)
names(g4)[1] <- "SE"
g3$model <- 'by hand'
g4$model <- 'auto'
ses <- rbind(g3, g4)

# Dashed line will show you the true value of the sampling distribution. 
ggplot(ses, aes(x=SE, color=model, fill=model)) +
  geom_histogram(alpha=0.3, position="identity", bins=50) +
  labs(title="IV vs. OLS SEs",x="SEs", y = "Count") +
  theme_classic(base_size = 15)
