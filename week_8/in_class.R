# In Class Week 8
rm(list = ls()) # clear memory
# Put your own working Directory Here
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/public-repository-1/week_8")
set.seed(22092008)
options(htmltools.dir.version = FALSE)
library(pacman)
p_load(car,sandwich,FSM,skedastic,modelsummary, summarytools,lfe, whitestrap, parallel,GGally,broom,kableExtra, estimatr,plm,leaflet, gganimate, ggplot2, ggthemes, viridis, dplyr,plyr, magrittr, knitr,pagedown,tibble,latex2exp,MASS,stargazer)


# First exercise- Multicollinearity

# In this exercise we will generate some data and amp up the multicollinearity between right hand side variables,
# without changing the residual or the model! We are interested in studying our standard errors. 

# Step 1: Let's generate some data for a MRM with four right hand side variables. 

n =100

# Make some mean for my rhs variables
mu <- matrix(c(2, 3, 7, 1, 0),ncol=1)

# Make the correlation matrix
sigma = matrix(c(1,0.0,0,0,0,0.0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1000),ncol=5)

# Create some data
x = as.data.frame(mvrnorm(n=100,mu, sigma))
# Ues these our lines are ugly. 
names(x)[1] <- "x1"
names(x)[2] <- "x2"
names(x)[3] <- "x3"
names(x)[4] <- "x4"
names(x)[5] <- "err"
# Generate some true outcomes
x$y = 7 - 0.2*x$x1 + 0.5*x$x2 - 0.3*x$x3  + 0.2*x$x4 + x$err

# Run a regression of y on the xs. 
mod_1 = lm(y ~ 1+ x1 + x2 + x3 + x4, data=x)
summary(mod_1)

# Calculate The VIFs for your model. What do you see?
vif_values <- vif(mod_1)
vif_values

# Task 1: Now let us mess with correlation between x1 and x2. Change the covariance between x1 and x2 to 0.8
# You do that by changing the covariance matrix sigma. The second element and sixth element is what you need to change to 0.8. 
# Try it again for 0.95. Then try it again for 0.99. What do you see about the VIFs and the coefficients on x1 and x2?
# If you are really bored you can set this up as a Monte Carlo and run it many times. 


#### Second Exercise: Endogeneity
# We can use the same setup as before to demonstrate that OVB is an issue. Here will use a monte carlo type setup though
rm(list = ls()) # clear memory
n =100
numloop = 1000
g <- integer(numloop) # vector to hold sample mean for each iteration
# Make some mean for my rhs variables
mu <- matrix(c(2, 3, 7, 1, 0),ncol=1)

# Make the correlation matrix
sigma = matrix(c(1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1000),ncol=5)

for(i in 1:numloop) {
# Create some data
x = as.data.frame(mvrnorm(n=100,mu, sigma))
# Ues these our lines are ugly. 
names(x)[1] <- "x1"
names(x)[2] <- "x2"
names(x)[3] <- "x3"
names(x)[4] <- "x4"
names(x)[5] <- "err"
# Generate some true outcomes with no problems anywhere. 
x$y = 7 - 0.2*x$x1 + 0.5*x$x2 - 0.3*x$x3  + 0.2*x$x4 + x$err
# Run a regression of y on the xs. 
mod_1 = lm(y ~ 1+ x1 + x2 + x3 + x4, data=x)
g[i] <-summary(mod_1)$coefficients[4]
}

g <- as.data.frame(g)
ggplot(g, aes(x=g)) +
  geom_histogram(alpha=0.5, fill="#FC9313", color="#002676", position="identity", bins=25)+
  geom_vline(aes(xintercept=mean(g)), color="red", linetype="dashed")+
  labs(title="Coefficients on x3 (Truth = 0.3)",x="Coefficient Estimate", y = "Count")+
  theme_classic()+
  theme_classic(base_size = 20) 

# Task 1: Leave data the same, but run again with x4 omitted from the model. Problems?
# Task 2: Change the covariance between x3 and x4 to 0.5. [Elements 14 and 18 in the covariance matrix]
# and run the model from task 1] again. What happens to your distribution?
# Task 3. Change the covariance to -0.4. What happens to your distribution.

# Do you believe me now?????
