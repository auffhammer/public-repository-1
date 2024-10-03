rm(list = ls()) # clear memory
setwd("/Users/auffhammer/tmp/COMPSS212_F24/slides/Lecture_06")
options(htmltools.dir.version = FALSE)
library(pacman)
p_load(lfe, whitestrap, parallel,GGally,broom,kableExtra, estimatr,plm,leaflet, gganimate, ggplot2, ggthemes, viridis, dplyr, magrittr, knitr,pagedown,tibble,latex2exp,MASS,stargazer)
load("cps2.rda")

# Step 1: Generate Correlogram between wage, education and experience (use command ggpairs from GGally package)


# Step 2: Run SRM Regression of ln(wage) on education and generate regression output. Record R^2 and R^2 adj.


# Step 3: Run MRM Regression of ln(wage) on education and experience and generate regression output. Record R^2 and R^2 adj.What ahppened to the two measures of fit?


# Step 4: Conduct an F-Test of the null that all slope coefficients are zero. Do you reject the null?

# Step 5: Generate the predicted values from Step 3 (Hint: Subtract residuals from ln(wage). Plot the residuals agains the predicted values. What do you see?

# Step 6: Conduct white test for heteroskedasticity. What do you learn? (use the white_test command)

# Step 7: Run the model with the White robust standard errors... Did your standard errors changed compared to model in Step 3. I would use the felm command as the slides say. 

# Step 8: Lean back and relax. 
