rm(list = ls()) # clear memory
setwd("/Users/auffhammer/tmp/COMPSS212_F24/slides/Lecture_06")
options(htmltools.dir.version = FALSE)
library(pacman)
p_load(lfe, whitestrap, parallel,GGally,broom,kableExtra, estimatr,plm,leaflet, gganimate, ggplot2, ggthemes, viridis, dplyr, magrittr, knitr,pagedown,tibble,latex2exp,MASS,stargazer)
load("cps2.rda")

# Step 1: Generate Correlogram between wage, education and experience (use command ggpairs from GGally package)
cps <- cps2[c(1:3)]
ggpairs(cps)

# Step 2: Run SRM Regression of ln(wage) on education and generate regression output. Record R^2 and R^2 adj.
cps$lwage <- log(cps$wage)
srm <-lm(lwage ~ educ, data=cps)
summary(srm)

# Step 3: Run MRM Regression of ln(wage) on education and experience and generate regression output. Record R^2 and R^2 adj.What ahppened to the two measures of fit?
cps$lwage <- log(cps$wage)
mrm <-lm(lwage ~ educ + exper, data=cps)
summary(mrm)

# Step 4: Condcut an F-Test of the null that all slope coefficients are zero. Do you reject the null?

# You just look at the output. The pvalue on the F-Statistic is extremely close to zero. You reject the null that your model has no explanatory power over a simple average. 

# Step 5: Generate the predicted values from Step 3 (Hint: Subtract residuals from ln(wage). Plot the residuals agains the predicted values. What do you see?
cps$res <- mrm$resid
cps$yhat <- cps$lwage - cps$res
ggplot(cps, aes(x=yhat, y=res)) +
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=lm, se=FALSE, color="#FC9313")+
  labs(title="Residual Plot",
       x="Predicted Values", y = "Residual")+
  theme_classic(base_size = 20) 

#  With my little eye, I think I spot some heteroskedastic Residuals! But I am not sure. So I woud like to test.


# Step 6: Conduct white test for heteroskedasticity. What do you learn? (use the white_test command)

hal <- white_test(mrm)
hal

# The pvalue is pretty small. Less than 5%. I would like to adjust for it. 

# Step 7: Run the model with the White robust standard errors... Did your standard errors changed compared to model in Step 3. I would use the felm command as the slides say. 
test_reg <- felm(lwage ~ educ + exper, data=cps)
test_nhet_out <- summary(test_reg, robust = F) %>% capture.output()
test_nhet_out[10:13] %>% paste0("\n") %>% cat()

test_het_out <- summary(test_reg, robust = T) %>% capture.output()
test_het_out[10:13] %>% paste0("\n") %>% cat()

# This did not change the standard errors a lot here, which is the case often. 


# Step 8: Lean back and relax. 

# this on is on you. 