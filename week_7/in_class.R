# In Class Week 7
rm(list = ls()) # clear memory
# Put your own working Directory Here
setwd("/Users/auffhammer/tmp/COMPSS212_F24/slides/Lecture_07")
options(htmltools.dir.version = FALSE)
library(pacman)
p_load(sandwich,FSM,skedastic,modelsummary, summarytools,lfe, whitestrap, parallel,GGally,broom,kableExtra, estimatr,plm,leaflet, gganimate, ggplot2, ggthemes, viridis, dplyr,plyr, magrittr, knitr,pagedown,tibble,latex2exp,MASS,stargazer)

#1. Read in data "class_7.csv"
#2. Regress births on income. 
#3. Plot your residuals against income
#4. Plot your residuals against time
#5. Test for AR(1) errors by using the residuals from your regression (regress residual on its lag)
#6. Follow the steps in the slides on calculating the FGLS estimator. 
# - Estimate the original (untransformed) model; save residuals.
# - Estimate ρ Regress residuals on their lags (no intercept).
# - Estimate the transformed model (look at formula on slide 29 )
