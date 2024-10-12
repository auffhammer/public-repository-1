# In Class Week 7 - Answer Key
rm(list = ls()) # clear memory
# Put your own working Directory Here
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/public-repository-1/week_7")
options(htmltools.dir.version = FALSE)
library(pacman)
p_load(sandwich,FSM,skedastic,modelsummary, summarytools,lfe, whitestrap, parallel,GGally,broom,kableExtra, estimatr,plm,leaflet, gganimate, ggplot2, ggthemes, viridis, dplyr,plyr, magrittr, knitr,pagedown,tibble,latex2exp,MASS,stargazer)

#1. Read in data "class_7.csv"
class_7 <-read.csv("class_7.csv")

#2. Regress births on income. 
mod_1 <- lm(births ~ income, data=class_7)
class_7$res <-mod_1$resid
summary(mod_1)

#3. Plot your residuals against income
ggplot(class_7,aes(x=income, y=res),) +
  geom_point(alpha=0.8, shape=16, fill="#002676", color="#002676", size=1)+
  geom_line(color="#FC9313", size=0.1)+
  labs(title="Disturbances against Income",
       x="Income", y = "Disturbance")+
  theme_classic(base_size = 20) 

# -> Not much to see here. At least not to me. 

#4. Plot your residuals against time
ggplot(class_7,aes(x=time, y=res),) +
  geom_point(alpha=0.8, shape=16, fill="#002676", color="#002676", size=1)+
  geom_line(color="#FC9313", size=0.1)+
  labs(title="Disturbances against Time",
       x="Time", y = "Disturbance")+
  theme_classic(base_size = 20) 

# Wowza! That surely looks correlated across time!

#5. Test for AR(1) errors by using the residuals from your regression (regress residual on its lag)
mod_3 <- lm(class_7$res~ -1 +  lag(class_7$res), data=class_7)
summary(mod_3)

# You strongly reject the null of no serial correlation. t= 55.03! Positive serial correlations as rho_hat>0


#6. Follow the steps in the slides on calculating the FGLS estimator. 
# Do the transformation. The third on transforms the intercept. 

class_7$ytrans <- class_7$births - mod_3$coefficients*lag(class_7$births)
class_7$xtrans <- class_7$income - mod_3$coefficients*lag(class_7$income)
class_7$one <- 1-mod_3$coefficients

# Run model on transformed data. 
mod_5 <- lm(ytrans ~ -1 +one +xtrans, data=class_7)
summary(mod_5)
