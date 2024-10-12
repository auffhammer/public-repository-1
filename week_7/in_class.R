# In Class Week 7
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

#3. Plot your residuals against income
plot(class_7$income,class_7$res)
#4. Plot your residuals against time
plot(class_7$time,class_7$res)
#5. Test for AR(1) errors by using the residuals from your regression (regress residual on its lag)
mod_3 <- lm(class_7$res~ -1 +  lag(class_7$res), data=class_7)
#6. Follow the steps in the slides on calculating the FGLS estimator. 
class_7$ytrans <- class_7$births - mod_3$coefficients*lag(class_7$births)
class_7$xtrans <- class_7$income - mod_3$coefficients*lag(class_7$income)
class_7$one <- 1-mod_3$coefficients

mod_5 <- lm(ytrans ~ -1 +one +xtrans, data=class_7)

# - Estimate the original (untransformed) model; save residuals.
# - Estimate ρ Regress residuals on their lags (no intercept).
# - Estimate the transformed model (look at formula on slide 29 )



rm(list = ls()) # clear memory
setwd("/Users/auffhammer/tmp/COMPSS212_F24/slides/Lecture_07")
read.csv("ar_df.csv")
options(htmltools.dir.version = FALSE)
library(pacman)
p_load(FSM, skedastic, modelsummary, summarytools,lfe, whitestrap, parallel,GGally,broom,kableExtra, estimatr,plm,leaflet, gganimate, ggplot2, ggthemes, viridis, dplyr,plyr, magrittr, knitr,pagedown,tibble,latex2exp,MASS,stargazer)
T <- 5e2
# Rho
rho <- 0.95
# Set seed and starting point
set.seed(1234)
start <- rnorm(1)
# Generate the data
ar_df <- tibble(
  t = 1:T,
  x = runif(T, min = 0, max = 1),
  e = rnorm(T, mean = 0, sd = 2),
  u = NA
)
for (x in 1:T) {
  if (x == 1) {
    ar_df$u[x] <- rho * start + ar_df$e[x]
  } else {
    ar_df$u[x] <- rho * ar_df$u[x-1] + ar_df$e[x]
  }
}
ar_df %<>% mutate(y = 1 + 3 * x + u)
ggplot(data = ar_df,
       aes(t, u)
) +
  geom_line(color = "black", size = 0.35)+
  geom_point(color = "blue", size = 2.25)

class_7 <- ar_df[c(1,2, 5)]
colnames(class_7)[2] <- "income"
colnames(class_7)[3] <- "births"
colnames(class_7)[1] <- "time"
write.csv(class_7, "class_7.csv")
