rm(list = ls()) # clear memory
library(pacman)
p_load(leaflet, ggplot2, ggthemes, viridis, dplyr, magrittr, knitr,pagedown,tibble,readxl)
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/public-repository-1/week_5")
tesla <- read_xlsx("tesla_x.xlsx")

# Question 1
q1 <-lm(price ~ odometer, tesla)
summary(q1)

# Question 2
ggplot(tesla, aes(x=odometer, y=price)) + 
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=lm, color="#FC9313")+
  labs(title="Vehicle Price by Odometer",
       x="Odometer", y = "Price")+
  theme_classic()  

# Question 3
tesla$res <- q1$resid
ggplot(tesla, aes(x=odometer, y=res)) +
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=lm, se=FALSE, color="#FC9313")+
  labs(title="Residual Plot",
       x="Odometer", y = "Residual")+
  theme_classic()

# Question 4
tesla$lprice <-log(tesla$price)
tesla$lodo <-log(tesla$odometer)

m1 <- lm(lprice ~ odometer, tesla)
m2 <- lm(price ~ lodo, tesla)
m3 <- lm(lprice ~ lodo, tesla)

tesla$m1res <- m1$resid
tesla$m2res <- m2$resid
tesla$m3res <- m3$resid

m1p <- ggplot(tesla, aes(x=odometer, y=m1res)) +
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=lm, se=FALSE, color="#FC9313")+
  labs(title="Model 1 Residual Plot",
       x="Odometer", y = "Residual")+
  theme_classic()

m2p <-  ggplot(tesla, aes(x=odometer, y=m2res)) +
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=lm, se=FALSE, color="#FC9313")+
  labs(title="Model 2 Residual Plot",
       x="Odometer", y = "Residual")+
  theme_classic()

m3p <- ggplot(tesla, aes(x=odometer, y=m3res)) +
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=lm, se=FALSE, color="#FC9313")+
  labs(title="Model 3 Residual Plot",
       x="Odometer", y = "Residual")+
  theme_classic()

print(m1p)
print(m2p)
print(m3p)

