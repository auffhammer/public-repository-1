# Let's use the Titanic dataset to see what factors predict survival. 
# Yes it's grim, but I am feeling grim. 
rm(list=ls())
library(pacman)
p_load(tidyverse,Amelia,yardstick)
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/public-repository-1/week_13")
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))

# Nice new trick - See what observations have missing data. 

#Gives a table of what is missing
sapply(training.data.raw,function(x) sum(is.na(x)))

# Max is a visual learner
missmap(training.data.raw, main = "Missing values vs observed")

# Let's drop class, since there are too many missing values. Also Passenger ID is useless.
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))
data <-  na.omit(data)
# Let's estimate our model 
model <- glm(Survived ~.,family="binomial",data=data)
summary(model)

# Let's check how we did? 
data$p_hat = predict(model, type = "response")
data$y_hat = as.numeric(data$p_hat>=0.5)
data$correct <- data$y_hat == data$Survived

data$TN <- data$y_hat==0 & data$Survived==0
data$FN <- data$y_hat==0 & data$Survived==1
data$FP <- data$y_hat==1 & data$Survived==0
data$TP <- data$y_hat==1 & data$Survived==1

sum(data$TP)
sum(data$FP)
sum(data$TN)
sum(data$FN)