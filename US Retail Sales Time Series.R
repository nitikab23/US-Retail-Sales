#Week 1 Assignment - EDA

#Calling the libraries
library(tidyr)
library(ggplot2)
library(caret)
library(rsq)
library(ncvreg)
library(bigmemory)
library(biglasso)
library(bigmemory.sri)
library(Matrix)
library(lars)
library(glmnet)
library(e1071)
library(ggcorrplot)
library(corrplot)
library(dummies)
library(dplyr)
library(polycor)
library(psych)
library(nlme)

#Setting the working directory
setwd("D:\\Me\\NEU\\Kasun - Data Mining\\Data\\")
data <- read.csv("crime.csv", header = TRUE)

#Removing unnecessary columns
data <- data[,c(2:3,8,11:14)]

#Checking the structure of Data
str(data)

#Changing the str
data$OFFENSE_CODE <- as.factor(data$OFFENSE_CODE)
data$HOUR <- as.factor(data$HOUR)
data$STREET <- as.character(data$STREET)

#Delimiting Date and Time
Date <- data.frame(do.call('rbind', strsplit(as.character(data$OCCURRED_ON_DATE),' ',fixed=TRUE)))

#Adding Date to data
data$OCCURRED_ON_DATE <- as.Date(Date$X1)

#Checking the centre and dispersion values
describe(data)
summary(data)

#Making 2 data sets 
# 1 Data without Street Names
Crime_Data <- data[,c(1:6)]
Crime_Data_Street <- na.omit(data)

NROW(Crime_Data)
NROW(Crime_Data_Street)

