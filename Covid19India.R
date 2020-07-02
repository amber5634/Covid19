#load packages 
library(tidyverse)
library(gridExtra)
library(lubridate)
library(RCurl)
library(magrittr)
#importing elements 
df <- read.csv("M://Covid19//covid_19_india.csv")
#Starting few elements from the start
head(df)
#Structue of data
str(df)
#Summary
summary(df)
#plot
#Confirmed Cases
plot(df$Date,df$Confirmed,main='Confirmed Cases in India',type='b')
df$DiffConf <- 0
df$DiffConf[2:nrow(df)] <- diff(df$Confirmed)
#Death Cases
plot(df$Date, df$Deaths, main='Deaths due to Corona in India', type='b')
#Cured/Recovery Cases
plot(df$Date, df$Cured, main='Cured cases in India', type='b')
df$RecRate <- df$Cured / df$Confirmed
#Growth Rate
plot(df$Date, df$RecRate, pch=16, col='darkgreen', type='b', ylab='Recovery Ratio', main='Recovery Ratio')
