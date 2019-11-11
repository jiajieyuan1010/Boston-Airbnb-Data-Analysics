#data analytics competition
rm(list=ls()); gc()
library(tidyverse) 
library(knitr)
library(datasets)
library(data.table)
library(ggplot2)
library(sandwich)
library(lmtest)
library(zoo)
library(MASS)
library(car)
#read the data

listdetail <- read.csv('/Users/apple/Desktop/data_competition/listings_details.csv',header = TRUE)
head(listdetail)
glimpse(listdetail)
listdetail=as.data.table(listdetail)
summary(listdetail)

list_summary <- read.csv('/Users/apple/Desktop/data_competition/listings_summary.csv',header = TRUE)
listsummary=as.data.table(list_summary)
head(listsummary)

review_summary <- read.csv('/Users/apple/Desktop/data_competition/reviews_summary.csv',header = TRUE)
head(review_summary)
reviewsummary=as.data.table(review_summary)
##

## clean data
listdetail$price <- substring(as.character(listdetail$price),2)
listdetail$price <- as.numeric(sub(",","",listdetail$price))
listdetail$cleaning_fee <- substring(as.character(listdetail$cleaning_fee),2)
listdetail$cleaning_fee <- as.numeric(sub(",","",listdetail$cleaning_fee))
listdetail$security_deposit <- substring(as.character(listdetail$security_deposit),2)
listdetail$security_deposit <- as.numeric(sub(",","",listdetail$security_deposit))
listdetail$extra_people <- substring(as.character(listdetail$extra_people),2)
listdetail$extra_people <- as.numeric(sub(",","",listdetail$extra_people))
listdetail$host_is_superhost  <- ifelse(listdetail$host_is_superhost =='t',1,0)
listdetail$instant_bookable   <- ifelse(listdetail$instant_bookable  =='t',1,0)
listdetail$host_identity_verified    <- ifelse(listdetail$host_identity_verified   =='t',1,0)
glimpse(listdetail)
listdetail[,unique(room_type)]   #Entire home/apt Private room    Shared room     Hotel room    
listdetail[,unique(neighbourhood_cleansed)] 

## run regression

reg <- lm(log(price) ~ -1+factor(room_type)+neighbourhood_cleansed+bedrooms+number_of_reviews+accommodates
          +beds+guests_included*bedrooms+availability_365+square_feet+host_is_superhost+security_deposit+instant_bookable,
          data = listdetail)
summary(reg,robust = "HC1")

coeftest(reg,vcov = vcovHC(reg,type = 'HC1'))

## plot
ggplot(data=reg,aes(reg$residuals))+geom_histogram( fill="skyblue") + 
  labs(title="Histogram for Residuals", x="Residuals", y="Count")
library(modelr)

plot(reg)

hist(reg$residuals)
par(mfrow = c(2, 2))

plot(density(resid))
qqnorm(resid) 

