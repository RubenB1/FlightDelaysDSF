



# Part 2: Logistic regression ---------------------------------------------

#This first code allows us to show how was our first regression analysis with the GLM model. 
#In this code, we took into account all the covariates that we can fill with deep 
#information and in the short term which is not very realistic for all the flights. 
#For example, the weather delay we take into consideration in the GLM forecast in this code, 
#is not known before the flight occurred in the reality. This is something we 
#noticed after the first analysis (this one).
#The second code will show a more realistic forecast in virtue of the values 
#we already know before the flight itself (as we can do in the reality).

## Step 1: Preparation
graphics.off() # close all plots
rm(list=ls()) # remove all objects from the current workspace (R memory)

load(file = "data15_weather_holidays.RData")#load cleaned dataframe (df) into environment
## Step 2: load libraries

  #These are all the libraries that this code and the follow up codes need.
#install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(dplyr)
library(broom)
library(ggplot2)
#library(MASS)


## Step 3: Split up column 
dim(data) #rows and columns in df

#create additional column where all the delay values are summarised as yes(1)/no(0)
data$DELAY = ifelse(data$ARRIVAL_DELAY > 0, 1, 0)


#splitting df based on column value of Month:

  #Ensuring cleaned data
  data$MONTH=as.numeric(data$MONTH)
  data = na.omit(data)

train= filter(data, data$MONTH <= 11) #Training set November (11)
test= filter(data, data$MONTH >= 12) #Testing set December (12)


## Step 4: Logistic regression (why not linear? there are more than 2 covariates) with test df

#chose categorical variable for y:
p = ncol(train)

  #Working with convenient formats
  #We select only the columns that the analysis 
  #have shown to be relevant (see analysis analytical dashboard)

#Training set
train=select(train, MONTH
                   , DAY
                   , DESTINATION_AIRPORT
                   , ARR_HOUR
                   , ORIGIN_AIRPORT
                   , DEP_HOUR
                   , DAY_OF_WEEK
                   , DEPARTURE_DELAY
                   , TAXI_OUT
                   , DISTANCE
                   , TAXI_IN
                   , ARRIVAL_DELAY
                   , AIR_SYSTEM_DELAY
                   , SECURITY_DELAY
                   , AIRLINE_DELAY
                   , LATE_AIRCRAFT_DELAY
                   , WEATHER_DELAY
                   , DEP_wind_speed
                   , ARR_wind_speed
                   , Holidays
                   , DELAY)

#Testing set
test=select(test, MONTH
             , DAY
             , DESTINATION_AIRPORT
             , ARR_HOUR
             , ORIGIN_AIRPORT
             , DEP_HOUR
             , DAY_OF_WEEK
             , DEPARTURE_DELAY
             , TAXI_OUT
             , DISTANCE
             , TAXI_IN
             , ARRIVAL_DELAY
             , AIR_SYSTEM_DELAY
             , SECURITY_DELAY
             , AIRLINE_DELAY
             , LATE_AIRCRAFT_DELAY
             , WEATHER_DELAY
             , DEP_wind_speed
             , ARR_wind_speed
             , Holidays
             , DELAY)

#Controlling the Numeric format of some individual columns of the training set
train$DEP_HOUR=as.numeric(train$DEP_HOUR)
train$ARR_HOUR=as.numeric(train$ARR_HOUR)
train$DAY_OF_WEEK=as.numeric(train$DAY_OF_WEEK)
train$ORIGIN_AIRPORT=as.numeric(train$ORIGIN_AIRPORT)
train$DESTINATION_AIRPORT=as.numeric(train$DESTINATION_AIRPORT)


#categorical vector, names of variables Y
Y = train$DELAY 

#categorical vector, names of variables X

X = cbind(train$MONTH
          ,train$DAY
          ,train$DESTINATION_AIRPORT
          ,train$ARR_HOUR
          ,train$ORIGIN_AIRPORT
          ,train$DEP_HOUR
          ,train$DAY_OF_WEEK
          ,train$DEPARTURE_DELAY
          ,train$TAXI_OUT
          ,train$DISTANCE
          ,train$TAXI_IN
          ,train$ARRIVAL_DELAY
          ,train$AIR_SYSTEM_DELAY
          ,train$SECURITY_DELAY
          ,train$AIRLINE_DELAY
          ,train$LATE_AIRCRAFT_DELAY
          ,train$WEATHER_DELAY
          ,train$DEP_wind_speed
          ,train$ARR_wind_speed
          ,train$Holidays)

dim(X)
typeof(X)
typeof(Y)

  #Creating the GLM model
  
  model_glm = glm(Y~X,train, family = poisson(link = "log")) #converge/work, Gives type 2 errors, but less empirical error for a same critical value
  #model_glm = glm(Y~X,train, family = gaussian(link = "identity")) #converge/work, Gives type 1 errors, but more empirical error for a same criticla value

    #Working with the GLM model
    beta_glm = model_glm$coefficients # NA for linearly dependent vars
    dev = model_glm$deviance #visualizing the deviance
    beta_glm[is.na(beta_glm)] = 0 #setting NA values as 0 to be able to use the vector as multiplicator later on
    
    #Applying the sigmoid function
    etas_glm= 1 / (1 + exp(-(cbind(rep(1,nrow(train)), X) %*% beta_glm))) #which function?????????
    etas_glm=cbind(etas_glm,Y)
    
    #Testing with the critical value
    crit=0.519
    summary(etas_glm)
    
    # plot(etas_glm) #Plotting obtained forecast
    # plot(beta_glm) #Plotting coefficients
    # plot((cbind(rep(1,nrow(train)), X) %*% beta_glm)) #Plotting (trained data*betas)
    
    TypeI_errors_glmall = length(which((etas_glm[,1] > crit) & (etas_glm[,2] != 1))) #false positives
    TypeII_errors_glmall = length(which((etas_glm[,1] <= crit) & (etas_glm[,2] == 1))) #false negatives
    Empirical_error_glmall = (TypeI_errors_glmall + TypeII_errors_glmall) / nrow(train)
    beta_forecast = beta_glm



  ##TESTING PART
    #Including the intercept in test

  Xtest=select(test, MONTH
              , DAY
              , DESTINATION_AIRPORT
              , ARR_HOUR
              , ORIGIN_AIRPORT
              , DEP_HOUR
              , DAY_OF_WEEK
              , DEPARTURE_DELAY
              , TAXI_OUT
              , DISTANCE
              , TAXI_IN
              , ARRIVAL_DELAY
              , AIR_SYSTEM_DELAY
              , SECURITY_DELAY
              , AIRLINE_DELAY
              , LATE_AIRCRAFT_DELAY
              , WEATHER_DELAY
              , DEP_wind_speed
              , ARR_wind_speed
              , Holidays)
  
  Xtest=cbind(rep(1), Xtest)

# Working with the good formats (matrix or vector)
  Xtest$ORIGIN_AIRPORT=as.numeric(Xtest$ORIGIN_AIRPORT)
  Xtest$DESTINATION_AIRPORT=as.numeric(Xtest$DESTINATION_AIRPORT)
  Xtest$ARR_HOUR=as.numeric(Xtest$ARR_HOUR)
  Xtest$DEP_HOUR=as.numeric(Xtest$DEP_HOUR)
  Xtest$`rep(1)`=as.numeric(Xtest$`rep(1)`)
  Xtest$MONTH=as.numeric(Xtest$MONTH)

  ## Testing the model with the training set
dim(Xtest)
dim(as.matrix(beta_forecast))

#Multiplying the Testing set by the weights found in the training process.
matrix_multi = function(A, B){
  C = matrix(0, dim(A)[1], dim(B)[2], TRUE)
  for (d in 1:dim(A)[1]){
    for (e in 1:dim(B)[2]){
      for (f in 1:dim(A)[2]){
        C[d,e] = C[d,e] + A[d,f] * B[f,e]
      }
    }
  }
  return (C)
}


#Application of the multiplication function
Y_forecast_test= matrix_multi(Xtest,as.matrix(beta_forecast)) 


# Computing the errors for the TESTING phase

etas_glmtest=cbind(Y_forecast_test,test$DELAY)

TypeI_errors_glmalltest = length(which((etas_glmtest[,1] > crit) & (etas_glmtest[,2] != 1)))
TypeII_errors_glmalltest = length(which((etas_glmtest[,1] <= 1-crit) & (etas_glmtest[,2] == 1)))
Empirical_error_glmalltest = (TypeI_errors_glmalltest + TypeII_errors_glmalltest) / nrow(test)

  # plot(etas_glmtest)

#Analytical overview of the model and characteristics of weights, data sets, correlation...
  # summary(model_glm)
    # summary(Y_forecast_test)
    # summary(lm(X~Y))
    # lm(X~Y)


# Here, (GLM all) we can notice that the empirical error from the training set is 28.29% and the one from
#the testing set is about 43.42% (rounded) - For a critical value of 0.519 in the model (optimal).
#The critical value is found in the second code by a for-loop, it makes the 2 codes 
#comparable at a same critical value which is calculated as optimal for the second code.
