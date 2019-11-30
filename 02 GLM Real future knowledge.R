
# Part 1: Analysis --------------------------------------------------------


# Part 2: Logistic regression ---------------------------------------------
## Step 1: Preparation
graphics.off() # close all plots
rm(list=ls()) # remove all objects from the current workspace (R memory)

load(file = "data15_weather_holidays.RData")#load cleaned dataframe (df) into environment
## Step 2:
#install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(dplyr)
library(broom)
library(ggplot2)



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

#How many flights were delayed in November and December
  delayed_summary= as.data.frame(table(data$DELAY))
  # delayed_11= as.data.frame(table(train$DELAY))
  # delayed_12= as.data.frame(table(test$DELAY))

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
             , TAXI_OUT
             , DISTANCE
             , TAXI_IN
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
             , TAXI_OUT
             , DISTANCE
             , TAXI_IN
             , DEP_wind_speed
             , ARR_wind_speed
             , Holidays
             , DELAY)

#Controlling the Numeric format of some individual columns of the training set
train$DEP_HOUR=as.numeric(train$DEP_HOUR)
train$ARR_HOUR=as.numeric(train$ARR_HOUR)
train$MONTH=as.numeric(train$MONTH)
train$DAY_OF_WEEK=as.numeric(train$DAY_OF_WEEK)
train$ORIGIN_AIRPORT=as.numeric(train$ORIGIN_AIRPORT)
train$DESTINATION_AIRPORT=as.numeric(train$DESTINATION_AIRPORT)


#categorical vector, names of variables Y
Y = train$DELAY 

# for(i in 1:length(train)){
#   Y[i]=Y[i]
# }

#categorical vector, names of variables X

X = cbind(train$MONTH
          ,train$DAY
          ,train$DESTINATION_AIRPORT
          ,train$ARR_HOUR
          ,train$ORIGIN_AIRPORT
          ,train$DEP_HOUR
          ,train$DAY_OF_WEEK
          ,train$TAXI_OUT
          ,train$DISTANCE
          ,train$TAXI_IN
          ,train$DEP_wind_speed
          ,train$ARR_wind_speed
          ,train$Holidays)

dim(X)
typeof(X)
typeof(Y)

#Creating the GLM model
model_glm = glm(Y~X,train, family = binomial(link="logit")) #Gives even more precision than the gaussian prameter. There is lower empirical error than all other models tested in this code (glm-gaussian and glm-poisson)
#model_glm = glm(Y~X,train, family = poisson(link = "log")) #converge/work, Gives type 2 errors, but less empirical error for a same critical value
#model_glm = glm(Y~X,train, family = gaussian(link = "identity")) #converge/work, Gives type 1 errors, but more empirical error for a same criticla value

#Working with the GLM model
beta_glm = model_glm$coefficients # NA for linearly dependent vars
dev = model_glm$deviance #visualizing the deviance
beta_glm[is.na(beta_glm)] = 0 #setting NA values as 0 to be able to use the vector as multiplicator later on

#Applying the sigmoid function
etas_glm= 1 / (1 + exp(-(cbind(rep(1,nrow(train)), X) %*% beta_glm)))
etas_glm=cbind(etas_glm,Y)

#Testing with the critical value

summary(etas_glm)

# plot(etas_glm) #Plotting obtained forecast
# plot(beta_glm) #Plotting coefficients
# plot((cbind(rep(1,nrow(train)), X) %*% beta_glm)) #Plotting (trained data*betas)

  #Finding the correct Critical value for our model
CRIT=matrix(seq(0,1,0.001), length(seq(0,1,0.001)) ,2)
for(i in 1:length(seq(0,1,0.001))){
  c=CRIT[i,1]
  TypeI_errors_glm = length(which((etas_glm[,1] > c) & (etas_glm[,2] != 1)))
  TypeII_errors_glm = length(which((etas_glm[,1] <= c) & (etas_glm[,2] == 1)))
  CRIT[i,2] = (TypeI_errors_glm + TypeII_errors_glm) / nrow(train)
}
plot(CRIT, type="l", xlab= "Critical Value", ylab="Empirical Error")
summary(CRIT)
crit=CRIT[which.min(CRIT[,2]),1]

TypeI_errors_glm = length(which((etas_glm[,1] > crit) & (etas_glm[,2] != 1))) #false positives
TypeII_errors_glm = length(which((etas_glm[,1] <= crit) & (etas_glm[,2] == 1))) #false negatives
Empirical_error_glm = (TypeI_errors_glm + TypeII_errors_glm) / nrow(train)
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
                 , TAXI_OUT
                 , DISTANCE
                 , TAXI_IN
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
Xtest$DAY_OF_WEEK=as.numeric(Xtest$DAY_OF_WEEK)


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

etas_glm=cbind(Y_forecast_test,test$DELAY)

TypeI_errors_test = length(which((etas_glm[,1] > crit) & (etas_glm[,2] != 1)))
TypeII_errors_test = length(which((etas_glm[,1] <= 1-crit) & (etas_glm[,2] == 1)))
Empirical_error_test = (TypeI_errors_glm + TypeII_errors_glm) / nrow(test)

# plot(etas_glm)

#Analytical overview of the model and characteristics of weights, data sets, correlation...
    # summary(model_glm)
    # summary(Y_forecast_test)
    # summary(lm(X~Y))
    # lm(X~Y)


#For the bundle and comparison of the different models in different settings, 
#we rename the data with different names for each model trial.

TypeI_errors_glmRFT = TypeI_errors_glm
TypeII_errors_glmRFT = TypeII_errors_glm
Empirical_error_glmRFT = Empirical_error_glm#RFT stands for real future knowledge

TypeI_errors_glmtestRFT = TypeI_errors_test
TypeII_errors_glmtestRFT = TypeII_errors_test
Empirical_error_glmtestRFT = Empirical_error_test

# Here, (GLM RFT) we can notice that the empirical error from the training set is 28.37% and the one from
#the testing set is about 27.52% (rounded) - For a critical value of 0.519 in the model (optimal).

