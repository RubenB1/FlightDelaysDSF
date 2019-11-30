#Code 5, All in one!


# # Analysis and regression code ------------------------------------------

# Part 1: Analysis --------------------------------------------------------


# Part 2: Logistic regression ---------------------------------------------
## Step 1: Preparation
graphics.off() # close all plots
rm(list=ls()) # remove all objects from the current workspace (R memory)

load(file = "data15_weather_holidays.RData")#load cleaned dataframe (df) into environment
## Step 2:
#install.packages("tidyverse")
#install.packages("MASS")
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
etas_glm= 1 / (1 + exp(-(cbind(rep(1,nrow(train)), X) %*% beta_glm))) #applying sigmoid function
etas_glm=cbind(etas_glm,Y)

#Testing with the critical value
crit=0.5
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
summary(model_glm)
summary(Y_forecast_test)
summary(lm(X~Y))
lm(X~Y)


# Real future knowledge code ----------------------------------------------

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

etas_glmtest=cbind(Y_forecast_test,test$DELAY)

TypeI_errors_test = length(which((etas_glm[,1] > crit) & (etas_glm[,2] != 1)))
TypeII_errors_test = length(which((etas_glm[,1] <= 1-crit) & (etas_glm[,2] == 1)))
Empirical_error_test = (TypeI_errors_glm + TypeII_errors_glm) / nrow(test)

# plot(etas_glm)

#Analytical overview of the model and characteristics of weights, data sets, correlation...
summary(model_glm)
summary(Y_forecast_test)
summary(lm(X~Y))
lm(X~Y)


#For the bundle and comparison of the different models in different settings, 
#we rename the data with different names for each model trial.

TypeI_errors_glmRFT = TypeI_errors_glm
TypeII_errors_glmRFT = TypeII_errors_glm
Empirical_error_glmRFT = Empirical_error_glm#RFT stands for real future knowledge

TypeI_errors_glmtestRFT = TypeI_errors_test
TypeII_errors_glmtestRFT = TypeII_errors_test
Empirical_error_glmtestRFT = Empirical_error_test



# Forecast categories of delay code --------------------------------------------

#create additional column where all the delay values are summarised as:
# 0 = no delay (<0)
# 1 = small delay (0 to 10 min)
# 2 = medium delay (10+ to 45 min)
# 3 = large delay (45+ to more delay)

data$DELAY = ifelse(data$ARRIVAL_DELAY > 45, 3,
                    (ifelse(data$ARRIVAL_DELAY > 10, 2,
                            (ifelse(data$ARRIVAL_DELAY > 0, 1, 0)))))

#How many flights were delayed in November and December
delayed_summary= as.data.frame(table(data$DELAY))
# delayed_11= as.data.frame(table(train$DELAY))
# delayed_12= as.data.frame(table(test$DELAY))

#splitting df based on column value of Month:

#Ensuring cleaned data
data$MONTH=as.numeric(data$MONTH)
data = na.omit(data)

#Controlling the Numeric format of some individual columns of the data set
data$DEP_HOUR=as.numeric(data$DEP_HOUR)
data$ARR_HOUR=as.numeric(data$ARR_HOUR)
data$DAY_OF_WEEK=as.numeric(data$DAY_OF_WEEK)
data$ORIGIN_AIRPORT=as.numeric(data$ORIGIN_AIRPORT)
data$DESTINATION_AIRPORT=as.numeric(data$DESTINATION_AIRPORT)

#Defining the training and testing sets (by months)
train= filter(data, data$MONTH <= 11) #Training set November (11)
test= filter(data, data$MONTH >= 12) #Testing set December (12)


## Step 4: Logistic regression (why not linear? there are more than 2 covariates) with test df

#chose categorical variable for y:
p = ncol(train)

#Working with convenient formats
#We select only the columns that the analysis 
#have shown to be relevant (see analysis analytical dashboard)


#categorical vector, names of variables Y
Y = train$DELAY 

#categorical vector, names of variables X

X = cbind(train$DAY
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

#Creating the LDA (Linear Discriminant Analysis) model, convenient for clasification problems of more than 2 categories (4 categories in our case)
library(MASS)
model_lda = lda(Y~X,train) 

#Working with the GLM model
etas_lda = predict(model_lda,train)$class
#summary(model_lda)

# plot(model_lda) #Plotting obtained forecast

etas_lda=cbind(etas_lda,Y) #put together the forecasted data and the true reality


#Checking the misclassified and well classified items per category


mis0 = length(which((etas_lda[,1] != 0) & (etas_lda[,2] == 0))) #misclassified 0 values
well0 = length(which((etas_lda[,1] == 0) & (etas_lda[,2] == 0))) #well classified 0 values

mis1 = length(which((etas_lda[,1] != 1) & (etas_lda[,2] == 1))) #misclassified 1 values
well1 = length(which((etas_lda[,1] == 1) & (etas_lda[,2] == 1))) #well classified 1 values

mis2 = length(which((etas_lda[,1] != 2) & (etas_lda[,2] == 2))) #misclassified 2 values
well2 = length(which((etas_lda[,1] == 2) & (etas_lda[,2] == 2))) #well classified 2 values

mis3 = length(which((etas_lda[,1] != 3) & (etas_lda[,2] == 3))) #misclassified 3 values
well3 = length(which((etas_lda[,1] == 3) & (etas_lda[,2] == 3))) #well classified 3 values

errors = cbind(rbind("0",mis0,well0),rbind("1",mis1,well1),rbind("2",mis2,well2),rbind("3",mis3,well3))
summary(errors)

Empirical_error_lda= (mis0+mis1+mis2+mis3)/length(Y)



##TESTING PART

#Taking similar sized data
test=test[1:27772,]
Y = test$DELAY[1:27772]

etas_ldatest = predict(model_lda,test)$class
summary(model_lda)

# plot(model_lda) #Plotting obtained forecast

etas_ldatest=cbind(etas_ldatest,Y) #put together the forecasted data and the true reality


# Computing the errors for the TESTING phase

mistest0 = length(which((etas_ldatest[,1] != 0) & (etas_ldatest[,2] == 0))) #misclassified 0 values
welltest0 = length(which((etas_ldatest[,1] == 0) & (etas_ldatest[,2] == 0))) #well classified 0 values

mistest1 = length(which((etas_ldatest[,1] != 1) & (etas_ldatest[,2] == 1))) #misclassified 1 values
welltest1 = length(which((etas_ldatest[,1] == 1) & (etas_ldatest[,2] == 1))) #well classified 1 values

mistest2 = length(which((etas_ldatest[,1] != 2) & (etas_ldatest[,2] == 2))) #misclassified 2 values
welltest2 = length(which((etas_ldatest[,1] == 2) & (etas_ldatest[,2] == 2))) #well classified 2 values

mistest3 = length(which((etas_ldatest[,1] != 3) & (etas_ldatest[,2] == 3))) #misclassified 3 values
welltest3 = length(which((etas_ldatest[,1] == 3) & (etas_ldatest[,2] == 3))) #well classified 3 values

errorstest = cbind(rbind("0",mistest0,welltest0),rbind("1",mistest1,welltest1),rbind("2",mistest2,welltest2),rbind("3",mistest3,welltest3))
summary(errors)

Empirical_error_ldatest= (mistest0+mistest1+mistest2+mistest3)/length(Y)

# plot(etas_ldatest)


#Analytical overview of the model and characteristics of weights, data sets, correlation...
  # summary(model_lda)
  # summary(etas_ldatest)
  # summary(lm(X~Y))
  # lm(X~Y)
  # plot(model_lda)

#For the bundle and comparison of the different models in different settings, 
#we mention here the data the will be reused for summarization purposes.

Empirical_error_lda86 = Empirical_error_lda

Empirical_error_ldatest86 = Empirical_error_ldatest




# Forecast categories of delay enhanced -----------------------------------

load(file = "data15_weather_holidays.RData")#load cleaned dataframe (df) into environment

#Ensuring cleaned data
data$MONTH=as.numeric(data$MONTH)
data = na.omit(data)

## Step 3: Split up column 
dim(data) #rows and columns in df

#create additional column where all the delay values are summarised as:
# 0 = no delay (<0)
# 1 = small delay (0 to 10 min)
# 2 = medium delay (10+ to 45 min)
# 3 = large delay (45+ to more delay)

data$DELAY = ifelse(data$ARRIVAL_DELAY > 45, 3,
                    (ifelse(data$ARRIVAL_DELAY > 10, 2,
                            (ifelse(data$ARRIVAL_DELAY > 0, 1, 0)))))


#Controlling the Numeric format of some individual columns of the data set
data$DEP_HOUR=as.numeric(data$DEP_HOUR)
data$ARR_HOUR=as.numeric(data$ARR_HOUR)
data$DAY_OF_WEEK=as.numeric(data$DAY_OF_WEEK)
data$ORIGIN_AIRPORT=as.numeric(data$ORIGIN_AIRPORT)
data$DESTINATION_AIRPORT=as.numeric(data$DESTINATION_AIRPORT)

#Defining the training and testing sets (by months)
train= filter(data, data$MONTH <= 11) #Training set November (11)
test= filter(data, data$MONTH >= 12) #Testing set December (12)


## Step 4: Logistic regression (why not linear? there are more than 2 covariates) with test df

#chose categorical variable for y:
p = ncol(train)

#Working with convenient formats
#We select only the columns that the analysis 
#have shown to be relevant (see analysis analytical dashboard)


#categorical vector, names of variables Y
Y = train$DELAY 

#categorical vector, names of variables X

X = cbind(train$DAY
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

#Creating the LDA (Linear Discriminant Analysis) model, convenient for clasification problems of more than 2 categories (4 categories in our case)
model_lda = lda(Y~X,train) 

#Working with the GLM model
etas_lda = predict(model_lda,train)$class
summary(model_lda)

# plot(model_lda) #Plotting obtained forecast
etas_lda=as.numeric(etas_lda)-1
etas_lda=cbind(etas_lda,Y) #put together the forecasted data and the true reality

#We include a "-1" because the categorical LDA model gives almost always (often) 
#a forecast in the category above the one set (1-4 categories instead of 0-3 categories) 
#with the 0 standing for no delay. With the manipulation, we reduce the emprical error
#from 85% to 21.81%. We give sense to the data analysed by gaphic visualization with the 
#dashboard approach (see analysis paper part)


#Checking the misclassified and well classified items per category


mis0 = length(which((etas_lda[,1] != 0) & (etas_lda[,2] == 0))) #misclassified 0 values
well0 = length(which((etas_lda[,1] == 0) & (etas_lda[,2] == 0))) #well classified 0 values

mis1 = length(which((etas_lda[,1] != 1) & (etas_lda[,2] == 1))) #misclassified 1 values
well1 = length(which((etas_lda[,1] == 1) & (etas_lda[,2] == 1))) #well classified 1 values

mis2 = length(which((etas_lda[,1] != 2) & (etas_lda[,2] == 2))) #misclassified 2 values
well2 = length(which((etas_lda[,1] == 2) & (etas_lda[,2] == 2))) #well classified 2 values

mis3 = length(which((etas_lda[,1] != 3) & (etas_lda[,2] == 3))) #misclassified 3 values
well3 = length(which((etas_lda[,1] == 3) & (etas_lda[,2] == 3))) #well classified 3 values

errors = cbind(rbind("0",mis0,well0),rbind("1",mis1,well1),rbind("2",mis2,well2),rbind("3",mis3,well3))
summary(errors)

Empirical_error_lda= (mis0+mis1+mis2+mis3)/length(Y)



##TESTING PART

#Taking similar sized data
test=test[1:27772,]
Y = test$DELAY[1:27772]

etas_ldatest = predict(model_lda,test)$class
summary(model_lda)

# plot(model_lda) #Plotting obtained forecast
etas_ldatest=as.numeric(etas_ldatest)-1
etas_ldatest=cbind(etas_ldatest,Y) #put together the forecasted data and the true reality


# Computing the errors for the TESTING phase

mistest0 = length(which((etas_ldatest[,1] != 0) & (etas_ldatest[,2] == 0))) #misclassified 0 values
welltest0 = length(which((etas_ldatest[,1] == 0) & (etas_ldatest[,2] == 0))) #well classified 0 values

mistest1 = length(which((etas_ldatest[,1] != 1) & (etas_ldatest[,2] == 1))) #misclassified 1 values
welltest1 = length(which((etas_ldatest[,1] == 1) & (etas_ldatest[,2] == 1))) #well classified 1 values

mistest2 = length(which((etas_ldatest[,1] != 2) & (etas_ldatest[,2] == 2))) #misclassified 2 values
welltest2 = length(which((etas_ldatest[,1] == 2) & (etas_ldatest[,2] == 2))) #well classified 2 values

mistest3 = length(which((etas_ldatest[,1] != 3) & (etas_ldatest[,2] == 3))) #misclassified 3 values
welltest3 = length(which((etas_ldatest[,1] == 3) & (etas_ldatest[,2] == 3))) #well classified 3 values

errorstest = cbind(rbind("0",mistest0,welltest0),rbind("1",mistest1,welltest1),rbind("2",mistest2,welltest2),rbind("3",mistest3,welltest3))
summary(errors)

Empirical_error_ldatest= (mistest0+mistest1+mistest2+mistest3)/length(Y)

# plot(etas_ldatest)


#Analytical overview of the model and characteristics of weights, data sets, correlation...
    # summary(model_lda)
    # summary(etas_ldatest)
    # summary(lm(X~Y))
    # lm(X~Y)
  # plot(model_lda)


#For the bundle and comparison of the different models in different settings, 
#we mention here the data the will be reused for summarization purposes.

Empirical_error_lda33 = Empirical_error_lda

Empirical_error_ldatest33 = Empirical_error_ldatest



# Bundling models ---------------------------------------------------------

#In this section the goal is to bundle the outcome of the tried models in order to combine 
#the errors and to check wether we could reduce the empirical error by soe combination of 
#outcomes and "learning our errors to overcome them".

traindel=train$DELAY #real delay for training set
testdel=test$DELAY #real delay for testing set

modelstrain = cbind(etas_glm,etas_lda)

colnames(modelstrain)[1] = "etas_glm" #renaming the first column of the matrix correctly
modelstrain[,1]= ifelse(modelstrain[,1]>crit, 1,0) #changing the model value to the forecasted oucome (0/1)


#"comparing the errors from the GLN (RFT) and 
#the LDA models for errors that can be bundles and therefore reduced.
#For example, if an error was made by the GLM model but not by the LDA, then we can bundle
#that knowledge and therefore waive errors between models.


potential=length(which((modelstrain[,4]!=0) & (modelstrain[,1]==0) & (modelstrain[,3]==0)))



modelstrain= modelstrain[,-2]
Y=modelstrain[,3]
X = cbind(train$DAY
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
          ,train$Holidays
          ,modelstrain[,1:2])


modelstrain=as.data.frame(modelstrain)
class(modelstrain)


modelstrain_lda = lda(Y~X,modelstrain)
etas_lda_bundle = predict(modelstrain_lda,modelstrain)$class
# View(etas_lda_bundle)

etas_lda_bundle= cbind(as.numeric(etas_lda_bundle)-1,Y)


#Checking the misclassified and well classified items per category

mis0 = length(which((etas_lda_bundle[,1] != 0) & (etas_lda_bundle[,2] == 0))) #misclassified 0 values
well0 = length(which((etas_lda_bundle[,1] == 0) & (etas_lda_bundle[,2] == 0))) #well classified 0 values

mis1 = length(which((etas_lda_bundle[,1] != 1) & (etas_lda_bundle[,2] == 1))) #misclassified 1 values
well1 = length(which((etas_lda_bundle[,1] == 1) & (etas_lda_bundle[,2] == 1))) #well classified 1 values

mis2 = length(which((etas_lda_bundle[,1] != 2) & (etas_lda_bundle[,2] == 2))) #misclassified 2 values
well2 = length(which((etas_lda_bundle[,1] == 2) & (etas_lda_bundle[,2] == 2))) #well classified 2 values

mis3 = length(which((etas_lda_bundle[,1] != 3) & (etas_lda_bundle[,2] == 3))) #misclassified 3 values
well3 = length(which((etas_lda_bundle[,1] == 3) & (etas_lda_bundle[,2] == 3))) #well classified 3 values

errors = cbind(rbind("0",mis0,well0),rbind("1",mis1,well1),rbind("2",mis2,well2),rbind("3",mis3,well3))
summary(errors)

Empirical_error_lda= (mis0+mis1+mis2+mis3)/length(Y)


##TESTING PART

#Taking similar sized data

modelstest = cbind(etas_glmtest[1:27772,],etas_ldatest)
modelstest[,1]= ifelse(modelstest[,1]>crit, 1,0)

potentialtest=length(which((modelstest[,4]!=0) & (modelstest[,1]==0) & (modelstest[,3]==0)))
modelstest= modelstest[,-2]#############


modelstest=cbind(test[1:27772,],modelstest[,1:2])
Y = testdel[1:27772]


etas_lda_bundletest = predict(modelstrain_lda,modelstest)$class

# plot(model_lda) #Plotting obtained forecast
etas_lda_bundletest=as.numeric(etas_lda_bundletest)-1
etas_lda_bundletest=cbind(etas_lda_bundletest,Y) #put together the forecasted data and the true reality


# Computing the errors for the TESTING phase

mistest0 = length(which((etas_lda_bundletest[,1] != 0) & (etas_lda_bundletest[,2] == 0))) #misclassified 0 values
welltest0 = length(which((etas_lda_bundletest[,1] == 0) & (etas_lda_bundletest[,2] == 0))) #well classified 0 values

mistest1 = length(which((etas_lda_bundletest[,1] != 1) & (etas_lda_bundletest[,2] == 1))) #misclassified 1 values
welltest1 = length(which((etas_lda_bundletest[,1] == 1) & (etas_lda_bundletest[,2] == 1))) #well classified 1 values

mistest2 = length(which((etas_lda_bundletest[,1] != 2) & (etas_lda_bundletest[,2] == 2))) #misclassified 2 values
welltest2 = length(which((etas_lda_bundletest[,1] == 2) & (etas_lda_bundletest[,2] == 2))) #well classified 2 values

mistest3 = length(which((etas_lda_bundletest[,1] != 3) & (etas_lda_bundletest[,2] == 3))) #misclassified 3 values
welltest3 = length(which((etas_lda_bundletest[,1] == 3) & (etas_lda_bundletest[,2] == 3))) #well classified 3 values

errorstest = cbind(rbind("0",mistest0,welltest0),rbind("1",mistest1,welltest1),rbind("2",mistest2,welltest2),rbind("3",mistest3,welltest3))
summary(errors)

Empirical_error_ldatest= (mistest0+mistest1+mistest2+mistest3)/length(Y)

# plot(etas_lda_bundletest)


#Analytical overview of the model and characteristics of weights, data sets, correlation...
    # summary(model_lda)
    # summary(etas_lda_bundletest)
    # summary(lm(X~Y))
    # lm(X~Y)
  # plot(model_lda)


#For the bundle and comparison of the different models in different settings, 
#we mention here the data the will be reused for summarization purposes.

Empirical_error_lda_bundle = Empirical_error_lda

Empirical_error_lda_bundletest = Empirical_error_ldatest


