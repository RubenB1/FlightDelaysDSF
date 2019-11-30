
# In the previous code with the LDA analysis, we noticed that the outcome was from 1 to 4.
#Instead, our set categories were from 0 to 3. This caused a problem and a quite high 
#empirical error as the comparison was biased with a +1 factor. Finally, we workd on 
#the issue and formulated this more elaborated and corrected code with a -1 factor 
#in the LDA model forecast outcome before the test for the empirical error.
#This reduced a lot the empirical error.

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
library(MASS)
library(ggplot2)

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

# Here, (LDA model enhanced) we can notice that the empirical error from the training set is 33.40% and the one from
#the testing set is about 50.06% (rounded). The forecast is not very precise BUT we can try to
#bundle the different outcomes from the different models in order to obtain more precision).

