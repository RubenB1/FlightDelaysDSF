
# !!! For this code to be running, we have to run all the codes 0 to 4 
# with (without clearing the environement before each code). 
# This code takes into consideration the outcome variables from previous codes !!!
  # Alternatively, we can run the code 5 "All in one", which contains all the 
  # codes 1 to 4 arranged to be runned in one single time.

#install.packages("tidyverse")
#install.packages("MASS")
library("MASS")
library("tidyverse")

#In this section the goal is to bundle the outcome of the tried models in order to combine 
#the errors and to check wether we could reduce the empirical error of our forecast by 
#some combination of outcomes and "learning our errors to overcome them".
#Therefore, we create a new model that include the data from flights and the corresponding
#forecast from previous models (GLM and LDA).

traindel=train$DELAY #real delay for training set
testdel=test$DELAY #real delay for testing set

modelstrain = cbind(etas_glm[1:27772,],etas_lda)

  colnames(modelstrain)[1] = "etas_glm" #renaming the first column of the matrix correctly
  modelstrain[,1]= ifelse(modelstrain[,1]>crit, 1,0) #changing the model value to the forecasted oucome (0/1)


# Comparing the errors from the GLM (RFT) and 
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

Empirical_error_ldabundled= (mis0+mis1+mis2+mis3)/length(Y)


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

# Here, (LDA Bundling model) we can notice that the empirical error from the training set is 33.40% and the one from
#the testing set is about 50.15% (rounded).

