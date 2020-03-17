#---------Setup Libraries------------------------------------
#install.packages("tidyverse")
#install.packages("naniar")
#install.packages("VIM")
#install.packages("DMwR")
#install.packages("caret")

#---------Load Libraries ------------------------------------
library(plyr)
library(tidyverse)
library(naniar)
library(VIM)
library(DMwR)
library(caret)

#--------Load Data-------------------------------------------
train_df <- read.csv("https://raw.githubusercontent.com/behnouri/lprice-prediction/master/train.csv", na.strings = c("", "NA"))
test_df <- read.csv("https://raw.githubusercontent.com/behnouri/lprice-prediction/master/test.csv", na.strings=c("NA",""))

#--------Prepare Train Data---------------------------------
head(train_df)
sum(is.na(train_df))
colnames(train_df)[12] <- "dkeyboard"
colnames(train_df)[1] <- "id"


vis_miss(train_df,cluster= TRUE)
gg_miss_var(train_df)
gg_miss_case(train_df)


rown_four_nulls <- as.integer(rownames(train_df[rowSums(is.na(train_df[])) == 4,]))
clean2 <- train_df[-c(rown_four_nulls),]
gg_miss_var(clean2)
gg_miss_case(clean2)

clean2$screen_surface <- mapvalues(clean2$screen_surface,c("glossy", "matte"), c("Glossy", "Matte"))

aggr(x = clean2[,8:20])
glimpse(clean2)
clean3_knn <- knnImputation(clean2)
aggr(x=clean3_knn)

clean3_knn %>%
  summarise_if(is.factor,nlevels)


#-------Split Train Data to train/test subsets (80/20 percent) ----------------------
require(caTools)
set.seed(741)
sample = sample.split(clean3_knn$id,SplitRatio = 0.8)
training_subset =subset(clean3_knn,sample ==TRUE)
test_subset = subset(clean3_knn,sample ==FALSE)


#-------Prepare Test Data-----------------------------------
colnames(test_df)[12] <- "dkeyboard"
colnames(test_df)[1] <- "id"
glimpse(test_df)
sum(is.na(test_df))
aggr(x=test_df[,6:20])
clean_test <- test_df
clean_test$screen_surface <- mapvalues(clean_test$screen_surface,c("glossy","matte"),c("Glossy","Matte"))

#--------- Data not normalized ---------------

# Selecting only the features to use
maxPrice_Clean_Training_prev <- training_subset %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os, max_price)
maxPrice_Clean_Training <- data.frame(model.matrix(~., data=maxPrice_Clean_Training_prev))

minPrice_Clean_Training_prev <- training_subset %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os, min_price)
minPrice_Clean_Training <- data.frame(model.matrix(~., data=minPrice_Clean_Training_prev))


#-------- Data normalization -------------------

index_Categ <- match(c("brand", "touchscreen", "dkeyboard", "os", "max_price", "min_price"), names(training_subset))
preProcValues <- preProcess(training_subset[-index_Categ], method = "range")

trainScaled <- predict(preProcValues, training_subset)
glimpse(trainScaled)

#testScaleded <- predict(preProcValues, test) #Should also normalized the test data based on the training data



#------Repeated K-Fold Cross Validation (K = 10, repeats = 3)----------------

# Selecting only the features to use
maxPrice_Norm_Training_prev <- trainScaled %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os, max_price)
maxPrice_Norm_Training <- data.frame(model.matrix(~., data=maxPrice_Norm_Training_prev))
maxPrice_Norm_Training

minPrice_Norm_Training_prev <- trainScaled %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os, min_price)
minPrice_Norm_Training <- data.frame(model.matrix(~., data=minPrice_Norm_Training_prev))
minPrice_Norm_Training

# Training control definition
set.seed(123)
train.control <- trainControl(method = "repeatedcv",
                              number = 5, repeats = 3)



#--------Models for maxPrice with Normalized data (except decision tree models) -----------------

##### Train the model 1 (Linear regression)
model1_max <- train(max_price ~ . , data = maxPrice_Norm_Training,
                    method = "lm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 2 (Generalized Linear Model without func specified -> could be improved)
model2_max <- train(max_price ~ . , data = maxPrice_Norm_Training,
                    method = "glm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 3 (GLM with Step AIC)
model3_max <- train(max_price ~ . , data = maxPrice_Norm_Training,
                    method = "glmStepAIC", trControl = train.control, metric = "MAE")

##### Train the model 4 (Elastic net (glm))
model4_max <- train(max_price ~ . , data = maxPrice_Norm_Training,
                    method = "glmnet", trControl = train.control, metric = "MAE")

##### Train the model 5 Boosted Tree
model5_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                    method = "bstTree", trControl = train.control, metric = "MAE")

##### Train the model 6 eXtreme Gradient Boosting
model6_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                    method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 7 Parallel Random Forest
model7_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 8 Stochastic Gradient Boosting
model8_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                    method = "gbm", trControl = train.control, metric = "MAE")



#--------Models for min_price with Normalized data (except decision tree models) -----------------

##### Train the model 1 (Linear regression)
model1_min <- train(min_price ~ . , data = minPrice_Norm_Training,
                    method = "lm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 2 (Generalized Linear Model without func specified -> could be improved)
model2_min <- train(min_price ~ . , data = minPrice_Norm_Training,
                    method = "glm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 3 (GLM with Step AIC)
model3_min <- train(min_price ~ . , data = minPrice_Norm_Training,
                    method = "glmStepAIC", trControl = train.control, metric = "MAE")

##### Train the model 4 (Elastic net (glm))
model4_min <- train(min_price ~ . , data = minPrice_Norm_Training,
                    method = "glmnet", trControl = train.control, metric = "MAE")

##### Train the model 5 Boosted Tree
model5_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "bstTree", trControl = train.control, metric = "MAE")

##### Train the model 6 eXtreme Gradient Boosting
model6_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 7 Parallel Random Forest
model7_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 8 Stochastic Gradient Boosting
model8_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "gbm", trControl = train.control, metric = "MAE")



#------- Summarize the results with Normalized data ----------------

print((model1_max$results$MAE+model1_min$results$MAE)/2)
print((model2_max$results$MAE+model2_min$results$MAE)/2)
print((model3_max$results$MAE+model3_min$results$MAE)/2)
print(min((model4_max$results$MAE+model4_min$results$MAE)/2))
print(min((model5_max$results$MAE+model5_min$results$MAE)/2))
print(min((model6_max$results$MAE+model6_min$results$MAE)/2))
print(min((model7_max$results$MAE+model7_min$results$MAE)/2))
print(min((model8_max$results$MAE+model8_min$results$MAE)/2))


# Test data subset not normalized (should use the real test data once it is clean)
Test_prev <- test_subset %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os)
Price_Test <- data.frame(model.matrix(~., data=Test_prev))


# Test data subset normalized (should use the real test data once it is clean)
testScaled <- predict(preProcValues, test_subset)
glimpse(test_subset)
glimpse(testScaled)

NormTest_prev <- testScaled %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os)
Price_NormTest <- data.frame(model.matrix(~., data=NormTest_prev))


# Prediction of min_price
predict(model1_min, maxPrice_NormTest, type = "raw") #Liner regression

predict(model7_min, maxPrice_Test, type = "raw") #Parallel Random Forest

# Prediction of max_price
predict(model1_max, maxPrice_NormTest, type = "raw") #Liner regression

predict(model7_max, maxPrice_Test, type = "raw") #Parallel Random Forest






# ------- Other models already tried ---------------

# Bayesian Generalized Linear Model ("bayesglm"): error 212.22
# Boosted Generalized Linear Model ("glmboost"): error 216.01
# Boosted Linear Model ("BstLm"): error 264.93
# Generalized Additive Model using LOESS ("gamLoess"): error 210.55 (with some warnings)
# Generalized Additive Model using Splines ("gam"): error 215.83
# L2 Regularized Support Vector Machine (dual) with Linear Kernel ("svmLinear3"): error 211.24
# Multi-Layer Perceptron ("mlp"): error 483.44 (There were missing values in resampled performance measures) -> due to unbalance variables
# Multi-Layer Perceptron, multiple layers ("mlpWeightDecayML"): error 478.04 (There were missing values in resampled performance measures) -> due to unbalance variables
# Neural Network ("nnet"): error 815 (There were missing values in resampled performance measures) -> due to unbalance variables
# Partial Least Squares ("pls"): error 224.67

# Models that failed
# Bayesian Additive Regression Trees ("bartMachine"): shows an error with the rjava.
# Bayesian Regularized Neural Networks ("brnn"): shows an error missing values (no MAE).
# Boosted Generalized Additive Model ("gamboost"): error (no MAE).
# Dynamic Evolving Neural-Fuzzy Inference System ("DENFIS"): attributes without interval/range (no MAE).
# Elasticnet ("enet"): error some columns with zero variance.
# Ensembles of Generalized Linear Models ("randomGLM"): nCandidateCovariates is larger than nFeaturesInBag. Had to stop, takes too much time
# Gradient Boosting Machines ("gbm_h2o"): model fit failed (no MAE).
# Partial Least Squares Generalized Linear Models ("plsRglm"): error (no MAE).
# Random Forest by Randomization ("extraTrees"): shows an error with the rjava.
# Relevance Vector Machines with Polynomial Kernel ("rvmPoly"): model fit failed (no MAE).


