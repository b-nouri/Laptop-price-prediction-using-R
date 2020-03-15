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





#------Repeated K-Fold Cross Validation (K = 10, repeats = 3)----------------

# Selecting only the features to use
maxPrice_Clean_Training <- training_subset %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os, max_price)
glimpse(maxPrice_Clean_Training)

minPrice_Clean_Training <- training_subset %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os, min_price)
glimpse(minPrice_Clean_Training)

# Training control definition
set.seed(123)
train.control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)

#--------Models for maxPrice -----------------

##### Train the model 1 (Linear regression)
model1_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                method = "lm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 2 (Generalized Linear Model without func specified -> could be improved)
model2_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                method = "glm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 3 (GLM with Step AIC)
model3_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                method = "glmStepAIC", trControl = train.control, metric = "MAE")

##### Train the model 4 (Elastic net (glm))
model4_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                method = "glmnet", trControl = train.control, metric = "MAE")

#--------Models for min_price -----------------

model1_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                method = "lm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 2 (Generalized Linear Model without func specified -> could be improved)
model2_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                method = "glm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 3 (GLM with Step AIC)
model3_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                method = "glmStepAIC", trControl = train.control, metric = "MAE")

##### Train the model 4 (Elastic net (glm))
model4_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                method = "glmnet", trControl = train.control, metric = "MAE")


#------- Summarize the results----------------

print((model1_max$results$MAE+model1_min$results$MAE)/2)
print((model2_max$results$MAE+model2_min$results$MAE)/2)
print((model3_max$results$MAE+model3_min$results$MAE)/2)
print(min((model4_max$results$MAE+model4_min$results$MAE)/2))

