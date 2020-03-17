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

#-------------------Exploring correlations------------------ 
library(PerformanceAnalytics)
library(dplyr)
library(caret)

training_subset.n=training_subset%>%select_if(is.numeric) #Creating subset of numeric variables to check all the correlations 
chart.Correlation(training_subset.n)                      #Showing all the correlations, scatterplots and distributions in one pic very useful to get the  insight on how to change features 

#-------------Tranaforming categorical variables ----------
lookup = training_subset %>%                     #creating lookup table for mean of max_price of brands
  group_by(brand) %>%
  summarise(mean_brand = mean(max_price))
training_subset = left_join(training_subset, lookup)   #Joining tables 
training_subset=training_subset[,-3]                   #removing "brand" column 
training_subset=training_subset[,c(-2,-3)]             #removing name and base_name
training_subset=training_subset[,-3]                   #removing pixel_x

library(forcats)
training_subset$screen_size=fct_collapse(as.factor(training_subset$screen_size), '11.6' = c("10.1","10.8","12","12.2","12.3"))  
training_subset$screen_size=fct_collapse(as.factor(training_subset$screen_size), '13.3' = c("12.5","13.5"))
training_subset$screen_size=fct_collapse(as.factor(training_subset$screen_size), '14' = c("13.9"))
training_subset$screen_size=fct_collapse(as.factor(training_subset$screen_size), '15.6' = c("15","15.4","16"))
training_subset$screen_size=fct_collapse(as.factor(training_subset$screen_size), '17.3' = c("17"))

lookup = training_subset %>%                     #creating lookup table for mean of screen_size
  group_by(screen_size) %>%
  summarise(mean_screensize = mean(max_price))
training_subset = left_join(training_subset, lookup)
 
training_subset=training_subset[,-2]              #removing "screen_size" column 

training_subset$pixels_y=fct_collapse(as.factor(training_subset$pixels_y), '768' = c("800","900"))  
training_subset$pixels_y=fct_collapse(as.factor(training_subset$pixels_y), '1080' = c("1200","1280","1440","1504","1600","1800","1824","1920","2000","2160"))

lookup = training_subset %>%                      #creating lookup table for mean of max_price
  group_by(pixels_y) %>%
  summarise(mean_pixely = mean(max_price))
training_subset = left_join(training_subset, lookup)

training_subset=training_subset[,-2]            #Removing "pixel_y" column

lookup = training_subset %>%                     #creating lookup table for mean of screen_surface
  group_by(screen_surface) %>%
  summarise(mean_screensurface = mean(max_price))
training_subset = left_join(training_subset, lookup)
training_subset=training_subset[,-2]             #Removing screen_surface

lookup = training_subset %>%                     #creating lookup table for mean of screen_size
  group_by(touchscreen) %>%
  summarise(mean_touchscreen = mean(max_price))
training_subset = left_join(training_subset, lookup)
training_subset=training_subset[,-2]             #Removing touchscreen column

training_subset$cpu=fct_collapse(as.factor(training_subset$cpu), 'AMD' = c("AMD A10","AMD A12","AMD A6","AMD A8","AMD A9","AMD FX","AMD Ryzen 3","AMD Ryzen 5","AMD Ryzen 7")) 
training_subset$cpu=fct_collapse(as.factor(training_subset$cpu), 'OTHER' = c("MediaTek","Rockchip","Samsung Exynos")) 

lookup = training_subset %>%                     #creating lookup table for mean of "cpu" prices
  group_by(cpu) %>%
  summarise(mean_cpu= mean(max_price))
training_subset = left_join(training_subset, lookup)
training_subset=training_subset[,-2]             #Removing cpu
training_subset=training_subset[,-2]             #Removing cpu_detail

lookup = training_subset %>%                     #creating lookup table for mean of dkeyboard
  group_by(dkeyboard) %>%
  summarise(mean_dkeyboard= mean(max_price))
training_subset = left_join(training_subset, lookup)
training_subset=training_subset[,-2]  #Removing dkeyboard

lookup = training_subset %>%                     #creating lookup table for mean of discrete_gpu
  group_by(discrete_gpu) %>%
  summarise(mean_discretegpu= mean(max_price))
training_subset = left_join(training_subset, lookup)
training_subset=training_subset[,-2]             #Removing discrete_gpu

training_subset$gpu=fct_collapse(as.factor(training_subset$gpu), 'AMD Radeon R' = c("AMD Radeon R2","AMD Radeon R4","AMD Radeon R5","AMD Radeon R6","AMD Radeon R7")) 

lookup = training_subset %>%                     #creating lookup table for mean of gpu
  group_by(gpu) %>%
  summarise(mean_gpu= mean(max_price))
training_subset = left_join(training_subset, lookup)
training_subset=training_subset[,-2]             #Removing gpu

lookup = training_subset %>%                     #creating lookup table for mean of gpu
  group_by(os) %>%
  summarise(mean_os= mean(max_price))
training_subset = left_join(training_subset, lookup)
training_subset=training_subset[,-2]             #Removing os

training_subset=training_subset[,-2]             #Removing os_details
training_subset$ram=as.numeric(training_subset$ram)
training_subset$ssd=as.numeric(training_subset$ssd)
training_subset$storage=as.numeric(training_subset$storage)

#---------------Scaling the data to (0,1) range---------
x = training_subset[, -c(6,7)]   #Removing response variables (Min and Max price) from the training subset 
y = training_subset$max_price    #Having max_price as response


training_without=subset(training_subset,select=-c(6,7))      #Ranging all the variables between 0 and 1
preProcess_range_model <- preProcess(training_without, method='range')
trainData <- predict(preProcess_range_model, newdata = training_without)

featurePlot(x = trainData,                                   #creating scaterplot to look at relations
            y = training_subset$max_price, 
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(5, 3))

set.seed(100)                            #Random Forest for finding the most imporant varaibles 
options(warn=-1)

ctrl <- rfeControl(functions = rfFuncs,
                                      method = "repeatedcv",
                                       repeats = 5,
                                      verbose = FALSE)

   lmProfile <- rfe(x=trainData[, 2:15], y=training_subset$max_price,
                                 rfeControl = ctrl)

   lmProfile
