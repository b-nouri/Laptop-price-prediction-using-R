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
library(PerformanceAnalytics)

#--------Load Data-------------------------------------------
train_df <- read.csv("https://raw.githubusercontent.com/behnouri/lprice-prediction/master/train.csv", na.strings = c("", "NA"))
test_df <- read.csv("https://raw.githubusercontent.com/behnouri/lprice-prediction/master/test.csv", na.strings=c("NA",""))

#--------Load CPU and GPU Data-----------------------------------------
gpu_df <- read.csv("https://raw.githubusercontent.com/behnouri/lprice-prediction/master/GPU_Benchmark.csv", na.strings = c("", "NA"))
cpu_df <- read.csv("https://raw.githubusercontent.com/behnouri/lprice-prediction/master/CPU_Benchmark.csv", na.strings = c("", "NA"))
colnames(cpu_df)[1] <- "cpu_model"
colnames(cpu_df)[2] <- "cpu_benchmark_score"
colnames(gpu_df)[1] <- "gpu_model"
colnames(gpu_df)[2] <- "gpu_benchmark_score"
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

clean4 <- clean3_knn %>%
  mutate(resolution = pixels_x * pixels_y)

df_res <- unique(clean4[c("screen_size","pixels_x","pixels_y","resolution")])
df_res %>%
  arrange(desc(resolution))

ggplot(clean4,aes(x=resolution,y=max_price,color=screen_size)) +
  geom_point() +
  scale_color_gradient(low="blue", high="red")


cor(clean4$resolution,clean4$max_price)
cor(clean4$resolution,clean4$max_price,method = "spearman")
cor(clean4$screen_size,clean4$max_price)

sort(unique(clean4$screen_size))

clean4 %>%
  select(screen_size) %>%
  table()


clean4[clean4$pixels_x == 3840, c("brand","base_name","screen_size","pixels_x","pixels_y")]


#--------------CPU Scores-----------------------------------------------
clean4<-clean4 %>%
  mutate(cpu_details,cpu_clean= gsub("\\s*(\\d[.]\\d*)\\s*(GHz|ghz|Ghz|Ghz|gHz).*","",clean4$cpu_details))

cpu_df<-cpu_df %>%
  mutate(cpu_model,cpu_clean= gsub("\\s*([@]).*|\\s*(APU).*","",cpu_df$cpu_model))

clean5 <- clean4 %>%
  left_join(cpu_df,by="cpu_clean")

clean5$cpu_model <- as.character(clean5$cpu_model)
clean5$cpu_benchmark_score[is.na(clean5$cpu_benchmark_score)] <- 500
clean5$cpu_model[is.na(clean5$cpu_model)] <- "other"



#--------------GPU Scores-----------------------------------------------
clean6 <- mutate(clean5, gpu = ifelse(discrete_gpu == 0, 0,as.character(gpu)))

clean6<-clean6 %>%
  mutate(gpu,gpu_model= gsub("^(\\S+\\s+\\n?){1}","",clean6$gpu))

gpu_df[,1] <- gsub(" with", "", gpu_df$gpu_model)
gpu_df[,1] <- gsub(" Design", "", gpu_df$gpu_model)

clean6$gpu_model <- gsub("GeFoce", "GeForce", clean6$gpu_model)
clean6$gpu_model <- gsub("GTX1070", "GTX 1070", clean6$gpu_model)

clean6 <- clean6 %>%
  left_join(gpu_df,by="gpu_model")

clean6$gpu_benchmark_score[clean6$gpu_model == 0] <- 0

geforce_df <- filter(clean6, grepl('GeForce',clean6$gpu))
geforce_mean_score <- mean(geforce_df$gpu_benchmark_score, na.rm =TRUE)

clean6[is.na(clean6$gpu_benchmark_score) & grepl("GeForce",clean6$gpu_model),"gpu_benchmark_score"] <- geforce_mean_score

gpu_null <- clean6 %>%
  select(gpu_model,gpu_benchmark_score) %>%
  filter(is.na(clean6$gpu_benchmark_score))

clean6[is.na(clean6$gpu_benchmark_score),"gpu_benchmark_score"] <- mean(clean6$gpu_benchmark_score,na.rm=TRUE)


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

clean_test_knn <- knnImputation(clean_test)
aggr(x=clean_test_knn)

clean_test_knn %>%
  summarise_if(is.factor,nlevels)


#--------- Data not normalized ---------------

# Selecting only the features to use
maxPrice_Clean_Training_prev <- clean3_knn %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os, max_price)
maxPrice_Clean_Training <- data.frame(model.matrix(~., data=maxPrice_Clean_Training_prev))

minPrice_Clean_Training_prev <- clean3_knn %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os, min_price)
minPrice_Clean_Training <- data.frame(model.matrix(~., data=minPrice_Clean_Training_prev))


# Adding pixels_x, discrete_gpu, removing os

maxPrice_Clean_Training_prev2 <- clean3_knn %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, pixels_x, discrete_gpu, max_price)
maxPrice_Clean_Training2 <- data.frame(model.matrix(~., data=maxPrice_Clean_Training_prev2))

minPrice_Clean_Training_prev2 <- clean3_knn %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, pixels_x, discrete_gpu, min_price)
minPrice_Clean_Training2 <- data.frame(model.matrix(~., data=minPrice_Clean_Training_prev2))

# Adding pixels_x*pixels_y, discrete_gpu, removing os

clean3_knn$pixels_xy = clean3_knn$pixels_x*clean3_knn$pixels_y

maxPrice_Clean_Training_prev3 <- clean3_knn %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, pixels_xy, discrete_gpu, max_price)
maxPrice_Clean_Training3 <- data.frame(model.matrix(~., data=maxPrice_Clean_Training_prev3))

minPrice_Clean_Training_prev3 <- clean3_knn %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, pixels_xy, discrete_gpu, min_price)
minPrice_Clean_Training3 <- data.frame(model.matrix(~., data=minPrice_Clean_Training_prev3))

# Adding pixels_x*pixels_y, discrete_gpu, gpu_benchmark_score, cpu_benchmark_score, removing os ,removing dkeyboard

clean6$pixels_xy = clean6$pixels_x*clean6$pixels_y

maxPrice_Clean_Training_prev4 <- clean6 %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, pixels_xy, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, max_price)
maxPrice_Clean_Training4 <- data.frame(model.matrix(~., data=maxPrice_Clean_Training_prev4))

minPrice_Clean_Training_prev4 <- clean6 %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, pixels_xy, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, min_price)
minPrice_Clean_Training4 <- data.frame(model.matrix(~., data=minPrice_Clean_Training_prev4))


#-------- Data normalization -------------------

index_Categ <- match(c("brand", "touchscreen", "dkeyboard", "os", "max_price", "min_price"), names(clean3_knn))
preProcValues <- preProcess(clean3_knn[-index_Categ], method = "range")

trainScaled <- predict(preProcValues, clean3_knn)
glimpse(trainScaled)

testScaled <- predict(preProcValues, clean_test)
glimpse(testScaled)


#------Repeated K-Fold Cross Validation (K = 20, repeats = 3)----------------

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
                              number = 20, repeats = 3)



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

##### Train the model 9 Parallel Random Forest: with pixels_x and discrete_gpu, removing os
model9_max <- train(max_price ~ . , data = maxPrice_Clean_Training2,
                    method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 10 Parallel Random Forest: with pixels_xy and discrete_gpu, removing os
model10_max <- train(max_price ~ . , data = maxPrice_Clean_Training3,
                    method = "parRF", trControl = train.control, metric = "MAE")
##### Train the model 11 Parallel Random Forest: with pixels_xy and discrete_gpu, removing os
model11_max <- train(max_price ~ . , data = maxPrice_Clean_Training4,
                     method = "parRF", trControl = train.control, metric = "MAE")

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

##### Train the model 9 Parallel Random Forest: with pixels_x and discrete_gpu, removing os
model9_min <- train(min_price ~ . , data = minPrice_Clean_Training2,
                    method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 10 Parallel Random Forest: with pixels_xy and discrete_gpu, removing os
model10_min <- train(min_price ~ . , data = minPrice_Clean_Training3,
                    method = "parRF", trControl = train.control, metric = "MAE")
##### Train the model 11 Parallel Random Forest: with pixels_xy and discrete_gpu, removing os
model11_min <- train(min_price ~ . , data = minPrice_Clean_Training4,
                     method = "parRF", trControl = train.control, metric = "MAE")

#------- Summarize the results ----------------

print(model1_max$results$MAE+model1_min$results$MAE)
print(model2_max$results$MAE+model2_min$results$MAE)
print(model3_max$results$MAE+model3_min$results$MAE)
print(min(model4_max$results$MAE+model4_min$results$MAE))
print(min(model5_max$results$MAE+model5_min$results$MAE))
print(min(model6_max$results$MAE+model6_min$results$MAE))
print(min(model7_max$results$MAE+model7_min$results$MAE))
print(min(model8_max$results$MAE+model8_min$results$MAE))

print(min(model9_max$results$MAE+model9_min$results$MAE)) #Changed some features: with pixels_x and discrete_gpu, removing os
print(min(model10_max$results$MAE+model10_min$results$MAE)) #Changed some features: with pixels_xy and discrete_gpu, removing os
print(min(model11_max$results$MAE+model11_min$results$MAE)) # Adding pixels_x*pixels_y, discrete_gpu, gpu_benchmark_score,
                                                            # cpu_benchmark_score, removing os ,removing dkeyboard
# -------- Prediction of test data --------------------

clean_test_knn$pixels_xy = clean_test_knn$pixels_x*clean_test_knn$pixels_y

# Test data not normalized
Test_prev <- clean_test_knn %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, pixels_xy, discrete_gpu)
Price_Test <- data.frame(model.matrix(~., data=Test_prev))
glimpse(Test_prev)
glimpse(Price_Test)
model.matrix(~., data=Test_prev)

# Test data normalized
NormTest_prev <- clean_test_knn %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os)
Price_NormTest <- data.frame(model.matrix(~., data=NormTest_prev))

#Adding missing columns (use corresponding training set)
missingcol <- names(maxPrice_Clean_Training3[!(names(maxPrice_Clean_Training3[, !(names(maxPrice_Clean_Training3) == "max_price")]) %in% names(Price_Test))])
Price_Test[missingcol] <- 0
Price_NormTest[missingcol] <- 0


# Example of Prediction of min_price
predict(model1_min, Price_NormTest, type = "raw") #Linear regression should reference the Normalized Test data - Decision tress to not Normalized

# Example of Prediction of max_price
predict(model1_max, Price_NormTest, type = "raw") #Linear regression should reference the Normalized Test data - Decision tress to not Normalized


# ----------- Results ------------------

id_test <- clean_test_knn %>% select(id)

bothModels <- list(model10_min ,model10_max)
pred <- data.frame(predict(bothModels, Price_Test, type = "raw")) #Parallel Random Forest (best so far)
names(pred) <- c("MIN","MAX")

results <- cbind(id_test,pred)
results

write.csv(results, file = "Model 2(Parallel Random Forest).csv", row.names = F)



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
