#---------Setup Libraries------------------------------------
# install.packages("tidyverse")
# install.packages("naniar")
# install.packages("VIM")
# install.packages("DMwR")
# install.packages("caret")

#---------Load Libraries ------------------------------------
library(plyr)
library(tidyverse)
library(naniar)
library(VIM)
library(DMwR)
library(caret)
library(PerformanceAnalytics)               
library(xlsx)
library(RCurl)

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
clean5$cpu_benchmark_score[is.na(clean5$benchmark_score)] <- 500
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


# #-------Split Train Data to train/test subsets (80/20 percent) ----------------------
# require(caTools)
# set.seed(741)
# sample = sample.split(clean3_knn$id,SplitRatio = 0.8)
# training_subset =subset(clean3_knn,sample ==TRUE)
# test_subset = subset(clean3_knn,sample ==FALSE)
# 
# 
# #-------Prepare Test Data-----------------------------------
# colnames(test_df)[12] <- "dkeyboard"
# colnames(test_df)[1] <- "id"
# glimpse(test_df)
# sum(is.na(test_df))
# aggr(x=test_df[,6:20])
# clean_test <- test_df
# clean_test$screen_surface <- mapvalues(clean_test$screen_surface,c("glossy","matte"),c("Glossy","Matte"))


# #------Repeated K-Fold Cross Validation (K = 10, repeats = 3)----------------
# 
# # Selecting only the features to use
# maxPrice_Clean_Training <- training_subset %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os, max_price)
# glimpse(maxPrice_Clean_Training)
# 
# minPrice_Clean_Training <- training_subset %>% select(brand, touchscreen, screen_size , weight, ram, storage, dkeyboard, ssd, os, min_price)
# glimpse(minPrice_Clean_Training)
# 
# # Training control definition
# set.seed(123)
# train.control <- trainControl(method = "repeatedcv",
#                               number = 10, repeats = 3)

# #--------Models for maxPrice -----------------
# 
# ##### Train the model 1 (Linear regression)
# model1_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
#                 method = "lm", trControl = train.control, metric = "MAE") #warning a lot of features
# 
# ##### Train the model 2 (Generalized Linear Model without func specified -> could be improved)
# model2_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
#                 method = "glm", trControl = train.control, metric = "MAE") #warning a lot of features
# 
# ##### Train the model 3 (GLM with Step AIC)
# model3_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
#                 method = "glmStepAIC", trControl = train.control, metric = "MAE")
# 
# ##### Train the model 4 (Elastic net (glm))
# model4_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
#                 method = "glmnet", trControl = train.control, metric = "MAE")
# 
# #--------Models for min_price -----------------
# 
# model1_min <- train(min_price ~ . , data = minPrice_Clean_Training,
#                 method = "lm", trControl = train.control, metric = "MAE") #warning a lot of features
# 
# ##### Train the model 2 (Generalized Linear Model without func specified -> could be improved)
# model2_min <- train(min_price ~ . , data = minPrice_Clean_Training,
#                 method = "glm", trControl = train.control, metric = "MAE") #warning a lot of features
# 
# ##### Train the model 3 (GLM with Step AIC)
# model3_min <- train(min_price ~ . , data = minPrice_Clean_Training,
#                 method = "glmStepAIC", trControl = train.control, metric = "MAE")
# 
# ##### Train the model 4 (Elastic net (glm))
# model4_min <- train(min_price ~ . , data = minPrice_Clean_Training,
#                 method = "glmnet", trControl = train.control, metric = "MAE")
# 
# 
# #------- Summarize the results----------------
# 
# print((model1_max$results$MAE+model1_min$results$MAE)/2)
# print((model2_max$results$MAE+model2_min$results$MAE)/2)
# print((model3_max$results$MAE+model3_min$results$MAE)/2)
# print(min((model4_max$results$MAE+model4_min$results$MAE)/2))
# 
