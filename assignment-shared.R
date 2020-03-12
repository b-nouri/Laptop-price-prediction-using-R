#---------Setup Libraries------------------------------------

#install.packages("tidyverse")
#install.packages("naniar")
#install.packages("VIM")
#install.packages("DMwR")

#---------Load Libraries ------------------------------------

library(tidyverse)
library(naniar)
library(VIM)
library(DMwR)

#--------Load Data-------------------------------------------

train_df <- read.csv("https://raw.githubusercontent.com/behnouri/lprice-prediction/master/train.csv", na.strings = c("", "NA"))
test_df <- read.csv("https://raw.githubusercontent.com/behnouri/lprice-prediction/master/train.csv", na.strings=c("NA",""))

#--------Prepare Train Data---------------------------------
head(train_df)
sum(is.na(train_df))
colnames(train_df)[12] <- "dkeyboard"

vis_miss(train_df,cluster= TRUE)
gg_miss_var(train_df)
gg_miss_case(train_df)


rown_four_nulls <- as.integer(rownames(train_df[rowSums(is.na(train_df[])) == 4,]))
clean2 <- train_df[-c(rown_four_nulls),]
gg_miss_var(clean2)
gg_miss_case(clean2)

clean2$screen_surface <- mapvalues(clean2$screen_surface,c("glossy", "matte"), c("Glossy", "Matte"))
glimpse(clean2)

aggr(x = clean2[,8:20])
glimpse(clean2)
clean3_knn <- knnImputation(clean2)
aggr(x=clean3_knn)

#-------------Prepare Test Data-----------------------------------
colnames(test_df)[12] <- "dkeyboard"
glimpse(test_df)
sum(is.na(test_df))
aggr(x=test_df[,7:17])
clean_test <- test_df
clean_test$screen_surface <- mapvalues(clean_test$screen_surface,c("glossy","matte"),c("Glossy","Matte"))
