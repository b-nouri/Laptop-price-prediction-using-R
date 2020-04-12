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

new.cpu <- data.frame(cpu_model = c("Intel Pentium Gold 4415Y", "Intel Pentium Gold 4417U"),
                      cpu_benchmark_score = c(3800, 3900))
cpu_df <- rbind(cpu_df, new.cpu)
cpu_df <- cpu_df[!is.na(cpu_df$cpu_benchmark_score),]
gpu_df <- gpu_df[!is.na(gpu_df$gpu_benchmark_score),]
# cpu_df$cpu_benchmark_score <- scale(cpu_df$cpu_benchmark_score,center = TRUE)
# gpu_df$gpu_benchmark_score <- scale(gpu_df$gpu_benchmark_score,center = TRUE)
# 
# boxplot(cpu_df$cpu_benchmark_score)
# boxplot(gpu_df$gpu_benchmark_score)

###############################Prepare Train Data#############################################
colnames(train_df)[12] <- "dkeyboard"
colnames(train_df)[1] <- "id"

##--------------------NA Values for TRAIN DATA------------------------------------
##--------------------Remove Rows with more than 4 nulls--------------------------
rown_four_nulls <- as.integer(rownames(train_df[rowSums(is.na(train_df[])) == 4,]))
clean2 <- train_df[-c(rown_four_nulls),]
gg_miss_var(clean2)
gg_miss_case(clean2)

##-------------------Use Knn for imputing null values-----------------------------
aggr(x = clean2[,8:20])
clean3_knn <- knnImputation(clean2)
aggr(x=clean3_knn)

##-----------------Screen Surface for TRAIN DATA----------------------------------------------------
clean3_knn$screen_surface <- tolower(clean3_knn$screen_surface)

##----------------Screen Resolution for TRAIN DATA--------------------------------------------
clean4 <- clean3_knn %>%
  mutate(resolution = NA)

clean4 <- clean4 %>%
  mutate(resolution= ifelse(pixels_x==1366 & pixels_y==768,"HD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1600 & pixels_y==900,"HD+",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1920 & pixels_y==1080,"FHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2304 & pixels_y==1440,"Retina",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2560 & pixels_y==1440,"QHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2560 & pixels_y==1600,"Retina",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2880 & pixels_y==1800,"Retina",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==3000 & pixels_y==2000,"PixelSense",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==3200 & pixels_y==1800,"QHD+",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==3840 & pixels_y==2160,"UHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1920 & pixels_y==1280,"FHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2160 & pixels_y==1440,"FHD+",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1280 & pixels_y==800,"HD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1920 & pixels_y==1280,"FHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2400 & pixels_y==1600,"QHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2736 & pixels_y==1824,"UHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==3072 & pixels_y==1920,"Retina",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1920 & pixels_y==1200,"FHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2256 & pixels_y==1504,"PixelSense",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2736 & pixels_y==1824,"PixelSense",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1440 & pixels_y==900,"airhd",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==3240 & pixels_y==2160,"PixelSense",resolution))

##------------------Screen Size for TRAIN DATA---------------------------------------------------
clean4 <- clean4 %>%
  mutate(screen_size= ifelse(screen_size>=10 & screen_size<=10.7,10,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=10.8 & screen_size<=11.7,11,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=11.8 & screen_size<=12.6,12,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=12.7 & screen_size<=13.6,13,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=13.7 & screen_size<=14.6,14,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=13.7 & screen_size<=14.6,14,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=14.7 & screen_size<=15.6,15,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=15.7 & screen_size<=16.6,16,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=16.7 & screen_size<=17.6,17,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=17.7 & screen_size<=18.6,18,screen_size))

##---------------------Display Type---------------------------------------------
clean4$name <- tolower(clean4$name)
clean4 <- clean4 %>%
  mutate(display_type= "unkonwn") %>%
  mutate(display_type= ifelse(grepl("lcd",name),"lcd",display_type)) %>%
  mutate(display_type= ifelse(grepl("led",name),"led",display_type)) %>%
  mutate(display_type= ifelse(grepl("oled",name),"oled",display_type)) %>%
  mutate(display_type= ifelse(grepl("ips",name),"ips",display_type)) %>%
  mutate(display_type= ifelse(brand=="Apple" & resolution=="Retina","ips",display_type)) %>%
  mutate(display_type= ifelse(brand=="Apple" & (resolution=="HD"|resolution=="airhd"),"led",display_type)) %>%
  mutate(display_type= ifelse(grepl("Microsoft Surface",base_name),"ips",display_type))

##---------------------Brand Train Data-----------------------------------------
clean4$brand <- as.character(clean4$brand)
clean4 <- clean4 %>%
  mutate(brand_clean= brand) %>%
  mutate(brand_clean= ifelse(grepl("Eluktronics",base_name),"eluktronics",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Sager",base_name),"sager",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Prostar",base_name),"prostar",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Jumper",brand),"Other",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("RCA",brand),"Other",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Toshiba",brand),"Other",brand_clean))

brand_t <- clean4 %>%
  mutate(price=(max_price+min_price)/2) %>%
  group_by(brand_clean) %>%
  summarise(brand_mean = mean(price))
clean4 <- left_join(clean4,brand_t)
clean4 <- clean4 %>%
  mutate(brand_mean= ifelse(brand_mean <=300,300,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=450 & brand_mean >300,400,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=650 & brand_mean >450,600,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=900 & brand_mean >650,750,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=1200 & brand_mean >900,950,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=1400 & brand_mean >1200,1300,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=1600 & brand_mean >1400,1500,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean >2000,2000,brand_mean))


##---------------------CPU Scores-----------------------------------------------
clean4<-clean4 %>%
  mutate(cpu_details,cpu_clean= gsub("\\s*(\\d[.]\\d*)\\s*(GHz|ghz|Ghz|Ghz|gHz).*","",clean4$cpu_details))

cpu_df<-cpu_df %>%
  mutate(cpu_model,cpu_clean= gsub("\\s*([@]).*|\\s*(APU).*","",cpu_df$cpu_model))

clean5 <- clean4 %>%
  left_join(cpu_df,by="cpu_clean")

clean5$cpu_model <- as.character(clean5$cpu_model)
clean5$cpu_benchmark_score[is.na(clean5$cpu_benchmark_score)] <- 500
clean5$cpu_benchmark_score[clean5$cpu_details=="Intel Pentium Gold 4415Y"] <- 3800
clean5$cpu_model[is.na(clean5$cpu_model)] <- "other"

b <- c(-Inf, 1750, 2900,6600,7400,9100, Inf)
names <- c("1", "2", "3","4","5","6")
clean5$cpu_benchmark <- cut(clean5$cpu_benchmark_score, breaks = b, labels = names)

###--------------GPU Scores for TRAIN DATA-----------------------------------------------
gpu_df[,1] <- gsub(" with", "", gpu_df$gpu_model)
gpu_df[,1] <- gsub(" Design", "", gpu_df$gpu_model)

clean6 <- clean5
clean6<-clean6 %>%
  mutate(gpu_model= gsub("NVIDIA ","",clean6$gpu)) %>%
  mutate(gpu_model= ifelse(grepl("AMD",clean6$gpu),gsub("AMD ","",clean6$gpu),gpu_model))

clean6$gpu_model <- gsub("GeFoce", "GeForce", clean6$gpu_model)
clean6$gpu_model <- gsub("GTX1070", "GTX 1070", clean6$gpu_model)
clean6$gpu_model <- gsub("Radeon Pro 555X", "Radeon Pro 555", clean6$gpu_model)

clean6 <- clean6 %>%
  left_join(gpu_df,by="gpu_model")

geforce_df <- filter(clean6, grepl('GeForce',clean6$gpu))
geforce_mean_score <- mean(geforce_df$gpu_benchmark_score, na.rm =TRUE)

clean6[is.na(clean6$gpu_benchmark_score) & grepl("GeForce",clean6$gpu_model),"gpu_benchmark_score"] <- geforce_mean_score
clean6[is.na(clean6$gpu_benchmark_score) & grepl("Intel HD",clean6$gpu_model),"gpu_benchmark_score"] <- 800
clean6[is.na(clean6$gpu_benchmark_score) & grepl("Radeon R",clean6$gpu_model),"gpu_benchmark_score"] <- 1200
clean6[is.na(clean6$gpu_benchmark_score) & clean6$discrete_gpu == 0 ,"gpu_benchmark_score"] <- 500
gpu <- clean6 %>%
  select(gpu,gpu_model,max_price,discrete_gpu,gpu_benchmark_score)

b <- c(-Inf, 800, 1600,9900, Inf)
names <- c("1", "2", "3","4")
clean6$gpu_benchmark <- cut(clean6$gpu_benchmark_score, breaks = b, labels = names)

##-------------------Base Name for TRAIN DATA--------------------------------------------------------
library(stringr)
clean6$base_name <- tolower(clean6$base_name)
clean6$name <- tolower(clean6$name)
base_nam <- clean6 %>%
  mutate(base_name_clean= base_name) %>%
  mutate(base_name_clean= ifelse(grepl("asus rog gl702vs",base_name_clean),"ASUS ROG Strix GL702VS",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("asus 14 eeebook",base_name_clean),"asus eeebook 14",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("asus zenbook 3 deluxe ux490ua",base_name_clean),"asus zenbook 3",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("dell i3558-9136blk",base_name_clean),"Dell Inspiron 15.6 Touch-Screen Laptop Intel Core",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("dell g3",base_name_clean),"Dell g",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("dell g5",base_name_clean),"Dell g",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("dell g5",base_name_clean),"Dell g",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("acer cb3-532",base_name_clean),"acer chromebook cb3-532",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("asus c302ca-dhm4",base_name_clean),"asus chromebook c302ca-dhm4",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("acer cb3-531-c4a5",base_name_clean),"acer chromebook cb3-531-c4a5",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("asus c300sa",base_name_clean),"asus chromebook c300sa",base_name_clean)) %>%
  mutate(base_name_clean= gsub("^.*dell xps\\S+.*","Dell xps",base_name_clean)) %>%
  mutate(base_name_clean= gsub("lenovo 100e","lenovo",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("dell inspiron chromebook",base_name_clean),"dell chromebook",base_name_clean))

base_dd <- clean6 %>%
  select(base_name)
base_nam <- base_nam %>%
  mutate(base_name_clean= gsub("\\s*([(]).*|\\s*([-]).*","",base_nam$base_name_clean)) 

base_nam <- base_nam %>%
  mutate(base_name_clean2= ifelse(grepl("hp",base_name) & grepl("x360",base_name),gsub("x360","flip",base_nam$base_name_clean),base_name_clean)) %>%
  mutate(base_name_clean = base_name_clean2)

base_nam$base_name_clean <- tolower(base_nam$base_name_clean)

base_nam <- base_nam %>%
  mutate(base_name_clean=ifelse(grepl("acer",base_name),str_extract(base_nam$base_name_clean,"^(?=.*\\bacer\\b)(?:\\S+\\s){2}|^(?=.*\\bacer\\b)(?:\\S+){1}"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("alienware",base_name),str_extract(base_nam$base_name_clean,"(\\S+\\s){2}|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("alienware\\s\\D+\\d+",base_name),str_extract(base_nam$base_name_clean,"\\S+\\s\\D+"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("asus",base_name),str_extract(base_nam$base_name_clean,"(\\S+\\s){2,3}|(\\S+\\s\\S+){1,2}"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("asus x5.*",base_name),"asus x5",base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("asus fx.*",base_name),"asus fx",base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("asus\\s[q]\\d+.*",base_name),"asus q",base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("dell",base_name),str_extract(base_nam$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("google",base_name),str_extract(base_nam$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("hp",base_name_clean) & !grepl("flip",base_name_clean),str_extract(base_nam$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)|^(\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("hp",base_name_clean) & grepl("flip",base_name_clean),str_extract(base_nam$base_name_clean,"^(\\S+\\s){3}|^(\\S+\\s\\S+\\s\\S+)|^(\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("hp\\s\\d+",base_name),"hp",base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("huawei",base_name),str_extract(base_nam$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("lg",base_name),str_extract(base_nam$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("microsoft",base_name),str_extract(base_nam$base_name_clean,"^(\\w+\\s+){3}|^(\\S+\\s\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("msi",base_name),str_extract(base_nam$base_name_clean,"^(\\S+\\s+\\D+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("prostar",base_name),str_extract(base_nam$base_name_clean,"^(\\S+\\s+\\D+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("sager",base_name),str_extract(base_nam$base_name_clean,"^(\\S+\\s+\\D+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("samsung",base_name),str_extract(base_nam$base_name_clean,"^(\\w+\\s+){3}|^(\\S+\\s\\S+\\s\\S+)|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("samsung\\s\\S+\\s\\d+",base_name),str_extract(base_nam$base_name_clean,"samsung\\s\\S+"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("razer",base_name),str_extract(base_nam$base_name_clean,"^(\\w+\\s+){3}|^(\\S+\\s\\S+\\s\\S+)|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("jumper",base_name),str_extract(base_nam$base_name_clean,"^(\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("toshiba",base_name),str_extract(base_nam$base_name_clean,"^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("lenovo",base_name),str_extract(base_nam$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)|^(\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("rca",base_name),str_extract(base_nam$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)|^(\\S+)"),base_name_clean)) %>%
  select(brand,base_name,base_name_clean,max_price,name,touchscreen)

base_nam <- base_nam %>%
  mutate(base_name_clean= gsub("flip","",base_name_clean))

base_nam$base_name_clean <- str_squish(base_nam$base_name_clean)
unique(base_nam$base_name_clean)
clean6$base_name_clean <- base_nam$base_name_clean

#--------- 2-in-1 laptops --------------------------------------------
clean6 <- clean6 %>%
  mutate(x360 = ifelse(grepl("2-in-1",name)|grepl("x360",name)|grepl("transformer",name)|grepl("convertible",name)|grepl("flip",name)|
                         grepl("2-in-1",base_name)|grepl("x360",base_name)|grepl("transformer",base_name)|grepl("convertible",base_name)|grepl("flip",base_name)
                       ,1,0))

##------------------weight for Train Data---------------------------------------------------
clean6 <- clean6 %>%
  mutate(weight_clean= ifelse(weight<3,"Up to 3 Pounds",weight)) %>%
  mutate(weight_clean= ifelse(weight>=3 & weight<4,"3 to 3.9 Pounds",weight_clean)) %>%
  mutate(weight_clean= ifelse(weight>=4 & weight<5,"4 to 4.9 Pounds",weight_clean)) %>%
  mutate(weight_clean= ifelse(weight>=5 & weight<6,"5 to 5.9 Pounds",weight_clean)) %>%
  mutate(weight_clean= ifelse(weight>=6 & weight<7,"6 to 6.9 Pounds",weight_clean)) %>%
  mutate(weight_clean= ifelse(weight>=7 & weight<8,"7 to 7.9 Pounds",weight_clean)) %>%
  mutate(weight_clean= ifelse(weight>=8 ,"8 Pounds & Above",weight_clean))

#--------- Factorising Train Data-----------------------------------------------
clean6$screen_size <- as.factor(clean6$screen_size)
clean6$os <-as.factor(clean6$os)
clean6$resolution <- as.factor(clean6$resolution)
clean6$weight_clean <- as.factor(clean6$weight_clean)

#--------- Price variation and Percentage change -------------------

# Price variation
clean6 <- clean6 %>%
  mutate(price_variation = max_price - min_price)

# Price percentage variation based on min_price
clean6 <- clean6 %>%
  mutate(price_percentage_variation_min = (max_price - min_price)/min_price)

# Price percentage variation based on max_price
clean6 <- clean6 %>%
  mutate(price_percentage_variation_max = (max_price - min_price)/max_price)

# Average price
clean6 <- clean6 %>%
  mutate(ave_price = (max_price + min_price)/2)

hist(clean6$price_variation)
table(clean6$price_variation)

hist(clean6$price_percentage_variation_min)
table(clean6$price_percentage_variation_min)

clean6[clean6$id == 8789 |clean6$id == 20741,]

#-------Split Train Data to train/test subsets (80/20 percent) --------- // Currently not use because K-Folds creates a validation set
#require(caTools)
#set.seed(741)
#sample = sample.split(clean3_knn$id,SplitRatio = 0.8)
#training_subset =subset(clean3_knn,sample ==TRUE)
#test_subset = subset(clean3_knn,sample ==FALSE)


###################################Prepare Test Data###########################################################
colnames(test_df)[12] <- "dkeyboard"
colnames(test_df)[1] <- "id"
glimpse(test_df)
sum(is.na(test_df))
aggr(x=test_df[,6:20])
clean_test <- test_df

##------------Knn imputation of tet data---------------------------------------
clean_test_knn <- knnImputation(clean_test)
aggr(x=clean_test_knn)

##----------------Screen Surface for Test Data------------------------
clean_test_knn$screen_surface <- tolower(clean_test_knn$screen_surface)

##----------------Screen Resolution for TEST DATA--------------------------------------------
#---outlier width and lenght----------------------
clean_test1<-clean_test_knn
clean_test1[133,"pixels_x"] <- 1366
clean_test1[133,"pixels_y"] <- 768
clean_test1[203,"pixels_x"] <- 1366
clean_test1[203,"pixels_y"] <- 768

clean_test1 <- clean_test1 %>%
  mutate(resolution = NA)

clean_test1 <- clean_test1 %>%
  mutate(resolution= ifelse(pixels_x==1366 & pixels_y==768,"HD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1600 & pixels_y==900,"HD+",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1920 & pixels_y==1080,"FHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2304 & pixels_y==1440,"Retina",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2560 & pixels_y==1440,"QHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2560 & pixels_y==1600,"Retina",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2880 & pixels_y==1800,"Retina",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==3000 & pixels_y==2000,"PixelSense",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==3200 & pixels_y==1800,"QHD+",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==3840 & pixels_y==2160,"UHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1920 & pixels_y==1280,"FHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2160 & pixels_y==1440,"FHD+",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1280 & pixels_y==800,"HD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1920 & pixels_y==1280,"FHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2400 & pixels_y==1600,"QHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2736 & pixels_y==1824,"UHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==3072 & pixels_y==1920,"Retina",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1920 & pixels_y==1200,"FHD",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2256 & pixels_y==1504,"PixelSense",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==2736 & pixels_y==1824,"PixelSense",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1440 & pixels_y==900,"airhd",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==3240 & pixels_y==2160,"PixelSense",resolution)) %>%
  mutate(resolution= ifelse(pixels_x==1800 & pixels_y==1200,"PixelSense",resolution))

##------------------Screen Size for TEST DATA---------------------------------------------------
clean_test1 <- clean_test1 %>%
  mutate(screen_size= ifelse(screen_size>=10 & screen_size<=10.7,10,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=10.8 & screen_size<=11.7,11,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=11.8 & screen_size<=12.6,12,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=12.7 & screen_size<=13.6,13,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=13.7 & screen_size<=14.6,14,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=13.7 & screen_size<=14.6,14,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=14.7 & screen_size<=15.6,15,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=15.7 & screen_size<=16.6,16,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=16.7 & screen_size<=17.6,17,screen_size)) %>%
  mutate(screen_size= ifelse(screen_size>=17.7 & screen_size<=18.6,18,screen_size))
##---------------------Brand Train Data-----------------------------------------
clean4$brand <- as.character(clean4$brand)
clean4 <- clean4 %>%
  mutate(brand_clean= brand) %>%
  mutate(brand_clean= ifelse(grepl("Eluktronics",base_name),"eluktronics",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Sager",base_name),"sager",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Prostar",base_name),"prostar",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Jumper",brand),"Other",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("RCA",brand),"Other",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Toshiba",brand),"Other",brand_clean))


##----------------------------brand Test Data---------------------------------------
clean_test1$brand <- as.character(clean_test1$brand)
clean_test1 <- clean_test1 %>%
  mutate(brand_clean= brand) %>%
  mutate(brand_clean= ifelse(grepl("Eluktronics",base_name),"eluktronics",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Sager",base_name),"sager",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Prostar",base_name),"prostar",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Jumper",brand),"Other",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("RCA",brand),"Other",brand_clean)) %>%
  mutate(brand_clean= ifelse(grepl("Toshiba",brand),"Other",brand_clean))

brand_t <- clean4 %>%
  mutate(price=(max_price+min_price)/2) %>%
  group_by(brand_clean) %>%
  summarise(brand_mean = mean(price))
clean_test1 <- left_join(clean_test1,brand_t)
clean_test1 <- clean_test1 %>%
  mutate(brand_mean= ifelse(brand_mean <=300,300,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=450 & brand_mean >300,400,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=650 & brand_mean >450,600,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=900 & brand_mean >650,750,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=1200 & brand_mean >900,950,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=1400 & brand_mean >1200,1300,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean <=1600 & brand_mean >1400,1500,brand_mean)) %>%
  mutate(brand_mean= ifelse(brand_mean >2000,2000,brand_mean))

#--------------CPU Scores for test data -----------------------------------------
clean_test1 <-clean_test1 %>%
  mutate(cpu_details,cpu_clean= gsub("\\s*(\\d[.]\\d*)\\s*(GHz|ghz|Ghz|Ghz|gHz).*","",clean_test1$cpu_details))

cpu_df<-cpu_df %>%
  mutate(cpu_model,cpu_clean= gsub("\\s*([@]).*|\\s*(APU).*","",cpu_df$cpu_model))

clean_test2 <- clean_test1 %>%
  left_join(cpu_df,by="cpu_clean")

clean_test2$cpu_model <- as.character(clean_test2$cpu_model)
clean_test2$cpu_benchmark_score[is.na(clean_test2$cpu_benchmark_score)] <- 500
clean_test2$cpu_model[is.na(clean_test2$cpu_model)] <- "other"

b <- c(-Inf, 1750, 2900,6600,7400,9100, Inf)
names <- c("1", "2", "3","4","5","6")
clean_test3$cpu_benchmark <- cut(clean_test3$cpu_benchmark_score, breaks = b, labels = names)

#--------------GPU Scores for test data -----------------------------------------------
gpu_df[,1] <- gsub(" with", "", gpu_df$gpu_model)
gpu_df[,1] <- gsub(" Design", "", gpu_df$gpu_model)

clean_test3 <- clean_test2
clean_test3<-clean_test3 %>%
  mutate(gpu_model= gsub("NVIDIA ","",clean_test3$gpu)) %>%
  mutate(gpu_model= ifelse(grepl("AMD",clean_test3$gpu),gsub("AMD ","",clean_test3$gpu),gpu_model))

clean_test3$gpu_model <- gsub("GeFoce", "GeForce", clean_test3$gpu_model)
clean_test3$gpu_model <- gsub("GTX1070", "GTX 1070", clean_test3$gpu_model)
clean_test3$gpu_model <- gsub("Radeon Pro 555X", "Radeon Pro 555", clean_test3$gpu_model)

clean_test3 <- clean_test3 %>%
  left_join(gpu_df,by="gpu_model")

geforce_df <- filter(clean_test3, grepl('GeForce',clean_test3$gpu))
geforce_mean_score <- mean(geforce_df$gpu_benchmark_score, na.rm =TRUE)

clean_test3[is.na(clean_test3$gpu_benchmark_score) & grepl("GeForce",clean_test3$gpu_model),"gpu_benchmark_score"] <- geforce_mean_score
clean_test3[is.na(clean_test3$gpu_benchmark_score) & grepl("Intel HD",clean_test3$gpu_model),"gpu_benchmark_score"] <- 800
clean_test3[is.na(clean_test3$gpu_benchmark_score) & grepl("Radeon R",clean_test3$gpu_model),"gpu_benchmark_score"] <- 1200
clean_test3[is.na(clean_test3$gpu_benchmark_score) & grepl("Radeon HD R7",clean_test3$gpu_model),"gpu_benchmark_score"] <- 1100
clean_test3[is.na(clean_test3$gpu_benchmark_score) & clean_test3$discrete_gpu == 0 ,"gpu_benchmark_score"] <- 500
gpu <- clean_test3 %>%
  select(gpu,gpu_model,discrete_gpu,gpu_benchmark_score)

b <- c(-Inf, 800, 1600,9900, Inf)
names <- c("1", "2", "3","4")
clean_test3$gpu_benchmark <- cut(clean_test3$gpu_benchmark_score, breaks = b, labels = names)

#--------- Base_name_for_test_data-------------------------------------------------
clean_test3$base_name <- tolower(clean_test3$base_name)
clean_test3$name <- tolower(clean_test3$name)

base_nam_test <- clean_test3 %>%
  mutate(base_name_clean= base_name) %>%
  mutate(base_name_clean= ifelse(grepl("asus rog gl702vs",base_name_clean),"ASUS ROG Strix GL702VS",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("asus 14 eeebook",base_name_clean),"asus eeebook 14",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("asus zenbook 3 deluxe ux490ua",base_name_clean),"asus zenbook 3",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("dell i3558-9136blk",base_name_clean),"Dell Inspiron 15.6 Touch-Screen Laptop Intel Core",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("dell g3",base_name_clean),"Dell g",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("dell g5",base_name_clean),"Dell g",base_name_clean)) %>%
  mutate(base_name_clean= gsub("^.*dell xps\\S+.*","Dell xps",base_name_clean)) %>%
  mutate(base_name_clean= gsub("delll","dell",base_name_clean)) %>%
  mutate(base_name_clean= gsub("apple macbook pro [(]2019[)]","apple macbook pro 2019",base_name_clean)) %>%
  mutate(base_name_clean= gsub("apple macbook pro 2015","apple macbook pro 2014",base_name_clean)) %>%
  mutate(base_name_clean= gsub("apple macbook pro 2011","apple macbook pro 2012",base_name_clean)) %>%
  mutate(base_name_clean= gsub("apple macbook 2015","apple macbook 2017",base_name_clean)) %>%
  mutate(base_name_clean= gsub("microsoft surface go","microsoft surface 3",base_name_clean)) %>%
  mutate(base_name_clean= gsub("asus rog gl\\d+","asus rog strix ",base_name_clean)) %>%
  mutate(base_name_clean= gsub("asus zenbook 3 ","asus zenbook ",base_name_clean)) %>%
  mutate(base_name_clean= gsub("dell precision","dell latitude",base_name_clean)) %>%
  mutate(base_name_clean= gsub("asus vivobook pro","asus vivobook",base_name_clean)) %>%
  mutate(base_name_clean= gsub("razer blade$","razer blade pro",base_name_clean)) %>%
  mutate(base_name_clean= gsub("lenovo v330","lenovo chromebook",base_name_clean)) %>%
  mutate(base_name_clean= gsub("lenovo 300e","lenovo chromebook",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("dell inspiron chromebook",base_name_clean),"dell chromebook",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("asus transformer mini",base_name_clean),"asus transformer book mini",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("asus l402sa",base_name_clean),"  ASUS Vivobook L402SA",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("alienware area-51m",base_name_clean),"alienware 17 r5",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("samsung chromebook xe303c12",base_name_clean),"samsung chromebook",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("acer cb3-532",base_name_clean),"acer chromebook cb3-532",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("asus c302ca-dhm4",base_name_clean),"asus chromebook c302ca-dhm4",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("acer cb3-531-c4a5",base_name_clean),"acer chromebook cb3-531-c4a5",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("acer r11",base_name_clean),"acer chromebook r11",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("asus c300sa",base_name_clean),"asus chromebook c300sa",base_name_clean)) %>%
  mutate(base_name_clean= ifelse(grepl("samsung notebook flash",base_name_clean),"samsung notebook",base_name_clean))

base_nam_test <- base_nam_test %>%
  mutate(base_name_clean= gsub("\\s*([(]).*|\\s*([-]).*","",base_nam_test$base_name_clean)) 

base_nam_test <- base_nam_test %>%
  mutate(base_name_clean2= ifelse(grepl("hp",base_name) & grepl("x360",base_name),gsub("x360","flip",base_nam_test$base_name_clean),base_name_clean)) %>%
  mutate(base_name_clean = base_name_clean2)

base_nam_test$base_name_clean <- tolower(base_nam_test$base_name_clean)

base_nam_test <- base_nam_test %>%
  mutate(base_name_clean=ifelse(grepl("acer",base_name),str_extract(base_nam_test$base_name_clean,"^(?=.*\\bacer\\b)(?:\\S+\\s){2}|^(?=.*\\bacer\\b)(?:\\S+){1}"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("alienware",base_name),str_extract(base_nam_test$base_name_clean,"(\\S+\\s){2}|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("alienware\\s\\D+\\d+",base_name_clean),str_extract(base_nam_test$base_name_clean,"\\S+\\s\\D+"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("asus",base_name),str_extract(base_nam_test$base_name_clean,"(\\S+\\s){2,3}|(\\S+\\s\\S+){1,2}"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("asus x5.*",base_name),"asus x5",base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("asus fx.*",base_name),"asus fx",base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("asus\\s[q]\\d+.*",base_name),"asus q",base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("dell",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("google",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("hp",base_name_clean) & !grepl("flip",base_name_clean),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)|^(\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("hp",base_name_clean) & grepl("flip",base_name_clean),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s){3}|^(\\S+\\s\\S+\\s\\S+)|^(\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("hp\\s\\d+",base_name),"hp",base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("huawei",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("lg",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("microsoft",base_name),str_extract(base_nam_test$base_name_clean,"^(\\w+\\s+){3}|^(\\S+\\s\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("msi",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s+\\D+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("prostar",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s+\\D+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("sager",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s+\\D+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("samsung",base_name),str_extract(base_nam_test$base_name_clean,"^(\\w+\\s+){3}|^(\\S+\\s\\S+\\s\\S+)|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("samsung\\s\\S+\\s\\d+",base_name),str_extract(base_nam_test$base_name_clean,"samsung\\s\\S+"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("razer",base_name),str_extract(base_nam_test$base_name_clean,"^(\\w+\\s+){3}|^(\\S+\\s\\S+\\s\\S+)|^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("jumper",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("toshiba",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("lenovo",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)|^(\\S+)"),base_name_clean)) %>%
  mutate(base_name_clean=ifelse(grepl("rca",base_name),str_extract(base_nam_test$base_name_clean,"^(\\S+\\s){2}|^(\\S+\\s\\S+)|^(\\S+)"),base_name_clean)) %>%
  select(brand,base_name,base_name_clean,name,touchscreen)

base_nam_test <- base_nam_test %>%
  mutate(base_name_clean= gsub("flip","",base_name_clean))

base_nam_test$base_name_clean <- str_squish(base_nam_test$base_name_clean)
unique(base_nam_test$base_name_clean)
clean_test3$base_name_clean <- base_nam_test$base_name_clean

clean_test3$base_name_clean[!(clean_test3$base_name_clean %in% clean6$base_name_clean)]
#--------- 2-in-1 laptops - test data --------------------------------------------
clean_test3 <- clean_test3 %>%
  mutate(x360 = ifelse(grepl("2-in-1",name)|grepl("x360",name)|grepl("transformer",name)|grepl("convertible",name)|grepl("flip",name)|
                         grepl("2-in-1",base_name)|grepl("x360",base_name)|grepl("transformer",base_name)|grepl("convertible",base_name)|grepl("flip",base_name)
                       ,1,0))

##------------------weight for Test Data---------------------------------------------------
clean_test3 <- clean_test3 %>%
  mutate(weight_clean= ifelse(weight<3,"Up to 3 Pounds",weight)) %>%
  mutate(weight_clean= ifelse(weight>=3 & weight<4,"3 to 3.9 Pounds",weight_clean)) %>%
  mutate(weight_clean= ifelse(weight>=4 & weight<5,"4 to 4.9 Pounds",weight_clean)) %>%
  mutate(weight_clean= ifelse(weight>=5 & weight<6,"5 to 5.9 Pounds",weight_clean)) %>%
  mutate(weight_clean= ifelse(weight>=6 & weight<7,"6 to 6.9 Pounds",weight_clean)) %>%
  mutate(weight_clean= ifelse(weight>=7 & weight<8,"7 to 7.9 Pounds",weight_clean)) %>%
  mutate(weight_clean= ifelse(weight>=8 ,"8 Pounds & Above",weight_clean))

##---------------------Display Type---------------------------------------------
clean_test3$name <- tolower(clean_test3$name)
clean_test3 <- clean_test3 %>%
  mutate(display_type= "unkonwn") %>%
  mutate(display_type= ifelse(grepl("lcd",name),"lcd",display_type)) %>%
  mutate(display_type= ifelse(grepl("led",name),"led",display_type)) %>%
  mutate(display_type= ifelse(grepl("oled",name),"oled",display_type)) %>%
  mutate(display_type= ifelse(grepl("ips",name),"ips",display_type)) %>%
  mutate(display_type= ifelse(brand=="Apple" & resolution=="Retina","ips",display_type)) %>%
  mutate(display_type= ifelse(brand=="Apple" & (resolution=="HD"|resolution=="airhd"),"led",display_type)) %>%
  mutate(display_type= ifelse(grepl("Microsoft Surface",base_name),"ips",display_type))

#--------- Factorising Test Data-----------------------------------------------
clean_test3$screen_size <- as.factor(clean_test3$screen_size)
clean_test3$os <-as.factor(clean_test3$os)
clean_test3$resolution <- as.factor(clean_test3$resolution)
clean_test3$weight_clean <- as.factor(clean_test3$weight_clean)

#--------- Data not normalized ------------------

# Selecting only the features to use
#Features: brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution(pixels_x*pixels_y), discrete_gpu, 
#          cpu_benchmark_score, gpu_benchmark_score

maxPrice_Clean_Training_prev <- clean6 %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, max_price)
maxPrice_Clean_Training <- data.frame(model.matrix(~., data=maxPrice_Clean_Training_prev))

minPrice_Clean_Training_prev <- clean6 %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, min_price)
minPrice_Clean_Training <- data.frame(model.matrix(~., data=minPrice_Clean_Training_prev))



#-------- Data normalization -------------------

index_Response <- match(c("max_price", "min_price", "price_variation", "price_percentage_variation_min","price_percentage_variation_max", "ave_price"), names(clean6))
preProcValues <- preProcess(clean6[-index_Response], method = "range")

trainScaled <- predict(preProcValues, clean6)
glimpse(trainScaled)

testScaled <- predict(preProcValues, clean_test3)
glimpse(testScaled)

# Selecting only the features to use for Normalized data
maxPrice_Norm_Training_prev <- trainScaled %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, max_price)
maxPrice_Norm_Training <- data.frame(model.matrix(~., data=maxPrice_Norm_Training_prev))
maxPrice_Norm_Training

minPrice_Norm_Training_prev <- trainScaled %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, min_price)
minPrice_Norm_Training <- data.frame(model.matrix(~., data=minPrice_Norm_Training_prev))
minPrice_Norm_Training





#------Repeated K-Fold Cross Validation (K = 20, repeats = 3)----------------

# Training control definition
set.seed(123)
train.control <- trainControl(method = "repeatedcv",
                              number = 20, repeats = 3)



#--------Models for maxPrice with Normalized data (except decision tree models) -----------------

#Features: brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu, 
#          cpu_benchmark_score, gpu_benchmark_score


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

##### Train the model 7 Parallel Random Forest  <---------------BEST MODEL SO FAR
model7_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 8 Stochastic Gradient Boosting # warning for some brands (few observations)
model8_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                    method = "gbm", trControl = train.control, metric = "MAE")



#--------Models for min_price with Normalized data (except decision tree models) -----------------

#Features: brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu, 
#          cpu_benchmark_score, gpu_benchmark_score


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

##### Train the model 7 Parallel Random Forest  <---------------BEST MODEL SO FAR
model7_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 8 Stochastic Gradient Boosting # warning for some brands (few observations)
model8_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "gbm", trControl = train.control, metric = "MAE")



#------- Summarize the results ----------------

#Features: brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu, 
#          cpu_benchmark_score, gpu_benchmark_score


print(model1_max$results$MAE+model1_min$results$MAE)
print(model2_max$results$MAE+model2_min$results$MAE)
print(model3_max$results$MAE+model3_min$results$MAE)
print(min(model4_max$results$MAE+model4_min$results$MAE))
print(min(model5_max$results$MAE+model5_min$results$MAE))
print(min(model6_max$results$MAE+model6_min$results$MAE))
print(min(model7_max$results$MAE+model7_min$results$MAE)) # <---------------BEST MODEL SO FAR
print(min(model8_max$results$MAE+model8_min$results$MAE))


#------------- Models to predict price variation -------------

#---- Data not normalized -----
# For price_variation
varPrice_Clean_Training_prev <- clean6 %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, price_variation)
varPrice_Clean_Training <- data.frame(model.matrix(~., data=varPrice_Clean_Training_prev))

#------ Normalized data--------
# For price_variation
varPrice_Norm_Training_prev <- trainScaled %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, price_variation)
varPrice_Norm_Training <- data.frame(model.matrix(~., data=varPrice_Norm_Training_prev))

#------ Models (price variation) -----------
##### Train the model 1 (Linear regression)
model1_varPrice <- train(price_variation ~ . , data = varPrice_Norm_Training,
                    method = "lm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 2 (Generalized Linear Model without func specified -> could be improved)
model2_varPrice <- train(price_variation ~ . , data = varPrice_Norm_Training,
                    method = "glm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 3 (GLM with Step AIC)
model3_varPrice <- train(price_variation ~ . , data = varPrice_Norm_Training,
                    method = "glmStepAIC", trControl = train.control, metric = "MAE")

##### Train the model 4 (Elastic net (glm))
model4_varPrice <- train(price_variation ~ . , data = varPrice_Norm_Training,
                    method = "glmnet", trControl = train.control, metric = "MAE")

##### Train the model 5 Boosted Tree
model5_varPrice <- train(price_variation ~ . , data = varPrice_Clean_Training,
                    method = "bstTree", trControl = train.control, metric = "MAE")

##### Train the model 6 eXtreme Gradient Boosting
model6_varPrice <- train(price_variation ~ . , data = varPrice_Clean_Training,
                    method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 7 Parallel Random Forest  <---------------BEST MODEL SO FAR
model7_varPrice <- train(price_variation ~ . , data = varPrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 8 Stochastic Gradient Boosting # warning for some brands (few observations)
model8_varPrice <- train(price_variation ~ . , data = varPrice_Clean_Training,
                    method = "gbm", trControl = train.control, metric = "MAE")


model1_varPrice$results$MAE
model2_varPrice$results$MAE
model3_varPrice$results$MAE
min(model4_varPrice$results$MAE) #Best model for the variation, but not the best for max_price
min(model5_varPrice$results$MAE) #2nd best model
min(model6_varPrice$results$MAE)
min(model7_varPrice$results$MAE) #4th best model (not much difference between models)
min(model8_varPrice$results$MAE) #3rd best model

actual_max_price <- maxPrice_Clean_Training %>% select(max_price)
actual_min_price <- minPrice_Clean_Training %>% select(min_price)
min_price_pred <- data.frame(predict(model7_min, type = "raw"))
max_price_pred <- data.frame(predict(model7_max, type = "raw"))

var_pred1 <- data.frame(predict(model1_varPrice, type = "raw"))
var_pred2 <- data.frame(predict(model2_varPrice, type = "raw"))
var_pred3 <- data.frame(predict(model3_varPrice, type = "raw"))
var_pred4 <- data.frame(predict(model4_varPrice, type = "raw"))
var_pred5 <- data.frame(predict(model5_varPrice, type = "raw"))
var_pred6 <- data.frame(predict(model6_varPrice, type = "raw"))
var_pred7 <- data.frame(predict(model7_varPrice, type = "raw"))
var_pred8 <- data.frame(predict(model8_varPrice, type = "raw"))

hist(predict(model7_varPrice, type = "raw"))

#Based on min_price
max_price_pred1 <- min_price_pred+var_pred1
names(max_price_pred1) <- "max_price_pred"
max_price_pred2 <- min_price_pred+var_pred2
names(max_price_pred2) <- "max_price_pred"
max_price_pred3 <- min_price_pred+var_pred3
names(max_price_pred3) <- "max_price_pred"
max_price_pred4 <- min_price_pred+var_pred4
names(max_price_pred4) <- "max_price_pred"
max_price_pred5 <- min_price_pred+var_pred5
names(max_price_pred5) <- "max_price_pred"
max_price_pred6 <- min_price_pred+var_pred6
names(max_price_pred6) <- "max_price_pred"
max_price_pred7 <- min_price_pred+var_pred7
names(max_price_pred7) <- "max_price_pred"
max_price_pred8 <- min_price_pred+var_pred8
names(max_price_pred8) <- "max_price_pred"

mean(abs(actual_max_price$max_price-max_price_pred1$max_price_pred)) 
mean(abs(actual_max_price$max_price-max_price_pred2$max_price_pred)) 
mean(abs(actual_max_price$max_price-max_price_pred3$max_price_pred)) 
mean(abs(actual_max_price$max_price-max_price_pred4$max_price_pred)) 
mean(abs(actual_max_price$max_price-max_price_pred5$max_price_pred))
mean(abs(actual_max_price$max_price-max_price_pred6$max_price_pred)) 
mean(abs(actual_max_price$max_price-max_price_pred7$max_price_pred)) #Best model MAE=81.18 (using min_price as base price)
mean(abs(actual_max_price$max_price-max_price_pred8$max_price_pred))

MAE_maxprice_var7 <- mean(abs(actual_max_price$max_price-max_price_pred7$max_price_pred))
print(min(model7_min$results$MAE)+MAE_maxprice_var7) #Using min_price as base price

# Based on max_price
min_price_pred7 <- max_price_pred-var_pred7
names(min_price_pred7) <- "min_price_pred"

mean(abs(actual_min_price$min_price-min_price_pred7$min_price_pred)) #MAE = 75.39 (using max_price as base price)

MAE_minprice_var7 <- mean(abs(actual_min_price$min_price-min_price_pred7$min_price_pred))
print(min(model7_max$results$MAE)+MAE_minprice_var7) #Using max_price as base price

# There is a significant difference between training (validation) MAE (230.6977) and test (348.177)


#------ Models to predict price_percentage_variation -------------

#---- Data not normalized -----
# For price_percentage_variation based on min_price
percentage_varPrice_prev_min <- clean6 %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, price_percentage_variation_min)
percentage_varPrice_Training_min <- data.frame(model.matrix(~., data=percentage_varPrice_prev_min))

# For price_percentage_variation based on max_price
percentage_varPrice_prev_max <- clean6 %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, price_percentage_variation_max)
percentage_varPrice_Training_max <- data.frame(model.matrix(~., data=percentage_varPrice_prev_max))


#------ Normalized data--------
# For price_percentage_variation based on min_price
percentage_varPriceNorm_prev_min <- trainScaled %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, price_percentage_variation_min)
percentage_varPriceNorm_Training_min <- data.frame(model.matrix(~., data=percentage_varPrice_prev_min))

# For price_percentage_variation based on max_price
percentage_varPriceNorm_prev_max <- trainScaled %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, price_percentage_variation_max)
percentage_varPriceNorm_Training_max <- data.frame(model.matrix(~., data=percentage_varPrice_prev_max))

#--------Percentage price variation based on min_price-------
#----- Models -------------
##### Train the model 1 (Linear regression)
model1_perc_varPrice_min <- train(price_percentage_variation_min ~ . , data = percentage_varPriceNorm_Training_min,
                         method = "lm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 2 (Generalized Linear Model without func specified -> could be improved)
model2_perc_varPrice_min <- train(price_percentage_variation_min ~ . , data = percentage_varPriceNorm_Training_min,
                         method = "glm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 3 (GLM with Step AIC)
model3_perc_varPrice_min <- train(price_percentage_variation_min ~ . , data = percentage_varPriceNorm_Training_min,
                         method = "glmStepAIC", trControl = train.control, metric = "MAE")

##### Train the model 4 (Elastic net (glm))
model4_perc_varPrice_min <- train(price_percentage_variation_min ~ . , data = percentage_varPriceNorm_Training_min,
                         method = "glmnet", trControl = train.control, metric = "MAE")

##### Train the model 5 Boosted Tree
model5_perc_varPrice_min <- train(price_percentage_variation_min ~ . , data = percentage_varPrice_Training_min,
                         method = "bstTree", trControl = train.control, metric = "MAE")

##### Train the model 6 eXtreme Gradient Boosting
model6_perc_varPrice_min <- train(price_percentage_variation_min ~ . , data = percentage_varPrice_Training_min,
                         method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 7 Parallel Random Forest  <---------------BEST MODEL SO FAR
model7_perc_varPrice_min <- train(price_percentage_variation_min ~ . , data = percentage_varPrice_Training_min,
                         method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 8 Stochastic Gradient Boosting # warning for some brands (few observations)
model8_perc_varPrice_min <- train(price_percentage_variation_min ~ . , data = percentage_varPrice_Training_min,
                         method = "gbm", trControl = train.control, metric = "MAE")


model1_perc_varPrice_min$results$MAE
model2_perc_varPrice_min$results$MAE
model3_perc_varPrice_min$results$MAE
min(model4_perc_varPrice_min$results$MAE) 
min(model5_perc_varPrice_min$results$MAE)
min(model6_perc_varPrice_min$results$MAE)
min(model7_perc_varPrice_min$results$MAE)
min(model8_perc_varPrice_min$results$MAE) #This is the best model for the variation, but not the best for max_price

perc_var_pred1_min <- data.frame(predict(model1_perc_varPrice_min, type = "raw"))
perc_var_pred2_min <- data.frame(predict(model2_perc_varPrice_min, type = "raw"))
perc_var_pred3_min <- data.frame(predict(model3_perc_varPrice_min, type = "raw"))
perc_var_pred4_min <- data.frame(predict(model4_perc_varPrice_min, type = "raw"))
perc_var_pred5_min <- data.frame(predict(model5_perc_varPrice_min, type = "raw"))
perc_var_pred6_min <- data.frame(predict(model6_perc_varPrice_min, type = "raw"))
perc_var_pred7_min <- data.frame(predict(model7_perc_varPrice_min, type = "raw"))
perc_var_pred8_min <- data.frame(predict(model8_perc_varPrice_min, type = "raw"))

max_price_perc_pred1 <- min_price_pred*(1+perc_var_pred1_min)
names(max_price_perc_pred1) <- "max_price_pred"
max_price_perc_pred2 <- min_price_pred*(1+perc_var_pred2_min)
names(max_price_perc_pred2) <- "max_price_pred"
max_price_perc_pred3 <- min_price_pred*(1+perc_var_pred3_min)
names(max_price_perc_pred3) <- "max_price_pred"
max_price_perc_pred4 <- min_price_pred*(1+perc_var_pred4_min)
names(max_price_perc_pred4) <- "max_price_pred"
max_price_perc_pred5 <- min_price_pred*(1+perc_var_pred5_min)
names(max_price_perc_pred5) <- "max_price_pred"
max_price_perc_pred6 <- min_price_pred*(1+perc_var_pred6_min)
names(max_price_perc_pred6) <- "max_price_pred"
max_price_perc_pred7 <- min_price_pred*(1+perc_var_pred7_min)
names(max_price_perc_pred7) <- "max_price_pred"
max_price_perc_pred8 <- min_price_pred*(1+perc_var_pred8_min)
names(max_price_perc_pred8) <- "max_price_pred"

mean(abs(actual_max_price$max_price-max_price_perc_pred1$max_price_pred)) 
mean(abs(actual_max_price$max_price-max_price_perc_pred2$max_price_pred)) 
mean(abs(actual_max_price$max_price-max_price_perc_pred3$max_price_pred)) 
mean(abs(actual_max_price$max_price-max_price_perc_pred4$max_price_pred)) 
mean(abs(actual_max_price$max_price-max_price_perc_pred5$max_price_pred))
mean(abs(actual_max_price$max_price-max_price_perc_pred6$max_price_pred)) 
mean(abs(actual_max_price$max_price-max_price_perc_pred7$max_price_pred)) #Best model MAE=78.66 (using min_price as base price)
mean(abs(actual_max_price$max_price-max_price_perc_pred8$max_price_pred)) 

MAE_maxprice_perc_var7 <- mean(abs(actual_max_price$max_price-max_price_perc_pred7$max_price_pred))
print(min(model7_min$results$MAE)+MAE_maxprice_perc_var7) #Using min_price as base price (229.9546)


#--------Percentage price variation based on max_price-------

##### Train the model 7 Parallel Random Forest  <---------------BEST MODEL SO FAR
model7_perc_varPrice_max <- train(price_percentage_variation_max ~ . , data = percentage_varPrice_Training_max,
                                  method = "parRF", trControl = train.control, metric = "MAE")

perc_var_pred7_max <- data.frame(predict(model7_perc_varPrice_max, type = "raw"))

min_price_perc_pred7 <- max_price_pred*(1-perc_var_pred7_max)
names(min_price_perc_pred7) <- "min_price_pred"

MAE_minprice_perc_var7 <- mean(abs(actual_min_price$min_price-min_price_perc_pred7$min_price_pred))
print(min(model7_max$results$MAE)+MAE_minprice_perc_var7) #Using max_price as base price (233.39)


#------------- Models to predict average price -------------

#---- Data not normalized -----
# For price_variation
avePrice_Clean_Training_prev <- clean6 %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, ave_price)
avePrice_Clean_Training <- data.frame(model.matrix(~., data=avePrice_Clean_Training_prev))

#------ Normalized data--------
# For price_variation
avePrice_Norm_Training_prev <- trainScaled %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score, ave_price)
avePrice_Norm_Training <- data.frame(model.matrix(~., data=avePrice_Norm_Training_prev))

#------ Models (price variation) -----------
##### Train the model 1 (Linear regression)
model1_avePrice <- train(ave_price ~ . , data = avePrice_Norm_Training,
                         method = "lm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 2 (Generalized Linear Model without func specified -> could be improved)
model2_avePrice <- train(ave_price ~ . , data = avePrice_Norm_Training,
                         method = "glm", trControl = train.control, metric = "MAE") #warning a lot of features

##### Train the model 3 (GLM with Step AIC)
model3_avePrice <- train(ave_price ~ . , data = avePrice_Norm_Training,
                         method = "glmStepAIC", trControl = train.control, metric = "MAE")

##### Train the model 4 (Elastic net (glm))
model4_avePrice <- train(ave_price ~ . , data = avePrice_Norm_Training,
                         method = "glmnet", trControl = train.control, metric = "MAE")

##### Train the model 5 Boosted Tree
model5_avePrice <- train(ave_price ~ . , data = avePrice_Clean_Training,
                         method = "bstTree", trControl = train.control, metric = "MAE")

##### Train the model 6 eXtreme Gradient Boosting
model6_avePrice <- train(ave_price ~ . , data = avePrice_Clean_Training,
                         method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 7 Parallel Random Forest  <---------------BEST MODEL SO FAR
model7_avePrice <- train(ave_price ~ . , data = avePrice_Clean_Training,
                         method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 8 Stochastic Gradient Boosting # warning for some brands (few observations)
model8_avePrice <- train(ave_price ~ . , data = avePrice_Clean_Training,
                         method = "gbm", trControl = train.control, metric = "MAE")


model1_avePrice$results$MAE
model2_avePrice$results$MAE
model3_avePrice$results$MAE
min(model4_avePrice$results$MAE) 
min(model5_avePrice$results$MAE)
min(model6_avePrice$results$MAE)
min(model7_avePrice$results$MAE) #Best model
min(model8_avePrice$results$MAE)

actual_max_price <- maxPrice_Clean_Training %>% select(max_price)
actual_min_price <- minPrice_Clean_Training %>% select(min_price)

ave_pred7 <- data.frame(predict(model7_avePrice, type = "raw"))

#Based on price variation model 4
min_aveprice_pred4 <- ave_pred7-(var_pred4/2)
names(min_aveprice_pred4) <- "min_price_pred"
max_aveprice_pred4 <- ave_pred7+(var_pred4/2)
names(max_aveprice_pred4) <- "max_price_pred"

#Based on price variation model 5
min_aveprice_pred5 <- ave_pred7-(var_pred5/2)
names(min_aveprice_pred5) <- "min_price_pred"
max_aveprice_pred5 <- ave_pred7+(var_pred5/2)
names(max_aveprice_pred5) <- "max_price_pred"

#Based on price variation model 7
min_aveprice_pred7 <- ave_pred7-(var_pred7/2)
names(min_aveprice_pred7) <- "min_price_pred"
max_aveprice_pred7 <- ave_pred7+(var_pred7/2)
names(max_aveprice_pred7) <- "max_price_pred"

#Based on price variation model 8
min_aveprice_pred8 <- ave_pred7-(var_pred8/2)
names(min_aveprice_pred8) <- "min_price_pred"
max_aveprice_pred8 <- ave_pred7+(var_pred8/2)
names(max_aveprice_pred8) <- "max_price_pred"


mean(abs(actual_max_price$max_price-max_aveprice_pred4$max_price_pred))+ mean(abs(actual_min_price$min_price-min_aveprice_pred4$min_price_pred)) 
mean(abs(actual_max_price$max_price-max_aveprice_pred5$max_price_pred))+ mean(abs(actual_min_price$min_price-min_aveprice_pred5$min_price_pred)) 
mean(abs(actual_max_price$max_price-max_aveprice_pred7$max_price_pred))+ mean(abs(actual_min_price$min_price-min_aveprice_pred7$min_price_pred)) #Best model
mean(abs(actual_max_price$max_price-max_aveprice_pred8$max_price_pred))+ mean(abs(actual_min_price$min_price-min_aveprice_pred8$min_price_pred)) 


# -------- Prediction of test data --------------------

# Test data not normalized
Test_prev <- clean_test3 %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score)
Price_Test <- data.frame(model.matrix(~., data=Test_prev))
glimpse(Test_prev)
glimpse(Price_Test)
model.matrix(~., data=Test_prev)

# Test data normalized
NormTest_prev <- testScaled %>% select(brand, touchscreen, screen_size , weight, ram, storage, ssd, resolution, discrete_gpu,cpu_benchmark_score,gpu_benchmark_score)
Price_NormTest <- data.frame(model.matrix(~., data=NormTest_prev))

#Adding missing columns (use corresponding training set)
missingcol <- names(maxPrice_Clean_Training[!(names(maxPrice_Clean_Training[, !(names(maxPrice_Clean_Training) == "max_price")]) %in% names(Price_Test))])
Price_Test[missingcol] <- 0
Price_NormTest[missingcol] <- 0


# -------------------- Results ------------------------

id_test <- clean_test3 %>% select(id)

bothModels <- list(model7_min ,model7_max)
pred <- data.frame(predict(bothModels, Price_Test, type = "raw")) #Parallel Random Forest
names(pred) <- c("MIN","MAX")

results <- cbind(id_test,pred)
results

write.csv(results, file = "Model ####.csv", row.names = F)

# --- With Percentage Price Difference (min_price as base price) ---- 

bothModels2 <- list(model7_min ,model7_perc_varPrice)
pred_prev <- data.frame(predict(bothModels2, Price_Test, type = "raw")) #Parallel Random Forest (best so far)
names(pred_prev) <- c("MIN","Price_perc_dif")

pred1 <- pred_prev %>% select(MIN)
pred1 <- pred1 %>% mutate(MAX = MIN*(1+pred_prev$Price_perc_dif))
pred1

results <- cbind(id_test,pred1)
results

write.csv(results, file = "Model ####.csv", row.names = F)

# --- With Price Difference (min_price as base price) ---- 

pred_prev1 <- data.frame(predict(model7_min, Price_Test, type = "raw")) #Parallel Random Forest
pred_prev2 <- data.frame(predict(model4_varPrice, Price_NormTest, type = "raw")) #Elastic net (glm)
names(pred_prev1) <- "MIN"
names(pred_prev2) <- "Price_dif"

pred2 <- pred_prev1 %>% select(MIN)
pred2 <- pred2 %>% mutate(MAX = MIN+pred_prev2$Price_dif)
pred2

results <- cbind(id_test,pred2)
results

write.csv(results, file = "Model ####.csv", row.names = F)

# --- With Price Difference (max_price as base price) ---- 

bothModels3 <- list(model7_varPrice, model7_max)
pred_prev3 <- data.frame(predict(bothModels3, Price_Test, type = "raw")) #Parallel Random Forest (best so far)
names(pred_prev3) <- c("Price_dif", "MAX")

pred3 <- data.frame(pred_prev3$MAX - pred_prev3$Price_dif)
names(pred3) <- "MIN"
pred3 <- pred3 %>% mutate(MAX = pred_prev3$MAX)
pred3 

results <- cbind(id_test,pred3)
results

write.csv(results, file = "Model 8(PRF with price variation base on max_price).csv", row.names = F)

# --- With average price and price difference ---- 

bothModels4 <- list(model7_varPrice, model7_avePrice)
pred_prev4 <- data.frame(predict(bothModels4, Price_Test, type = "raw")) #Parallel Random Forest (best so far)
names(pred_prev4) <- c("Price_dif", "Price_average")

pred4 <- data.frame(pred_prev4$Price_average - (pred_prev4$Price_dif/2))
names(pred4) <- "MIN"
pred4 <- pred4 %>% mutate(MAX = pred_prev4$Price_average + (pred_prev4$Price_dif/2))
pred4 

results <- cbind(id_test,pred4)
results

write.csv(results, file = "Model 9(PRF with price average and variation).csv", row.names = F)

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

training.n=clean6%>%select_if(is.numeric) #Creating subset of numeric variables to check all the correlations 

#-------------Tranaforming categorical variables ----------
lookup = clean6 %>%                     #creating lookup table for mean of max_price of brands
  group_by(brand) %>%
  summarise(mean_brand = mean(max_price))
clean6 = left_join(clean6, lookup)   #Joining tables 
clean6=clean6[,-3]                   #removing "brand" column 
clean6=clean6[,c(-2,-3)]             #removing name and base_name
clean6=clean6[,-3]                   #removing pixel_x

library(forcats)
clean6$screen_size=fct_collapse(as.factor(clean6$screen_size), '11.6' = c("10.1","10.8","12","12.2","12.3"))  
clean6$screen_size=fct_collapse(as.factor(clean6$screen_size), '13.3' = c("12.5","13.5"))
clean6$screen_size=fct_collapse(as.factor(clean6$screen_size), '14' = c("13.9"))
clean6$screen_size=fct_collapse(as.factor(clean6$screen_size), '15.6' = c("15","15.4","16"))
clean6$screen_size=fct_collapse(as.factor(clean6$screen_size), '17.3' = c("17"))

lookup = clean6 %>%                     #creating lookup table for mean of screen_size
  group_by(screen_size) %>%
  summarise(mean_screensize = mean(max_price))
clean6 = left_join(clean6, lookup)
 
clean6=clean6[,-2]              #removing "screen_size" column 

clean6$pixels_y=fct_collapse(as.factor(clean6$pixels_y), '768' = c("800","900"))  
clean6$pixels_y=fct_collapse(as.factor(clean6$pixels_y), '1080' = c("1200","1280","1440","1504","1600","1800","1824","1920","2000","2160"))

lookup = clean6 %>%                      #creating lookup table for mean of max_price
  group_by(pixels_y) %>%
  summarise(mean_pixely = mean(max_price))
clean6 = left_join(clean6, lookup)

clean6=clean6[,-2]            #Removing "pixel_y" column

lookup = clean6 %>%                     #creating lookup table for mean of screen_surface
  group_by(screen_surface) %>%
  summarise(mean_screensurface = mean(max_price))
clean6 = left_join(clean6, lookup)
clean6=clean6[,-2]             #Removing screen_surface

lookup = clean6 %>%                     #creating lookup table for mean of screen_size
  group_by(touchscreen) %>%
  summarise(mean_touchscreen = mean(max_price))
clean6 = left_join(clean6, lookup)
clean6=clean6[,-2]             #Removing touchscreen column

clean6$cpu=fct_collapse(as.factor(clean6$cpu), 'AMD' = c("AMD A10","AMD A12","AMD A6","AMD A8","AMD A9","AMD FX","AMD Ryzen 3","AMD Ryzen 5","AMD Ryzen 7")) 
clean6$cpu=fct_collapse(as.factor(clean6$cpu), 'OTHER' = c("MediaTek","Rockchip","Samsung Exynos")) 

lookup = clean6 %>%                     #creating lookup table for mean of "cpu" prices
  group_by(cpu) %>%
  summarise(mean_cpu= mean(max_price))
clean6 = left_join(clean6, lookup)
clean6=clean6[,-2]             #Removing cpu
clean6=clean6[,-2]             #Removing cpu_detail

lookup = clean6 %>%                     #creating lookup table for mean of dkeyboard
  group_by(dkeyboard) %>%
  summarise(mean_dkeyboard= mean(max_price))
clean6 = left_join(clean6, lookup)
clean6=clean6[,-2]  #Removing dkeyboard

lookup = clean6 %>%                     #creating lookup table for mean of discrete_gpu
  group_by(discrete_gpu) %>%
  summarise(mean_discretegpu= mean(max_price))
clean6 = left_join(clean6, lookup)
clean6=clean6[,-2]             #Removing discrete_gpu

clean6$gpu=fct_collapse(as.factor(clean6$gpu), 'AMD Radeon R' = c("AMD Radeon R2","AMD Radeon R4","AMD Radeon R5","AMD Radeon R6","AMD Radeon R7")) 

lookup = clean6 %>%                     #creating lookup table for mean of gpu
  group_by(gpu) %>%
  summarise(mean_gpu= mean(max_price))
clean6 = left_join(clean6, lookup)
clean6=clean6[,-2]             #Removing gpu

lookup = clean6 %>%                     #creating lookup table for mean of gpu
  group_by(os) %>%
  summarise(mean_os= mean(max_price))
clean6 = left_join(clean6, lookup)
clean6=clean6[,-2]             #Removing os

clean6=clean6[,-2]             #Removing os_details
clean6$ram=as.numeric(clean6$ram)
clean6$ssd=as.numeric(clean6$ssd)
clean6$storage=as.numeric(clean6$storage)

#---------------Scaling the data to (0,1) range---------
x = clean6[, -c(6,7)]   #Removing response variables (Min and Max price) from the training subset 
y = clean6$max_price    #Having max_price as response


training_without=subset(clean6,select=-c(6,7))      #Ranging all the variables between 0 and 1
preProcess_range_model <- preProcess(training_without, method='range')
trainData <- predict(preProcess_range_model, newdata = training_without)

featurePlot(x = trainData,                                   #creating scaterplot to look at relations
            y = clean6$max_price, 
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

   lmProfile <- rfe(x=trainData[, 2:15], y=clean6$max_price,
                                 rfeControl = ctrl)

   lmProfile
