if (!require("pacman")) install.packages("pacman")

#install.packages("scales")
library(scales)
#install.packages("DataExplorer")
library(DataExplorer)

p_load(caret,data.table,dplyr,lubridate
       ,here,DescTools,DataExplorer,tidyverse
       ,googledrive,e1071,ggcorrplot,forcats
       ,xgboost,DiagrammeR
       ,rBayesianOptimization)

options(scipen=999) #remove scientific notation in printing 


####### parallel on windows #######
# p_load(doParallel)
# cl <- makeCluster(detectCores(), type='PSOCK')
# #registerDoParallel(1)
# registerDoParallel(cl)

# turn parallel processing off and run sequentially again:
# registerDoSEQ()

#####################################
parallel::detectCores()

p_load(doMC)

registerDoMC(cores = 16)

dribble <- drive_find(pattern="train.csv")
drive_download(as_dribble(dribble),overwrite = TRUE)

dribble <- drive_find(pattern="test.csv")
drive_download(as_dribble(dribble),overwrite = TRUE)

DT_train <- fread(here::here("train.csv"))
DT_test <- fread(here::here("test.csv"))
#####################################


# DT_train <- fread(here::here("100_data_raw-input","train.csv"))
# DT_test <- fread(here::here("100_data_raw-input","test.csv"))


DT_train$flag <- "train"
DT_test$flag <- "test"
DT_test$amount_spent_per_room_night_scaled <- 0

DT <- rbind(DT_train,DT_test)
#rm(DT_train)
#rm(DT_test)

#Date fields
vars <- c("booking_date","checkin_date","checkout_date")
DT[,(vars) := lapply(.SD,dmy), .SDcols = vars]

#Convert to factors
vars <- c("channel_code","main_product_code","persontravellingid",
          "resort_region_code","resort_type_code","room_type_booked_code",
          "state_code_resort","booking_type_code")
DT[,(vars) := lapply(.SD,as.factor), .SDcols = vars]




######### FEAT ENGGG




#Fixing outliers
DT[, `:=`(booking_date = if_else(booking_date > checkin_date ,checkin_date , booking_date),
          booking_date_outlier = as.factor(if_else(booking_date > checkin_date ,1 , 0)))]

DT[, `:=`(roomnights = ifelse(roomnights < 0, 1,roomnights),
          roomnights_outlier = as.factor(ifelse(roomnights < 0, 1,0)))]

DT[, total_pax_outlier1 := as.factor(case_when(
  total_pax == numberofadults + numberofchildren ~ 0,
  total_pax > numberofadults + numberofchildren ~ 1,
  total_pax < numberofadults + numberofchildren ~ -1,
  TRUE ~ 777))]

DT[, total_pax_outlier2 := as.factor(case_when(
  total_pax == numberofadults  ~ 0,
  total_pax > numberofadults  ~ 1,
  total_pax < numberofadults  ~ -1,
  TRUE ~ 777))]


# prop.table(table(DT$booking_date_outlier))
# prop.table(table(DT$roomnights_outlier))
# prop.table(table(DT$total_pax_outlier))


# Fixing missing values
DT[which(complete.cases(DT)== FALSE)]
vars <- c("season_holidayed_code","state_code_residence")

prop.table(table(DT$state_code_residence))
DT[, `:=`(state_code_residence = ifelse(is.na(state_code_residence),8,state_code_residence),
          state_code_residence_outlier = ifelse(is.na(state_code_residence),1,0))]

prop.table(table(DT$season_holidayed_code))
DT[, `:=`(season_holidayed_code = ifelse(is.na(season_holidayed_code),2,season_holidayed_code),
          season_holidayed_code_outlier = ifelse(is.na(season_holidayed_code),1,0))]

DT[which(complete.cases(DT)== FALSE)]




#Date features

vars <- c("booking_date","checkin_date")
vars_temp <- paste0(vars,"_year")
DT[,(vars_temp) := lapply(.SD, function(x) as.factor(year(x)))
   , .SDcols = vars]

vars_temp <- paste0(vars,"_month")
DT[,(vars_temp) := lapply(.SD, function(x) as.factor(month(x)))
   , .SDcols = vars]

vars_temp <- paste0(vars,"_dow")
DT[,(vars_temp) := lapply(.SD, function(x) as.factor(
  if_else(wday(x,week_start = 1) %in% c(6,7), 'WE','WD')))
  , .SDcols = vars]

DT[,length_of_stay := as.double(checkout_date - checkin_date)]
DT[,lead_time := as.double(checkin_date - booking_date)]

DT[,no_of_rooms := ifelse(roomnights == 0, length_of_stay 
                          , round(roomnights/length_of_stay,0) )]

################################################
################# GROUP BY  ####################
################################################

############## Group by Number of bookings per year ############

vars <- c("main_product_code"
          ,"room_type_booked_code","resort_type_code","cluster_code"
          ,"resort_id"
          ,"checkin_date_year","checkin_date_month","checkin_date_dow"
          ,"state_code_residence","persontravellingid"
          ,"member_age_buckets","season_holidayed_code")

for(i in vars) {
  temp <- c("flag",eval(i))
  
  DT[flag == "train",
     paste0(i,"_groupby")  := length(reservation_id),
     by = temp]
  
  DT[flag == "test",
     paste0(i,"_groupby")  := length(reservation_id),
     by = temp]
}




########### Total Pax outlier exploration ########
DT[,tot_pax_var1 := total_pax - (numberofadults + numberofchildren) ]

DT[,tot_pax_var2 := total_pax - numberofadults  ]

vars <- c("channel_code","booking_type_code","main_product_code"
          ,"room_type_booked_code","resort_type_code","cluster_code"
          ,"resort_id","reservationstatusid_code"
          ,"persontravellingid"
          ,"checkin_date_year","checkin_date_month","checkin_date_dow"
          ,"member_age_buckets","season_holidayed_code")


for(i in vars) {
  temp <- c("flag",eval(i),"tot_pax_var1")
  
  DT[flag == "train",
     paste0(i,"_tot_pax_var1")  := length(reservation_id),
     by = temp]
  
  DT[flag == "test",
     paste0(i,"_tot_pax_var1")  := length(reservation_id),
     by = temp]
}


for(i in vars) {
  temp <- c("flag",eval(i),"tot_pax_var2")
  
  DT[flag == "train",
     paste0(i,"_tot_pax_var2")  := length(reservation_id),
     by = temp]
  
  DT[flag == "test",
     paste0(i,"_tot_pax_var2")  := length(reservation_id),
     by = temp]
}







############    WEEKEND stay / month of stay could have mean higher spend ?      #############
# is this same as interactions ?
# which a random forest should be able to automatically identify ?

vars <- c("booking_type_code","main_product_code"
          ,"room_type_booked_code","resort_type_code","cluster_code"
          ,"resort_id"
          ,"state_code_residence","persontravellingid"
          ,"member_age_buckets","season_holidayed_code")

for(i in vars) {
  temp <- c("flag",eval(i),"checkin_date_dow")
  
  DT[flag == "train",
     paste0(i,"_checkin_date_dow")  := length(reservation_id),
     by = temp]
  
  DT[flag == "test",
     paste0(i,"_checkin_date_dow")  := length(reservation_id),
     by = temp]
}


for(i in vars) {
  temp <- c("flag",eval(i),"checkin_date_month")
  
  DT[flag == "train",
     paste0(i,"_checkin_date_month")  := length(reservation_id),
     by = temp]
  
  DT[flag == "test",
     paste0(i,"_checkin_date_month")  := length(reservation_id),
     by = temp]
}





# Binning
group_category(data = DT, feature = "numberofadults", threshold = 0.001,update = TRUE)
group_category(data = DT, feature = "numberofchildren", threshold = 0.001,update = TRUE)
group_category(data = DT, feature = "total_pax", threshold = 0.001,update = TRUE)




#housekeeping
DT$reservation_id <- NULL
DT$memberid <- NULL

vars <- names(which(lapply(DT,is.character)==TRUE))
DT[,(vars) := lapply(.SD,as.factor), .SDcols = vars]

vars <- c("booking_date","checkin_date","checkout_date")
DT[,(vars) := NULL]

vars <- c("season_holidayed_code","state_code_residence")
DT[,(vars) := lapply(.SD,as.factor), .SDcols = vars]


vars <- grep("outlier",names(DT),value = TRUE)
DT[,(vars) := lapply(.SD,as.factor), .SDcols = vars]



############# Scale the numerical values #################
# vars <- names(which(lapply(DT,is.numeric)==TRUE))
# vars <- vars[!vars == "amount_spent_per_room_night_scaled"]
# DT[,(vars) := lapply(.SD,scale), .SDcols = vars]



################## ################## ################## ################## 
###################  vtreat lincomb prcomp  START  ############
################## ################## ################## ################## 

#DT_bkup <- DT
#DT <- DT_bkup

p_load(vtreat)
p_load(magrittr)

vars <- names(DT)
vars <- vars[!vars %in% c("flag"
                          ,"amount_spent_per_room_night_scaled")]


my_train_labels_before_everything <- DT[flag =="train"]$amount_spent_per_room_night_scaled

# Create the treatment plan for train data
treatplan <- designTreatmentsZ(DT[flag == "train"]
                               , vars
                               , verbose = TRUE)
summary(treatplan)

# Get the "clean" and "lev" variables from the scoreFrame
temp <- data.table(treatplan$scoreFrame)
newvars <- temp[code %in% c("clean","lev")]$varName

# Prepare the training data
train.treat <- prepare(treatplan, DT[flag == "train",]
                       ,  varRestriction = newvars)

# Prepare the test data
test.treat <- prepare(treatplan, DT[flag =="test",]
                      ,  varRestriction = newvars)




temp_train.pr <- data.table(train.treat)
temp_train.pr$amount_spent_per_room_night_scaled <- my_train_labels_before_everything

index <- createDataPartition(y=temp_train.pr$amount_spent_per_room_night_scaled
                             , p=0.7
                             , list=FALSE)
my_train <- temp_train.pr[index,]
my_test <- temp_train.pr[-index,]

my_train_labels <- my_train$amount_spent_per_room_night_scaled
my_train$amount_spent_per_room_night_scaled <- NULL

my_test_labels <- my_test$amount_spent_per_room_night_scaled
my_test$amount_spent_per_room_night_scaled <- NULL

my_train <- xgb.DMatrix(as.matrix(my_train), label = my_train_labels)
my_test <- xgb.DMatrix(as.matrix(my_test), label = my_test_labels)

watchlist <- list(train = my_train,test = my_test)





################## vtreat lincomb prcomp END ###############
################## ################## ################## ################## 






#################################
#################################
p_load(GA)
opt_fn <- function(x) {
  xgb_model <- xgb.train(
    params = list(
      booster = "gbtree"
      ,objective = "reg:linear"
      ,eval_metric = "rmse"
      ,eta = x[2])
    ,data = my_train
    ,watchlist = watchlist
    ,nrounds = x[1]
    ,early_stopping_rounds = 5
    ,verbose = TRUE)
  
  Score = -xgb_model$evaluation_log[xgb_model$best_iteration]$test_rmse
  
}

# Range of the parameter values to be tested
lower <- c(nrounds = 100, eta = 0.01)
upper <- c(nrounds = 400, eta = 0.3)

#p_load(GA)
# Run the genetic algorithm
results <- ga(type = "real-valued",
              fitness = opt_fn,
              names = names(lower), 
              lower = lower,
              upper =  upper,
              popSize = 10, 
              maxiter = 10)

plot(results)
summary(results)

saveRDS(results,file="results1.Rds")


##################################################################
#######################     ga2       ############################
##################################################################


opt_fn <- function(x) {
  
  xgb_model <- xgb.train(
    params = list(
      booster = "gbtree"
      ,objective = "reg:linear"
      ,eval_metric = "rmse"
      ,eta = 0.26
      
      ,max_depth = round(x[1],0)
      ,gamma = x[2]
      ,subsample = x[3]
      ,colsample_bytree = x[4]   
      
    )
    ,data = my_train
    ,watchlist = watchlist
    ,nrounds = 150
    ,early_stopping_rounds = 5
    ,verbose = TRUE)
  
  Score = -xgb_model$evaluation_log[xgb_model$best_iteration]$test_rmse
  
}


# Range of the parameter values to be tested
lower <- c(max_depth = 2L,gamma = 0L,subsample = 0.8,colsample_bytree = 0.8)

upper <- c(max_depth = 6L,gamma = 50L,subsample = 1,colsample_bytree = 1)


#library(pacman)
#p_load(GA)
# Run the genetic algorithm
results <- ga(type = "real-valued",
              fitness = opt_fn,
              names = names(lower), 
              lower = lower,
              upper =  upper,
              popSize = 10, 
              maxiter = 15)
# 
# Started 19:10pm = 6 iters
# Started 19:50 = 
# 
#   max_depth    gamma subsample colsample_bytree
# [1,]   5.61751 11.13708  0.956332         0.813348


plot(results)
summary(results)




saveRDS(results,file="results3.Rds")

#max_depth    gamma subsample colsample_bytree
#[1,]  5.122181 16.14083 0.9599086        0.8752691







##################################################################
#######################     GA3       ############################
##################################################################


opt_fn <- function(x) {
  
  xgb_model <- xgb.train(
    params = list(
      booster = "gbtree"
      ,objective = "reg:linear"
      ,eval_metric = "rmse"
      ,eta = x[5]
      
      ,max_depth = round(x[1],0)
      ,gamma = x[2]
      ,subsample = x[3]
      ,colsample_bytree = x[4]   
      
    )
    ,data = my_train
    ,watchlist = watchlist
    ,nrounds = x[6]
    ,early_stopping_rounds = 5
    ,verbose = TRUE)
  
  Score = -xgb_model$evaluation_log[xgb_model$best_iteration]$test_rmse
  
}


lower <- c(max_depth = 2L,gamma = 0L,subsample = 0.8,colsample_bytree = 0.8, eta = 0.01, nrounds = 100)
upper <- c(max_depth = 6L,gamma = 50L,subsample = 1,colsample_bytree = 1, eta = 0.3, nrounds = 300)

results <- ga(type = "real-valued",
              fitness = opt_fn,
              names = names(lower), 
              lower = lower,
              upper =  upper,
              popSize = 10, 
              maxiter = 15)

plot(results)
summary(results)
saveRDS(results,file="results4.Rds")




##################### end GA3 ####################



### Re-evaluate the best itin on my_test

xgb_model <- xgb.train(
  params = list(
    booster = "gbtree"
    ,objective = "reg:linear"
    ,eval_metric = "rmse"
    ,eta = 0.26
      
      ,max_depth = 5
       ,gamma = 16.14
     ,subsample = 0.96
     ,colsample_bytree =   0.87

  )
  ,data = my_train
  ,watchlist = watchlist
  ,nrounds = 300
    ,early_stopping_rounds = 10
  ,verbose = TRUE
  ,prediction = TRUE) 



### Final train on full train

my_train <- xgb.DMatrix(as.matrix(train.treat), label = my_train_labels_before_everything)

xgb_model <- xgb.cv(
  params = list(
    booster = "gbtree"
    ,objective = "reg:linear"
    ,eval_metric = "rmse"
    
    ,eta = 0.17
    
    ,max_depth = 4
    ,gamma = 7
    ,subsample = 0.9
    ,colsample_bytree =   0.9
      
  )
  ,data = my_train
  ,nrounds = 400
    ,early_stopping_rounds = 10
  ,verbose = TRUE
  ,prediction = TRUE
  
  , nfold = 5
  , showsd = F
  , stratified = T
  
  ) 



### Submission
my_test <- xgb.DMatrix(as.matrix(test.treat))

pred <- predict(xgb_model,newdata = my_test)
summary(pred)

#get_unique_ids <- fread(here::here("100_data_raw-input","test.csv"))

get_unique_ids <- fread(here::here("test.csv"))

submit_kaggle <- as.data.frame(cbind(get_unique_ids$reservation_id,pred))
colnames(submit_kaggle) <- c("reservation_id"
                             ,"amount_spent_per_room_night_scaled")

write.csv(submit_kaggle
          ,file = here(paste0("Submit_XGB_",strftime(Sys.time(), format="%Y%m%d_%H%M%S"),".csv"))
          ,row.names = FALSE)
here()





#####################################
# Principal component
prin_comp <- prcomp(train.treat)
screeplot(prin_comp, npcs = 50, type = "lines")

train.pr <- prin_comp$x[,1:80]
test.pr <- predict(prin_comp, newdata = test.treat)
test.pr <- test.pr[,1:80]

temp_train.pr <- data.table(train.pr)
temp_train.pr$amount_spent_per_room_night_scaled <- my_train_labels_before_everything

index <- createDataPartition(y=temp_train.pr$amount_spent_per_room_night_scaled
                             , p=0.8
                             , list=FALSE) 
my_train <- temp_train.pr[index,]
my_test <- temp_train.pr[-index,]

my_train_labels <- my_train$amount_spent_per_room_night_scaled
my_train$amount_spent_per_room_night_scaled <- NULL

my_test_labels <- my_test$amount_spent_per_room_night_scaled
my_test$amount_spent_per_room_night_scaled <- NULL

my_train <- xgb.DMatrix(as.matrix(my_train), label = my_train_labels)
my_test <- xgb.DMatrix(as.matrix(my_test), label = my_test_labels)

watchlist <- list(test = my_test, train = my_train)
