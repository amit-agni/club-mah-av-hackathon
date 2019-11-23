if (!require("pacman")) install.packages("pacman")

install.packages("scales")
library(scales)
install.packages("DataExplorer")
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



############        MemberId      #############
# is this same as interactions ?
# which a random forest should be able to automatically identify ?

vars <- c("channel_code","booking_type_code","main_product_code"
          ,"room_type_booked_code","resort_type_code","cluster_code"
          ,"resort_id"
          ,"checkin_date_year","checkin_date_month","checkin_date_dow"
          ,"state_code_residence","persontravellingid"
          ,"member_age_buckets","season_holidayed_code")

for(i in vars) {
    temp <- c("flag",eval(i),"memberid")
    
    DT[flag == "train",
       paste0(i,"_memberid")  := length(reservation_id),
       by = temp]
    
    DT[flag == "test",
       paste0(i,"_memberid")  := length(reservation_id),
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


############## Top important feature changed to factor!!!! ############
############# Length of Stay interactions ##################

#DT$length_of_stay_new <- DT$length_of_stay

group_category(data = DT, feature = "length_of_stay", threshold = 0.001,update = TRUE)

vars <- c("booking_type_code","main_product_code"
          ,"room_type_booked_code","resort_type_code","cluster_code"
          ,"resort_id"
          ,"checkin_date_year","checkin_date_month","checkin_date_dow"
          ,"state_code_residence","persontravellingid"
          ,"member_age_buckets","season_holidayed_code")


for(i in vars) {
    temp <- c("flag",eval(i),"length_of_stay")
    
    DT[flag == "train",
       paste0(i,"_length_of_stay")  := length(reservation_id),
       by = temp]
    
    DT[flag == "test",
       paste0(i,"_length_of_stay")  := length(reservation_id),
       by = temp]
}


############## Second important feature changed to factor!!!! ############
############# No of rooms interactions ##################

group_category(data = DT, feature = "no_of_rooms", threshold = 0.001, update = TRUE)

vars <- c("booking_type_code","main_product_code"
          ,"room_type_booked_code","resort_type_code","cluster_code"
          ,"resort_id"
          ,"checkin_date_year","checkin_date_month","checkin_date_dow"
          ,"state_code_residence","persontravellingid"
          ,"member_age_buckets","season_holidayed_code")

for(i in vars) {
    temp <- c("flag",eval(i),"no_of_rooms")
    
    DT[flag == "train",
       paste0(i,"_no_of_rooms")  := length(reservation_id),
       by = temp]
    
    DT[flag == "test",
       paste0(i,"_no_of_rooms")  := length(reservation_id),
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
vars <- names(which(lapply(DT,is.numeric)==TRUE))
vars <- vars[!vars == "amount_spent_per_room_night_scaled"]
DT[,(vars) := lapply(.SD,scale), .SDcols = vars]




################# Polynomials ##################

# nos <- 5
# temp <- data.frame(poly(DT$ltv,nos))
# colnames(temp) <- paste0("ltv",seq(1,nos,1))
# DT <- cbind(DT,temp)
# 
# temp <- data.frame(poly(DT$asset_cost,nos))
# colnames(temp) <- paste0("asset_cost",seq(1,nos,1))
# DT <- cbind(DT,temp)





### Just to test, add the residuals of the below linear model 

# lm_model <- glm(persontravellingid  ~ 
#        resort_type_code * room_type_booked_code  ,
#     data = DT[, .(resort_type_code,room_type_booked_code,persontravellingid)],
#     famil)
# 
# summary(lm_model)
# 
# lm_model$residuals


#FEATURE SELECTION
# vars <- c("length_of_stay","no_of_rooms","total_pax","roomnights","persontravellingid","tot_bkgs_by_memberid_per_resort"
#   ,"tot_bkgs_by_memberid_per_year","resort_id","state_code_resort","lead_time","main_product_code"
#   ,"resort_region_code","resort_type_code","room_type_booked_code","numberofadults","tot_bkgs_by_memberid_per_room_type",
#   "state_code_residence","checkin_date_year",
#   "flag","amount_spent_per_room_night_scaled")
# DT <- DT[,which(names(DT) %in% vars),with = FALSE]

glimpse(DT)











### Model Data Prep

DT_bkup <- copy(DT)

DT <- DT[flag == "train",]
DT <- sample_n(DT,0.5*nrow(DT))

index <- createDataPartition(y=DT$amount_spent_per_room_night_scaled, p=0.8
                             , list=FALSE) 
my_train <- DT[index,]
my_test <- DT[-index,]


my_train_labels <- my_train$amount_spent_per_room_night_scaled
my_train$amount_spent_per_room_night_scaled <- NULL

my_test_labels <- my_test$amount_spent_per_room_night_scaled
my_test$amount_spent_per_room_night_scaled <- NULL

dummies <- dummyVars(~., data = my_train, fullRank = TRUE)
my_train <- predict(dummies, newdata = my_train)

dummies <- dummyVars(~., data = my_test,fullRank = TRUE)
my_test <- predict(dummies, newdata = my_test)


my_train <- xgb.DMatrix(my_train, label = my_train_labels)
my_test <- xgb.DMatrix(my_test, label = my_test_labels)

watchlist <- list(test = my_test, train = my_train)









### Bayesian Optimisation 

##### PART I : eta and nrounds
opt_fn <- function(nrounds, eta) {
    
    xgb_model <- xgb.train(
        params = list(
            booster = "gbtree"
            ,objective = "reg:linear"
            ,eval_metric = "rmse"
            ,eta = eta)
        ,data = my_train
        ,watchlist = watchlist
        ,nrounds = nrounds
        ,early_stopping_rounds = 10
        ,verbose = TRUE
        ,prediction = TRUE) 
    
    list(
        #Score = xgb_model$best_score
        Score = -xgb_model$evaluation_log[xgb_model$best_iteration]$test_rmse
        ,Pred = xgb_model$pred)
    
}

set.seed(1)
opt_res <- BayesianOptimization(opt_fn
                                ,bounds = list(
                                    nrounds = c(200,400)
                                    ,eta = c(0.01,0.3))
                                #,init_grid_dt = NULL
                                ,init_grid_dt = data.table(nrounds = 200,eta = 0.1)
                                ,init_points = 10
                                ,n_iter = 50
                                ,acq = "ucb"
                                ,kappa = 2.576
                                ,eps = 0.0
                                ,verbose = TRUE)



##### PART II : max_depth, min_child_weight, gamma, subsample, colsample_bytree
opt_fn <- function(max_depth, min_child_weight, gamma, subsample, colsample_bytree) {
    
    xgb_model <- xgb.train(
        params = list(
            booster = "gbtree"
            ,objective = "reg:linear"
            ,eval_metric = "rmse"
            ,eta = 0.06 
                
                ,max_depth = max_depth
            ,min_child_weight = min_child_weight
            ,gamma = gamma
            ,subsample = subsample
            ,colsample_bytree = colsample_bytree  
            
        )
        ,data = my_train
        ,watchlist = watchlist
        
        ,nrounds = 300
            
            ,early_stopping_rounds = 10
        ,verbose = TRUE
        ,prediction = TRUE) 
    
    list(
        #Score = xgb_model$best_score
        Score = -xgb_model$evaluation_log[xgb_model$best_iteration]$test_rmse
        ,Pred = xgb_model$pred)
    
}

set.seed(1)
opt_res <- BayesianOptimization(opt_fn
                                ,bounds = list(
                                    max_depth = c(2L,8L)
                                    ,min_child_weight = c(1L,5L)
                                    ,gamma = c(0L,50L)
                                    ,subsample = c(0.5,1)
                                    ,colsample_bytree = c(0.5,1)                                      
                                )
                                ,init_grid_dt = NULL
                                ,init_points = 10
                                ,n_iter = 100
                                ,acq = "ucb"
                                ,kappa = 2.576
                                ,eps = 0.0
                                ,verbose = TRUE)



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
    ,early_stopping_rounds = 3
    ,verbose = TRUE
    ,prediction = TRUE) 
  
  Score = -xgb_model$evaluation_log[xgb_model$best_iteration]$test_rmse
  
}


# Range of the parameter values to be tested
lower <- c(nrounds = 100, eta = 0.01)
upper <- c(nrounds = 500, eta = 0.3)


# Run the genetic algorithm
results <- ga(type = "real-valued",
              fitness = opt_fn,
              names = names(lower), 
              lower = lower,
              upper =  upper,
              popSize = 10, 
              maxiter = 20)

plot(results)
summary(results)

saveRDS(results,file="results1.Rds")

##################################################################
#######################     ga2       ############################
##################################################################

### PART II

opt_fn <- function(x) {
  
  xgb_model <- xgb.train(
    params = list(
      booster = "gbtree"
      ,objective = "reg:linear"
      ,eval_metric = "rmse"
      ,eta = 0.0681
      
      ,max_depth = round(x[1],0)
      ,gamma = x[2]
      ,subsample = x[3]
      ,colsample_bytree = x[4]   
      
    )
    ,data = my_train
    ,watchlist = watchlist
    ,nrounds = 260
    ,early_stopping_rounds = 5
    ,verbose = 0
    ,prediction = TRUE) 
  
  Score = -xgb_model$evaluation_log[xgb_model$best_iteration]$test_rmse
  
}


# Range of the parameter values to be tested
lower <- c(max_depth = 2L,gamma = 0L,subsample = 0.8,colsample_bytree = 0.8)
           
upper <- c(max_depth = 6L,gamma = 50L,subsample = 1,colsample_bytree = 1)
           

library(pacman)
p_load(GA)
# Run the genetic algorithm
results <- ga(type = "real-valued",
              fitness = opt_fn,
              names = names(lower), 
              lower = lower,
              upper =  upper,
              popSize = 10, 
              maxiter = 20)
# 
# Started 19:10pm = 6 iters
# Started 19:50 = 
# 
#   max_depth    gamma subsample colsample_bytree
# [1,]   5.61751 11.13708  0.956332         0.813348

  
plot(results)
summary(results)

saveRDS(results,file="results2.Rds")









#################################
##### PART III : L1 alpha wt , L2 lambda sq.wt
opt_fn <- function(alpha,lambda) {
    
    xgb_model <- xgb.train(
        params = list(
            booster = "gbtree"
            ,objective = "reg:linear"
            ,eval_metric = "rmse"
            ,eta = 
                
                ,max_depth = 
                ,min_child_weight = 
                ,gamma = 
                ,subsample = 
                ,colsample_bytree =   
                
                ,alpha = alpha
            ,lambda = lambda
            
        )
        ,data = my_train
        ,watchlist = watchlist
        
        ,nrounds = 
            
            ,early_stopping_rounds = 10
        ,verbose = TRUE
        ,prediction = TRUE) 
    
    list(
        #Score = xgb_model$best_score
        Score = -xgb_model$evaluation_log[xgb_model$best_iteration]$test_rmse
        ,Pred = xgb_model$pred)
    
}

set.seed(1)
opt_res <- BayesianOptimization(opt_fn
                                ,bounds = list(
                                    alpha = c(0,1)
                                    ,lambda = c(0,1)
                                )
                                ,init_grid_dt = NULL
                                ,init_points = 10
                                ,n_iter = 100
                                ,acq = "ucb"
                                ,kappa = 2.576
                                ,eps = 0.0
                                ,verbose = TRUE)





### Re-evaluate the best itin on my_test

xgb_model <- xgb.train(
    params = list(
        booster = "gbtree"
        ,objective = "reg:linear"
        ,eval_metric = "rmse"
        ,eta = 
            
            ,max_depth = 
            ,min_child_weight = 
            ,gamma = 
            ,subsample = 
            ,colsample_bytree =   
            
            ,alpha = 
            ,lambda = 
    )
    ,data = my_train
    ,watchlist = watchlist
    ,nrounds = 
        ,early_stopping_rounds = 10
    ,verbose = TRUE
    ,prediction = TRUE) 



# Learning Curve
ggplot(melt(xgb_model$evaluation_log,id.vars = "iter")) +
    geom_line(aes(x=iter, y=value, color=variable))

importance <- xgb.importance(model = xgb_model)
importance
xgb.plot.importance(importance)

importance[1:30]$Feature

# Other XGB plots [TO BE REVIEWED]
#xgb.plot.multi.trees(model = xgb_model)
#xgb.plot.deepness(model=xgb_model)
#xgb.plot.shap()
xgb_model$best_iteration
#xgb.plot.tree(model=xgb_model,trees = 1)


# Feature contributions [HOW CAN THIS BE USED]
#pred <- predict(xgb_model,newdata = my_test ,predcontrib = TRUE)







### Final train on full train



my_train <- DT_bkup[flag == "train",]

my_train_labels <- my_train$amount_spent_per_room_night_scaled
my_train$amount_spent_per_room_night_scaled <- NULL

dummies <- dummyVars(~., data = my_train, fullRank = TRUE)
my_train <- predict(dummies, newdata = my_train)

my_train <- xgb.DMatrix(my_train, label = my_train_labels)

xgb_model <- xgboost(
    params = list(
        booster = "gbtree"
        ,objective = "reg:linear"
        ,eval_metric = "rmse"
        
        ,eta = 
            
            ,max_depth = 
            ,min_child_weight = 
            ,gamma = 
            ,subsample = 
            ,colsample_bytree =   
            
            ,alpha = 
            ,lambda = 
            
    )
    ,data = my_train
    ,nrounds = 
        ,early_stopping_rounds = 10
    ,verbose = TRUE
    ,prediction = TRUE) 





### Submission


my_test <- DT_bkup[flag == "test",]
my_test$amount_spent_per_room_night_scaled <- NULL

dummies <- dummyVars(~., data = my_test,fullRank = TRUE)
my_test <- predict(dummies, newdata = my_test)

my_test <- xgb.DMatrix(my_test)

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

