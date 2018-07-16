#predict the total count of bikes rented during each hour on the 20th day

#set wd
setwd("/Users/User/Desktop/Bike Sharing Demand/")

#load data
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

#transforming of dataset's data
train$datetime <- as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")

#EDA
library(ggplot2)

ggplot(train, aes(x=temp, y=count)) + geom_point(aes(colour=temp), alpha=0.2) #shows that rental increases as temp increases

ggplot(train,aes(datetime,count)) + 
  geom_point(aes(color=temp),alpha=0.2) +  #shows seasonality of data
  scale_color_gradient(high='red',low='blue') + 
  theme_bw()

train$season <- as.factor(train$season)

ggplot(train, aes(season, count)) + geom_boxplot(aes(colour=season)) + theme_bw() #more rentals during winter vs spring

train$hour <- sapply(train$datetime, function(x) format(x,"%H"))
train$year <- sapply(train$datetime, function(x) format(x, "%Y"))

workingdayplot <- ggplot(subset(train, workingday == 1), aes(hour, count)) + 
  geom_point(aes(colour=temp), alpha=0.5, position=position_jitter(w=1, h=5)) +
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red')) +
  ggtitle("Working Day (Count vs Hour)")#general trend - Working Day

weekendplot <- ggplot(subset(train, workingday == 0), aes(hour, count)) + 
  geom_point(aes(colour=temp), alpha=0.5, position=position_jitter(w=1, h=5)) +
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red')) +
  ggtitle("Weekend (Count vs Hour)")#general trend - Weekend Day

grid.arrange(workingdayplot, weekendplot, nrow=2) #comparing between working day vs weekend

spring <- ggplot(subset(train, season == "1"), aes(hour, count)) + 
  geom_point(aes(colour=temp), alpha=0.2, position=position_jitter(w=1, h=5)) + 
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red'))+
  ggtitle("Spring (Hour Vs Count)")

summer <- ggplot(subset(train, season == "2"), aes(hour, count)) + 
  geom_point(aes(colour=temp), alpha=0.2, position=position_jitter(w=1, h=5)) + 
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red'))+
  ggtitle("Summer (Hour Vs Count)")

autumn <- ggplot(subset(train, season == "3"), aes(hour, count)) + 
  geom_point(aes(colour=temp), alpha=0.2, position=position_jitter(w=1, h=5)) + 
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red'))+
  ggtitle("Autumn (Hour Vs Count)")

winter <- ggplot(subset(train, season == "4"), aes(hour, count)) + 
  geom_point(aes(colour=temp), alpha=0.2, position=position_jitter(w=1, h=5)) + 
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red'))+
  ggtitle("Winter (Hour Vs Count)")

library(gridExtra)

grid.arrange(spring, summer, autumn, winter, ncol=2) #show rental per hour during 4 seasons

#Data transformation & Model preparation

train$hour = sapply(train$hour, as.numeric)
train$year = sapply(train$year, as.numeric)

library(naniar)
vis_miss(train) #checking for missing values

#----------Identifying if the features are correlated to each other-------#
cors <- cor(train[ , sapply(train, is.numeric)]) #shows a correlation matrix
high_cor <- which(abs(cors) > 0.6 & (abs(cors) < 1)) #identify the positions of high correlation features, counting from col down!
rows <- rownames(cors)[((high_cor-1) %/% 12)+1] # %/% = integer division i.e. 5 %/% 2 = 2/ 12 BECAUSE it has 12 col & rows!
cols <- colnames(cors)[ifelse(high_cor %% 12 == 0, 12, high_cor %% 12)] #modulus (x mod y) 5%%2 = 1 
vals <- cors[high_cor]

cor_data = data.frame(cols=cols, rows=rows, correlation=vals)
cor_data #as atemp and temp, count - casual and registered are high predictors, they will be removed

library(corrplot) 
corrplot(cors) #visualize correlation

library(dplyr)
train <- select(train, -c(atemp, registered, casual, datetime)) #datetime is removed because year and hour already inside

#-----------MODEL SELECTION-------------#
#--------Multiple Linear Regression-----#

model <- lm(count ~., data = train)

#using stepwise algorithm to find the best model based on lowest AIC

model <- step(model)

#----MODEL VALIDATION WITH TESTING DATA-----#
test$datetime <- as.POSIXct(test$datetime, format="%Y-%m-%d %H:%M:%S")
test$hour <- sapply(test$datetime, function(x) format(x,"%H"))
test$year <- sapply(test$datetime, function(x) format(x, "%Y"))
test$season <- as.factor(test$season)

for (i in 1:ncol(test)){
  if (typeof(test[,i]) == "character") {
    test[i] <- as.numeric(test[,i])
  }
}

value1 <- predict(model, newdata=test)
results <- data.frame(datetime = test$datetime, count=value1) #got the predicted values; however need to refer to the log one for accuracy!

# Write the results to a csv file
write.csv(results, file = 'BikeSharingDemand_MLR.csv', row.names = FALSE, quote=FALSE)

#-----------MODEL SELECTION-------------#
#--------SV Regression------#
library(e1071)
# Fitting SVR to the dataset
modelsvr <- svm(formula = count ~ .,
                data = train,
                type = 'eps-regression',
                kernel = 'radial')

# Predicting a new result
values2 <- predict(modelsvr, test)

# Save the results
results <- data.frame(datetime = test$datetime, count = values2)

# Write the results to a csv file
write.csv(results, file = 'BikeSharingDemand_SVR.csv', row.names = FALSE, quote=FALSE)

#-----------MODEL SELECTION-------------#
#--------Random Forest Regression------#
library(randomForest)
# Fitting Random Forest Regression to the dataset
set.seed(743)
modelRF <- randomForest(x = train[,-which(names(train)=="count")],
                         y = train$count)

# Predicting a new result with Random Forest Regression
values3 <- predict(modelRF, test)

# Save the results
results <- data.frame(datetime = test$datetime, count = values3)

# Write the results to a csv file
write.csv(results, file = 'BikeSharingDemand_RandomForest.csv', row.names = FALSE, quote=FALSE)

#######################################
#------Predictive Model w XGBOOST-----#
#######################################
library(caret)
library(Metrics)
library(xgboost)

# Create custom summary function in proper format for caret
custom_summary <- function(data, lev = NULL, model = NULL){
  out = rmsle(data[, "obs"], data[, "pred"])
  names(out) = c("rmsle")
  out
}

# Create control object
control <- trainControl(method = "cv",  # Use cross validation
                        number = 5,     # 5-folds
                        summaryFunction = custom_summary                      
)


# Create grid of tuning parameters
grid <- expand.grid(nrounds=c(100, 200, 400, 800), # Test 4 values for boosting rounds
                    max_depth= c(4, 6),           # Test 3 values for tree depth
                    eta=c(0.1, 0.05, 0.025),      # Test 3 values for learning rate: 0.1, 0.05, 0.025
                    gamma= c(0.1),                #https://xgboost.readthedocs.io/en/latest/parameter.html for explanation
                    colsample_bytree = c(1), 
                    min_child_weight = c(1),
                    subsample = c(1))

#training and development of model

xgb_tree_model <- train(count~.,      # Predict SalePrice using all features
                        data=train,
                        method="xgbTree",
                        trControl=control, #for cross validation w control
                        tuneGrid=grid, 
                        metric="rmsle",     # Use custom performance metric
                        maximize = FALSE)   # Minimize the metric

#Analysis of results
xgb_tree_model$bestTune #tells us the best model, is a tree with depth 4, trained 400 rounds w learning rate 0.1 (eta)
xgb_tree_model$results #find the RMSLE from the above model here: RMSLE = 0.1327114; the lower the better!


varImp(xgb_tree_model) #identify which predictors are most impt to the model

#testing of dataset

test_predictions <- predict(xgb_tree_model, newdata=test)

submission <- read.csv("sampleSubmission.csv")
submission$count <- test_predictions


for (i in 1:nrow(submission)){ #check on how do i get 
  if (submission[i,2] < 0){
  submission[i,2] <- 1
  } 
}

write.csv(submission, "bikerentalxgboosted.csv", row.names=FALSE)
