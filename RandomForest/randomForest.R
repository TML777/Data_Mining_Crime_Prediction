library(randomForest)

### for data partition
library(caret)

# library(caTools)

### for rvalue
library(plyr)

library(Metrics)



### setting working directory
FOLDER <- "/Users/tiko/R_Programs/Random_Forest"
setwd(FOLDER)

### set seed
set.seed(2024)

### reading data
murderData <- read.csv("CleanedData.csv")


### deleting unwanted collums 
murderData$X <- NULL
murderData$NameCity <- NULL



# Number of folds for cross-validation
num_folds <- 5  

# Cross-validation scheme
ctrl <- trainControl(method = "cv", number = num_folds)


#### Takes Very long to run!! Can just run the readRDS code below
### Train Random Forest model using k-fold cross-validation
rf_model <- train(
  x = murderData[, -which(names(murderData) == "Murder")],  # Input
  y = murderData$Murder,  # Target 
  method = "rf", 
  trControl = ctrl, # K-fold cross validation
  tuneGrid = expand.grid(mtry = 1:10),  # Grid of mtry values to try 1-10
  ntree = 500 # number of trees
  )



# Print random forest model
rf_model

### saving and reading random forest model, so we dont have to run train again
saveRDS(rf_model, "rf_model.rds")
rf_model <- readRDS("rf_model.rds") 


### to display mtry grid
rf_model$results


### printing Best No. of variables tried at each split 
paste0("Best No. of variables tried at each split: ", rf_model$bestTune$mtry)


### plot desplaying NUmber of Trees vs MSE
df <- data.frame(Tree = 1:length(rf_model$finalModel$mse), MSE = rf_model$finalModel$mse)
ggplot(data = df, aes(x = Tree, y = MSE)) + ### Number of trees vs MSE
  geom_line() +  # Plot as a line graph
  geom_point() +  # Add points for each data point
  labs(x = "Tree", y = "Mse", title = "Number of Trees vs MSE")  # Labels and title





### Train Random Forest model using k-fold cross-validation
### final forest already tuned
finalForest <- train(
  x = murderData[, -which(names(murderData) == "Murder")],  # Input
  y = murderData$Murder,  # Target 
  method = "rf", # Random Forest method
  trControl = ctrl, # K-fold cross validation
  tuneGrid = expand.grid(mtry = 5),  # best mtry model
  ntree = 500 # number of trees = 500
)


### saving and reading random forest model to comp, so we dont have to run train again
saveRDS(finalForest, "finalForest.rds")
finalForest <- readRDS("finalForest.rds") 


# printing most important attribute
attributeImportance <- varImp(finalForest)
print(attributeImportance)



### getting predicted data
predictions <- predict(finalForest, newdata = murderData[, -which(names(murderData) == "Murder")])


#length(murderPredictions)


### splitting predicted data into ranges
murderPredictionsRanged<-droplevels(cut(predictions,c(-1,1,10,30,80)))
murderPredictionsRanged <- revalue(murderPredictionsRanged, c("(-1,1]"="0 or 1", "(1,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))


### splitting actual data into ranges
testingMurderRanged<-droplevels(cut(murderData$Murder,c(-1,1,10,30,80)))
testingMurderRanged <- revalue(testingMurderRanged, c("(-1,1]"="0 or 1", "(1,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))


### confusion matrix
conf_matrix <- confusionMatrix(murderPredictionsRanged, testingMurderRanged)
print(conf_matrix)

### confusion matrix plot
conf_matrix_df <- as.data.frame(conf_matrix$table)
names(conf_matrix_df) <- c('Predicted', 'Actual', 'Count')
ggplot(data = conf_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count)) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Predicted")




### prediction matrix 
# table(murderPredictionsRanged, testingMurderRanged)



# Calculate accuracy metrics
mse <- mean((predictions - murderData$Murder)^2)
rmse <- sqrt(mse)
mae <- mean(abs(predictions - murderData$Murder))
r_squared <- cor(predictions, murderData$Murder)^2
# Calculate Mean Absolute Percentage Error (MAPE)
epsilon <- 1
mape <- mean(abs((predictions - murderData$Murder) / (murderData$Murder + epsilon))) * 100

accuracy <- 100 - mape


# Print accuracy metrics
print(paste0("MSE: ", mse))
print(paste0("RMSE: ", rmse))
print(paste0("MAE: ", mae))
print(paste0("R-squared: ", r_squared))
print(paste0("MAPE: ", mape))
print(paste0("Accuracy: ", accuracy))




# Create a data frame to hold the actual and predicted values
data <- data.frame(Actual = murderData$Murder, Predicted = predictions)

# Plot
ggplot(data, aes(x = Actual, y = Predicted)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear model line
  labs(title = "Plot of Actual vs. Predicted Values",
       x = "Actual Values",
       y = "Predicted Values") 
















####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
##################### Everything bellow here is Expermention #######################################################################
##################### just for fun can ignore ######################################################################################
################This one is not using k-fold corss validation but 20/80 split ######################################################



### setting working directory
FOLDER <- "/Users/tiko/R_Programs"
setwd(FOLDER)

### set seed
set.seed(2024)

### reading data
murderData <- read.csv("CleanedData.csv")


### deleting unwanted collums 
murderData$X <- NULL
murderData$NameCity <- NULL


### getting training set and testing set
trainIndex <- createDataPartition(y = murderData$Murder,
                                  p=0.8, list = FALSE)
training <- murderData[trainIndex,] # training set 80% of data
testing <- murderData[-trainIndex,] # testing set 20% of data







### random forest set at 1000 trees and defult variables split
murderForest <- randomForest(Murder ~., data = training, ntree=1000)
murderForest


### printing Number of Trees vs MSE
### to see how number of trees effect MSE
df <- data.frame(Tree = 1:length(murderForest$mse), MSE = murderForest$mse)
ggplot(data = df, aes(x = Tree, y = MSE)) +
  geom_line() +  # Plot as a line graph
  geom_point() +  # Add points for each data point
  labs(x = "Tree", y = "Mse", title = "Number of Trees vs MSE")  # Labels and title






### running 10 Random forests changing the NO. variables split each time
mse.values <- vector(length=10)
for(i in 1:10) {
  temp.rf <- randomForest(Murder ~ ., data=training, mtry=i, ntree=1000)
  mse.values[i] <- mean(temp.rf$mse)
}
mse.values


### getting the lowest mse values and the number of splits which cause it
min(mse.values)
lowestSplit <-which(mse.values == min(mse.values))
lowestSplit


### final forest with 1000 trees and best split value
finalForest <- randomForest(Murder ~ ., data=training, ntree=1000, mtry = lowestSplit)
finalForest



### running algorithm on testing data
murderPredictions <- predict(finalForest, newdata = testing)



### splitting predicted data into ranges
murderPredictionsRanged<-droplevels(cut(murderPredictions,c(-1,1,10,30,80)))
murderPredictionsRanged <- revalue(murderPredictionsRanged, c("(-1,1]"="0 or 1", "(1,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))


### splitting actual data into ranges
testingMurderRanged<-droplevels(cut(testing$Murder,c(-1,1,10,30,80)))
testingMurderRanged <- revalue(testingMurderRanged, c("(-1,1]"="0 or 1", "(1,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))


### confusion matrix
conf_matrix <- confusionMatrix(murderPredictionsRanged, testingMurderRanged)
print(conf_matrix)


### prediction matrix 
table(murderPredictionsRanged, testingMurderRanged)



### Evaluation ### 

mse <- mean((murderPredictions - testing$Murder)^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Mean Absolute Error (MAE)
mae <- mean(abs(murderPredictions - testing$Murder))

# R-squared (R2)
mean_actual <- mean(testing$Murder)
sst <- sum((testing$Murder - mean_actual)^2)  # Total sum of squares
ssr <- sum((murderPredictions - testing$Murder)^2)  # Residual sum of squares
r_squared <- 1 - (ssr / sst)

# Print the evaluation metrics
print(paste0("MSE:", mse))
print(paste0("RMSE:", rmse))
print(paste0("MAE:", mae))
print(paste0("R-squared:", r_squared))






####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
################### This one is is dividing the output into intervals before training, #############################################
################### basically turning into classification ##########################################################################


FOLDER <- "/Users/tiko/R_Programs"
setwd(FOLDER)

set.seed(2024)


### download merged data
murderData <- read.csv("CleanedData.csv")


murderData$X <- NULL
murderData$NameCity <- NULL


# murderData$Murder = factor(murderData$Murder) 

murderData$Murder<-droplevels(cut(murderData$Murder,c(-1,1,10,30,80)))

murderData$Murder <- revalue(murderData$Murder, c("(-1,1]"="0 or 1", "(1,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))

# table(murderData$Murder)



# summary(murderData)



trainIndex <- createDataPartition(y = murderData$Murder,
                               p=0.8, list = FALSE)
training <- murderData[trainIndex,]
testing <- murderData[-trainIndex,]

# ?randomForest
murderForest <-  randomForest(Murder ~., data = training, ntree=1000, proximity=TRUE)
murderForest

oob.error.data <- data.frame(
  Trees=rep(1:nrow(murderForest$err.rate), times=5),
  Type=rep(c("OOB", "0 or 1", "low", "medium", "high"), each=nrow(murderForest$err.rate)),
  Error=c(murderForest$err.rate[,"OOB"], 
          murderForest$err.rate[,"0 or 1"],
          murderForest$err.rate[,"medium"],
          murderForest$err.rate[,"low"],
          murderForest$err.rate[,"high"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type)) + 
  ggtitle(paste0("Error vs Number of Trees\n",
                 "No. of variables tried at each split: 3"))







oob.values <- vector(length=10)
for(i in 1:10) {
  temp.rf <- randomForest(Murder ~ ., data=training, mtry=i, ntree=1000)
  oob.values[i] <- temp.rf$err.rate[nrow(temp.rf$err.rate),1]
}


oob.values

min(oob.values)

lowestSplit <-which(oob.values == min(oob.values))
lowestSplit


finalForest <- randomForest(Murder ~ ., data=training, ntree=1000, mtry = lowestSplit, proximity=TRUE)
finalForest

oob.error.data <- data.frame(
  Trees=rep(1:nrow(finalForest$err.rate), times=5),
  Type=rep(c("OOB", "0 or 1", "low", "medium", "high"), each=nrow(finalForest$err.rate)),
  Error=c(finalForest$err.rate[,"OOB"], 
          finalForest$err.rate[,"0 or 1"],
          finalForest$err.rate[,"medium"],
          finalForest$err.rate[,"low"],
          finalForest$err.rate[,"high"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type)) + 
  ggtitle(paste0("Error vs Number of Trees\n",
                 "No. of variables tried at each split: ", lowestSplit))


