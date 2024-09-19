#DM CS 541
#Aigerim Toleukhanova
#Algorithm

# install.packages("Metrics")
# install.packages("caret")
# 
# library(Metrics)
# library(tidyverse)
# library(dplyr)
# library(caret) #for cross validation, and for conf mattrix
# library(corrplot) #for corolation
# library(plyr)
# library(pROC)
# library(ggplot2)# roc

#read clean data 
mergedMurder<-read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CleanedFinalData.csv" )

mergedMurder$X<-NULL

#I will use Linear Regression 
#need Corrolation
correlation_matrix <- cor(mergedMurder, use = "complete.obs")

corrplot(correlation_matrix, method = "circle",
         tl.col = "black",
         tl.srt = 45,
         diag = TRUE,
         number.cex = 0.7,
         title = "Correlation Plot",
         tl.pos = "lt",
         mar = c(1, 1, 1, 1),  # Combined Corrolation matrix
         addgrid.col = "gray")

###### based on corrolation matrix different variation of formulas##########
# Murder ~. #if we consider all features
 formula <- Murder ~ .
# 
# formula <- Murder ~ Population+ Violent+ Robbery+ Burglary+Rape+ Assault+ PropertyCrime
# # 
# formula <- Murder ~ Violent+ Robbery+ Burglary+ Arson+Rape+ Assault+ PropertyCrime+ Theft+ SQmiles
# # 
# 
#formula <- Murder ~ Violent+ Robbery+ Burglary+Arson+Rape+ Assault+ PropertyCrime+ Theft
# 
# formula <- Murder ~ Violent + Burglary+Arson+Rape+ Assault+ PropertyCrime+Population 
# formula <- Murder ~ Violent+ Robbery+ Burglary+ Arson+Rape+ Assault+ PropertyCrime+ CityNumber
#seeds 
set.seed(123)

#######################################Cross Validation###########
#5 fold#5 foldtraining
 train_control<-trainControl(method = "cv", number = 5)


#3 fold#3 foldtraining
#train_control<-trainControl(method = "cv", number = 3)


#10 fold train control for cross validation 
#train_control<-trainControl(method = "cv", number = 10)

#########train the model of LR####

#process of trainig the model
modelLR<-train(formula, data = mergedMurder,method = "lm",
               trControl =train_control)

#create prediction result 
predictions<-(predict(modelLR, mergedMurder))

#print the prediction
(predictions)

options(digits=11)
###############Evaluate#########################
#Root Mean Squared Error (RMSE)
rmse<- sqrt(mean((predictions - mergedMurder$Murder)^2))
#Mean Squared Error (MSE)
mse<- mse(mergedMurder$Murder, predictions)
# Mean Abs Error
mae <- mean(abs(predictions-mergedMurder$Murder))

#Mean Absolute percentage Error
#can not have zero, thus must add small value
actual <- mergedMurder$Murder + 0.0001
predicted <- predictions + 0.0001

# Set higher precision for console output
options(digits=11)

# Calculate MAPE
MAPE <- mape(actual, predicted)*100

# Calculate accuracy
accuracy <- 100.0 - MAPE

# Print the results with high precision

cat("MAE:", mae, "\nMSE:", mse, "\nRMSE:", rmse, "\nMAPE:", MAPE,
    "%\nAccuracy:", accuracy, "%\n")

#all about model 
(modelLR)
summary(modelLR)
?lm

# plot prediction vs actua;s
ggplot(mergedMurder, aes(x = mergedMurder$Murder, y = predictions)) + 
  geom_point() +  # This adds the scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +  
  labs(x = "Actual Values", y = "Predicted Values", title = "Plot of Actual vs. Predicted Values") +
  theme_minimal()



##########confusion matrix############

# #splitting predicted data into ranges: "(-1,3]"="0 or 3", "(3,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"
# murderPredictionsRanged<-droplevels(cut(predictions,c(-1,3,10,30,80)))
# murderPredictionsRanged <- revalue(murderPredictionsRanged,
#                                    c("(-1,3]"="0 to 3", "(3,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))
# 
# #splitting actual data into ranges: "(-1,3]"="0 or 3", "(3,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"
# testingMurderRanged<-droplevels(cut(mergedMurder$Murder,c(-1,3,10,30,80)))
# testingMurderRanged <- revalue(testingMurderRanged,
#                                c("(-1,3]"="0 to 3", "(3,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))


#splitting predicted data into ranges
murderPredictionsRanged<-droplevels(cut(predictions,c(-1,1,10,30,80)))
murderPredictionsRanged <- revalue(murderPredictionsRanged, 
                                   c("(-1,1]"="0 or 1", "(1,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))

#splitting actual data into ranges
testingMurderRanged<-droplevels(cut(mergedMurder$Murder,c(-1,1,10,30,80)))
testingMurderRanged <- revalue(testingMurderRanged, 
                               c("(-1,1]"="0 or 1", "(1,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))

conf_matrix <- confusionMatrix(murderPredictionsRanged, testingMurderRanged)
print(conf_matrix)
# Convert confusion matrix to a data frame for easier plotting
conf_matrix_df <- as.data.frame(conf_matrix$table)
names(conf_matrix_df) <- c('Predicted', 'Actual', 'Count')

# Plot the confusion matrix using ggplot2
ggplot(data = conf_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count)) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Predicted")

#Plot of actual vs predicted of range
ggplot(mergedMurder, aes(x = testingMurderRanged, y = murderPredictionsRanged)) + 
  geom_point() +  # This adds the scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # This adds the regression line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +  # Perfect fit line
  labs(x = "Actual Values", y = "Predicted Values", title = "Plot of Actual vs. Predicted Values") +
  theme_minimal()


ggplot(mergedMurder, aes(x = testingMurderRanged, y = murderPredictionsRanged)) +
  geom_point(aes(color = abs(testingMurderRanged - murderPredictionsRanged)), alpha = 0.6, size = 3) +
  geom_line(group = 1, color = "grey40", linetype = "twodash") +  # Connects the points with lines
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adds the regression line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +  # Perfect fit line
  scale_color_gradient(low = "blue", high = "red", name = "Error Magnitude") +  # Color gradient
  labs(x = "Actual Values in Range", y = "Predicted Values in Range", title = "Plot of Actual vs. Predicted Values in Range") +
  theme_minimal() +
  theme(legend.position = "right")



# ############### splitting the data to train vs test ##########
# trainIndex <- createDataPartition(y = mergedMurder$Murder,
#                                   p=0.8, list = FALSE)
# training <- mergedMurder[trainIndex,]
# testing <- mergedMurder[-trainIndex,]
# model<-lm(formula, training)
# (predictions<-round(predict(model, testing)))
# 
# small_constant<- 0.1
# 
# #mape = ((|actual-predicted|)/actual)*100%
# new_mape<- mean(abs(((predictions + small_constant) -
#                        (testing$Murder + small_constant)) /
#                       (testing$Murder + small_constant))) *100
# # mean absolute percentage error:
# print(paste("New adjusted Mape: ",new_mape))
# print(paste("Accuracy in %: ",100-new_mape))
# 
# #Root Mean Squared Error (RMSE)
# rmse<- sqrt(mean((predictions - testing$Murder)^2))
# 
# # Mean Abs Error
# mae <- mean(abs(predictions-testing$Murder))
# #result
# print(paste("RMSE:", round(rmse,12)))
# print(paste("MAE:",  round(mae,13)))
# print(paste("New adjusted Mape: ",new_mape))
# print(paste("Accuracy in %: ",100-new_mape))
# 
# #splitting predicted data into ranges
# murderPredictionsRanged<-droplevels(cut(predictions,c(-1,3,10,30,80)))
# murderPredictionsRanged <- revalue(murderPredictionsRanged, 
#                                    c("(-1,3]"="0 or 3", "(3,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))
# 
# #splitting actual data into ranges
# testingMurderRanged<-droplevels(cut(testing$Murder,c(-1,3,10,30,80)))
# testingMurderRanged <- revalue(testingMurderRanged, 
#                                c("(-1,3]"="0 or 3", "(3,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))
# 
# 
# conf_matrix <- confusionMatrix(murderPredictionsRanged, testingMurderRanged)
# print(conf_matrix)
# 
