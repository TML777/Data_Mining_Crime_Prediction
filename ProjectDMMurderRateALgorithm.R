#DM CS 541
#Aigerim Toleukhanova
#Algorithm

install.packages("Metrics")
install.packages("caret")

library(Metrics)
library(tidyverse)
library(dplyr)
library(caret) #for cross validation, and for conf mattrix
library(corrplot) #for corolation
library(plyr)

#download the clean the data with outlayer
mergedMurder<-read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CleanedDataWithOutlier.csv")

mergedMurder$X<-NULL

#Match city names with numbers
mergedMurder$CityNumber<-factor(mergedMurder$NameCity)
mergedMurder$CityNumber<-as.numeric(mergedMurder$CityNumber)

#for further needs delete name of the city
mergedMurder$NameCity<-NULL

#outlier Remove

IQR_Population <- IQR(mergedMurder$Population, na.rm = TRUE)

# Calculate lower and upper bounds
lower_bound <- quantile(mergedMurder$Population, 0.25, na.rm = TRUE) - 3 * IQR_Population
upper_bound <- quantile(mergedMurder$Population, 0.75, na.rm = TRUE) + 3* IQR_Population

# Filter out outliers
mergedMurder <- mergedMurder %>%
  filter(Population >= lower_bound & Population <= upper_bound)

#I will use Linear Regression 
#need Corrolation
correlation_matrix <- cor(mergedMurder, use = "complete.obs")

dev.new()
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

formula <- Murder ~ Population+ Violent+ Robbery+ Burglary+Rape+ Assault+ PropertyCrime
# 
#formula <- Murder ~ Violent+ Robbery+ Burglary+ Arson+Rape+ Assault+ PropertyCrime+ Theft+ SQmiles
# 
# 
 formula <- Murder ~ Violent+ Robbery+ Burglary+Arson+Rape+ Assault+ PropertyCrime+ Theft
# 
# formula <- Murder ~ Violent + Burglary+Arson+Rape+ Assault+ PropertyCrime+Population



#seeds 
set.seed(123)


#######################################Cross Validation###########
#5 fold#5 foldtraining
 train_control<-trainControl(method = "cv", number = 5)


#3 fold#3 foldtraining
#train_control<-trainControl(method = "cv", number = 3)


#10 fold train control for cross validation 
#train_control<-trainControl(method = "cv", number = 10)

#train the model opf LR
modelLR<-train(formula, data = mergedMurder,method = "lm",
               trControl =train_control)

#create prediction result 
predictions<-(predict(modelLR, mergedMurder))
#print the prediction
#(predictions)
options(digits=11)
#Root Mean Squared Error (RMSE)
rmse<- sqrt(mean((predictions - mergedMurder$Murder)^2))
#Mean Squared Error (MSE)
mse<- mse(mergedMurder$Murder, predictions)
# Mean Abs Error
mae <- mean(abs(predictions-mergedMurder$Murder))
#Mean Absolute percentage Error

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


##########confusion matrix############

#splitting predicted data into ranges
murderPredictionsRanged<-droplevels(cut(predictions,c(-1,3,10,30,80)))
murderPredictionsRanged <- revalue(murderPredictionsRanged, 
                                   c("(-1,3]"="0 or 3", "(3,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))

#splitting actual data into ranges
testingMurderRanged<-droplevels(cut(mergedMurder$Murder,c(-1,3,10,30,80)))
testingMurderRanged <- revalue(testingMurderRanged, 
                               c("(-1,3]"="0 or 3", "(3,10]"="low", "(10,30]" = "medium", "(30,80]" = "high"))


conf_matrix <- confusionMatrix(murderPredictionsRanged, testingMurderRanged)
print(conf_matrix)


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
