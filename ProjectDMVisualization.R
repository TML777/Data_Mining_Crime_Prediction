# install.packages("EnvStats")
install.packages("scatterplot3d")
install.packages("plotly")

library(EnvStats)
library(scatterplot3d)
library(ggplot2)
library(plotly)
library(corrplot)

###download already merged data
mergedMurder<- read.csv("C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\CleanedDataWithOutlier.csv")
mergedMurder$X<-NULL

#sort the column based on alphabet

mergedMurder<-mergedMurder[order(mergedMurder$NameCity),]

summary(mergedMurder)

#deal with NA
#log transformation will help with right skew to more normalize
transformData<-log(mergedMurder$Population+1)
hist(transformData,breaks = 30, main = "Histogram with 30 bars", 
     xlab = "Value", col = "lightblue" )

# Add a normal distribution curve
means <- mean(transformData, na.rm=TRUE)
sds<- sd(transformData, na.rm=TRUE)

curve(dnorm(x, mean=means, sd=sds) * length(transformData) * diff(hist(transformData, plot=FALSE)$breaks)[1], 
      add=TRUE, col="red")
#very skewed dist, thus use median 
medians<- median(mergedMurder$Population, na.rm=TRUE)
mergedMurder$Population[is.na(mergedMurder$Population)]<-medians
summary(mergedMurder$Population)

summary(mergedMurder)

view(mergedMurder)
#there is only 2 burlglary,4 property creme,  thus omit 
# Using base R to remove rows with NA in a specific column
mergedMurder <- mergedMurder[!is.na(mergedMurder$Burglary), ]
mergedMurder <- mergedMurder[!is.na(mergedMurder$PropertyCrime), ]
summary(mergedMurder)
#fill data for arson 
hist(mergedMurder$Arson)
means <- mean(mergedMurder$Arson, na.rm=TRUE)
sds<- sd(mergedMurder$Arson, na.rm=TRUE)

curve(dnorm(x, mean=means, sd=sds) * length(mergedMurder$Arson) * diff(hist(mergedMurder$Arson, plot=FALSE)$breaks)[1], 
      add=TRUE, col="red")

#very skewed dist, thus use median 
medians<- median(mergedMurder$Arson, na.rm=TRUE)
mergedMurder$Arson[is.na(mergedMurder$Arson)]<-medians
summary(mergedMurder)




#split the data into train and test
#SEEd
set.seed(1234)
index<-sample(nrow(mergedMurder), .80*nrow(mergedMurder))
training<- mergedMurder[index, ]
test<-mergedMurder[-index,]

summary(mergedMurder)


####run the model by using trained data by regression as DV
options(scipen = 10)

###omit na in traing
training <- na.omit(training) 
model<-lm(Murder~Population+	Violent+Robbery+Robbery+	Arson+Rape+	Assault, data =training)

summary(model)
#view(model$residuals)
#################presents of outlayer of the data###########
##       IQR approach 
training$residual <- model$residuals #we added the residual column into our training set
Q1<- quantile(training$residual, p=.25)
Q3<- quantile(training$residual, p=.75)
IQR<- IQR(training$residual)

#look at equation
min<-Q1-(1.5*IQR)
max<-Q1+(1.5*IQR)


data_nooutlayer<-training%>%filter(residual>=min & residual<=max)%>%select(-residual)

plot(mergedMurder$Population/100000, mergedMurder$Murder)

# ####run regression using data with no outlayer
# model1<-lm(Murder~Population+	Violent+Robbery+Robbery+	Arson+Rape+	Assault, data =data_nooutlayer)
# summary(model1)



# Create a new column for the city numbers
mergedMurder$CityNumber <- as.numeric(factor(mergedMurder$NameCity, 
                                             levels = unique(mergedMurder$NameCity)))

# Print the first few rows to verify the assignment
head(mergedMurder)

#
mergedMurder$X<-NULL
# Create a new column for the Density
mergedMurder$Density<- mergedMurder$Population/mergedMurder$SQmiles
#summary(mergedMurder)

# Remove rows with missing or infinite values
#mergedMurder <- mergedMurder[complete.cases(mergedMurder), ]

#converts some data into numeric
mergedMurder$Murder<- as.numeric(mergedMurder$Murder)
mergedMurder$Years<- as.numeric(mergedMurder$Years)



####   boxplot of  Murder with some outlier
boxplot(mergedMurder$Murder,
        main = "Boxplot of Murder",
        xlab = "Murder",
        ylab = "Murder ",
        outline = TRUE,  # with outlier
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)


####   boxplot of  Murder with no outlier
boxplot(mergedMurder$Murder,
        main = "Boxplot of Murder",
        xlab = "Murder",
        ylab = "Murder ",
        outline = FALSE,  # with no outlier
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)


####   boxplot of Population

PopBoxPlot<- boxplot(mergedMurder$Population/100000, main = "Boxplot of Population with No outlier(in 100K)",
        xlab = "Population",
        ylab = "Population (in 100K)",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)

####   boxplot of Robbery

MurderBoxPlot<-boxplot(mergedMurder$Robbery, main = "Boxplot of Robbery with No outlier",
        xlab = "Robbery",
        ylab = "Robbery",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)

##### boxplot of the Property crime

PropertyBoxPlot<-boxplot(mergedMurder$PropertyCrime, main = "Boxplot of Property Crime with No outlier",
        xlab = "PropertyCrime",
        ylab = "PropertyCrime",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)


##### boxplot of the Violent crime


ViolentBoxPlot<-boxplot(mergedMurder$Violent, main = "Boxplot of Violent with No outlier",
        xlab = "Violent",
        ylab = "Violent",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)

##### boxplot of the ARSON

AraonBoxPlot<-boxplot(mergedMurder$Arson, main = "Boxplot of Arson with No outlier",
        xlab = "Arson",
        ylab = "Arson",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)




                       # Basic Scatter plot and QQ-plot 


plot(mergedMurder$Population, mergedMurder$Murder, main = "Scatter plot of Population vs Murder", 
     xlab = "Population", ylab = "Murder number", col = "blue")
# Regression line by using 
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Population), col = "purple")


qqplot(mergedMurder$Population, mergedMurder$Murder,  main = "QQ plot of Population vs Murder", 
       xlab = "Population", ylab = "Murder number", col = "black")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Population), col = "red")


plot(mergedMurder$Violent, mergedMurder$Murder, main = "Scatter plot of Violent vs Murder", 
    xlab = "Violent", ylab = "Murder number", col = "blue")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Violent), col = "red")
qqplot(mergedMurder$Violent, mergedMurder$Murder,  main = "QQ plot of Violent vs Murder", 
       xlab = "Violent", ylab = "Murder number", col = "black")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Violent), col = "orange")



plot(mergedMurder$Robbery, mergedMurder$Murder, main = "Scatter plot of Robbery vs Murder", 
     xlab = "Robbery", ylab = "Murder number", col = "blue")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Robbery), col = "orange")
qqplot(mergedMurder$Robbery, mergedMurder$Murder,  main = "QQ plot of Robbery vs Murder", 
       xlab = "Robbery", ylab = "Murder number", col = "black")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Robbery), col = "red")


plot(mergedMurder$Arson, mergedMurder$Murder, main = "Scatter plot of Arson vs Murder", 
     xlab = "Arson", ylab = "Murder number", col = "blue")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Arson), col = "red")
qqplot(mergedMurder$Arson, mergedMurder$Murder,  main = "QQ plot of Arson vs Murder", 
       xlab = "Arson", ylab = "Murder number", col = "black")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Arson), col = "red")


plot(mergedMurder$PropertyCrime, mergedMurder$Murder, main = "Scatter plot of PropertyCrime vs Murder", 
     xlab = "PropertyCrime", ylab = "Murder number", col = "blue")
qqplot(mergedMurder$PropertyCrime, mergedMurder$Murder,  main = "QQ plot of PropertyCrime vs Murder", 
       xlab = "PropertyCrime", ylab = "Murder number", col = "black")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$PropertyCrime), col = "red")



#######################################################################################
#                     "Population vs Murder" other method of implimenting

ggplot(mergedMurder, aes(x =Population/100000, y = Murder)) + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "Population vs Murder", x = "pop", y = "murder")

#Linear Regression Line
ggplot(mergedMurder, aes(x = Population/100000, y = Murder)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() + 
  labs(title = "Population vs Murder", x = "pop", y = "murder")

# Correlation Analysis 
cor.test(mergedMurder$Population, mergedMurder$Murder, use = "complete.obs")
#



 #                         Violent vs Murder
ggplot(mergedMurder, aes(x =Violent, y = Murder)) + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "Violent vs Murder", x = "violent", y = "murder")

#Linear Regression Line
ggplot(mergedMurder, aes(x = Violent, y = Murder)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() + 
  labs(title = "Violent vs Murder", x = "violent", y = "murder")

# Correlation Analysis
cor.test(mergedMurder$Violent, mergedMurder$Murder, use = "complete.obs")




#######################################################################################
# Check data types
str(mergedMurder)
#check 
length(mergedMurder$CityNumber)
length(mergedMurder$Years)
length(mergedMurder$Murder)


#data cube :city, year, #murders
with(mergedMurder, {scatterplot3d(x= mergedMurder$CityNumber, 
                                  y = mergedMurder$Years, z = mergedMurder$Murder,
                                  main = " 3D cube plot of CITY, YEAR, MURDERS", 
                                  xlab = "City", ylab = "Year", zlab = "Murders")})

#library(scatterplot3d) Will help to darw 3d


                       #3d scatter plot 
with(mergedMurder, {
  # Create the plot
  s3d <- scatterplot3d(x = mergedMurder$CityNumber,  # City
                       y = mergedMurder$Years,     # Year
                       z = mergedMurder$Population, # Population
                       color = "blue",  # Set point color
                       pch = 16,     # Set point shape
                       main = "3D Scatter Plot",
                       xlab = "City", 
                       ylab = "Year", 
                       zlab = "Population",
                       type = "h",   # Set point type to 'h' for hexagons
                       scale.y = 0.5, # Adjust scale of y-axis
                       angle = 55,    # Set viewing angle
                       box = FALSE    # Disable bounding box
  )
  
  # Add points representing the number of murders
  s3d$points3d(x = mergedMurder$CityNumber, 
           y = mergedMurder$Years, 
           z = mergedMurder$Population, 
           col = "red", 
           pch = 16)
})
###############################looks horrible try other method


#library(plotly) - will give to draw 3D rotate it, zoom it 

# 3D scatter plot
plot_ly(mergedMurder, x = ~CityNumber, y = ~Years, z = ~Population, color = ~Murder, 
        colors = "Reds", marker = list(size = 10), type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "City"), 
                      yaxis = list(title = "Year"), 
                      zaxis = list(title = "Population"))) %>%
  colorbar(title = "Number of Murders")


# 3D scatter plot
plot_ly(mergedMurder, x = ~CityNumber, y = ~Years, z = ~Violent, color = ~Murder, 
        colors = "Reds", marker = list(size = 10), type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "City"), 
                      yaxis = list(title = "Year"), 
                      zaxis = list(title = "Violent"))) %>%
  colorbar(title = "Number of Murders")


# 3D scatter plot
plot_ly(mergedMurder, x = ~NameCity, y = ~Years, z = ~PropertyCrime, color = ~Murder, 
        colors = "Reds", marker = list(size = 16), type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "City"), 
                      yaxis = list(title = "Year"), 
                      zaxis = list(title = "PropertyCrime"))) %>%
  colorbar(title = "Number of Murders")

# 3D scatter plot
plot_ly(mergedMurder, x = ~NameCity, y = ~Years, z = ~Density, color = ~Murder, 
        colors = "Reds", marker = list(size = 16), type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "City"), 
                      yaxis = list(title = "Year"), 
                      zaxis = list(title = "Density"))) %>%
  colorbar(title = "Number of Murders")

#2d scatter plot
plot_ly(mergedMurder, x = ~NameCity, y = ~Population/100000, color = ~Murder, 
        colors = "Reds", type = "scatter", mode = "markers", marker = list(size = 16)) %>%
  layout(xaxis = list(title = "City"), 
         yaxis = list(title = "Population")) %>%
  colorbar(title = "Number of Murders")


####loooks not that pretty, add range for murder
create_range_column <- function(murder_count) {
  ceiling(murder_count / 5) * 5  # Round up to the nearest multiple of 5
}

# Create the range column
mergedMurder$MurderRange <- sapply(mergedMurder$Murder, create_range_column)

# View the updated data frame
head(mergedMurder)

plot_ly(mergedMurder, x = ~NameCity, y = ~Population/100000, color = ~MurderRange, 
        colors = "Reds", type = "scatter", mode = "markers", marker = list(size = 16)) %>%
  layout(xaxis = list(title = "City"), 
         yaxis = list(title = "Population")) %>%
  colorbar(title = "Number of Murders Range")  
#############no change, ask professor


                # Calculate the correlation matrix 

correlation_matrix <- cor(mergedMurder)
#check the structure
str(mergedMurder)

numericMergedMurder<-mergedMurder[sapply(mergedMurder, is.numeric)]
correlation_matrix<-cor(numericMergedMurder)
correlation_matrix


# Visualize the correlation matrix

corrplot(correlation_matrix, method = "circle", 
        # type = "lower",
         tl.col = "black",
         tl.srt = 45,
         diag = TRUE,
         #addCoef.col = "yellow",
         number.cex = 0.7,
         title = "Correlation Plot",
         tl.pos = "lt",
         mar = c(0,0,1,0), 
         addgrid.col = "gray" # Add grid lines for better readability 
)

#somehow gives me warning

#######

#Normalize data
minMaxScale<- function(x){
 (x-min(x))/(max(x)-min(x))
}

normMergedMurder<- as.data.frame((lapply(numericMergedMurder, minMaxScale)))

head(normMergedMurder)
#run corralation matrix in normazided data
correlation_matrix<-cor(numericMergedMurder)
correlation_matrix

# Visualize the correlation matrix

corr_mat<-corrplot(correlation_matrix, method = "circle", 
         # type = "lower",
         tl.col = "black",
         tl.srt = 45,
         diag = TRUE,
         #addCoef.col = "yellow",
         number.cex = 0.7,
         title = "Correlation Plot for Normalized Data",
         tl.pos = "lt",
         mar = c(0,0,1,0), 
         addgrid.col = "gray"  # Add grid lines for better readability
        )


#write.csv(normMergedMurder,"C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/NormalizedDataMurder.csv")

                               #clustering
#help(clusterData)??
