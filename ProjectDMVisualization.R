# install.packages("EnvStats")
install.packages("scatterplot3d")
install.packages("plotly")

# library(EnvStats)
library(scatterplot3d)
library(ggplot2)
library(plotly)


###download already merged data

mergedMurder<- read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CleanedDataWithOutlier.csv")

#sort the column based on alphabet
mergedMurder<-mergedMurder[order(mergedMurder$NameCity),]

summary(mergedMurder)


# Create a new column for the city numbers
mergedMurder$CityNumber <- as.numeric(factor(mergedMurder$NameCity, 
                                             levels = unique(mergedMurder$NameCity)))

# Print the first few rows to verify the assignment
head(mergedMurder)



# Remove rows with missing or infinite values
mergedMurder <- mergedMurder[complete.cases(mergedMurder), ]

mergedMurder$X<-NULL
#converts some data into numeric
mergedMurder$Murder<- as.numeric(mergedMurder$Murder)
mergedMurder$Years<- as.numeric(mergedMurder$Years)



####   boxplot of  Murder
boxplot(mergedMurder$Murder,
        main = "Boxplot of Murder",
        xlab = "Murder",
        ylab = "Murder ",
        outline = FALSE,  # with outlier
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

AraonBoxPlot<-boxplot(mergedMurder$Arson, main = "Boxplot of Arson with No outlier",
        xlab = "Arson",
        ylab = "Arson",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)




                       # Basic Scatter plot and QQplot 


plot(mergedMurder$Population, mergedMurder$Murder, main = "Scatter plot of Population vs Murder", 
     xlab = "Population", ylab = "Murder number", col = "blue")
qqplot(mergedMurder$Population, mergedMurder$Murder,  main = "QQ plot of Population vs Murder", 
       xlab = "Population", ylab = "Murder number", col = "black")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Population), col = "red")


plot(mergedMurder$Violent, mergedMurder$Murder, main = "Scatter plot of Violent vs Murder", 
    xlab = "Violent", ylab = "Murder number", col = "blue")
qqplot(mergedMurder$Violent, mergedMurder$Murder,  main = "QQ plot of Violent vs Murder", 
       xlab = "Violent", ylab = "Murder number", col = "black")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Violent), col = "red")



plot(mergedMurder$Robbery, mergedMurder$Murder, main = "Scatter plot of Robbery vs Murder", 
     xlab = "Robbery", ylab = "Murder number", col = "blue")
qqplot(mergedMurder$Robbery, mergedMurder$Murder,  main = "QQ plot of Robbery vs Murder", 
       xlab = "Robbery", ylab = "Murder number", col = "black")
# Regression line by using 
abline(lm(mergedMurder$Murder ~ mergedMurder$Robbery), col = "red")


plot(mergedMurder$Arson, mergedMurder$Murder, main = "Scatter plot of Arson vs Murder", 
     xlab = "Arson", ylab = "Murder number", col = "blue")
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
#                                Population vs Murder"
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


# Check data types
str(mergedMurder)



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


#************************************
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




#######
#scatter plot -done
#3D scatter plot  done
#pixel oriented 
#clustering 
