#read file from csv
library(tidyverse)
library(vroom)
library(readr)
                            #save all samples in one list

folder_path <-"C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV"
#folder_path <-"C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV"
file.exists(folder_path)
file_paths <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

#create empty list and call allSamples
allSamples <- list()

#combine all cvs files into list
for(file_path in file_paths) {
  table_name <- tools::file_path_sans_ext(basename(file_path))
  allSamples[[table_name]] <- read.csv(file_path)
}
allSamples$NormalizedDataMurder<-NULL

#                           #convert City into String
# allSamples$crimeCalifornia2005$City<-as.character(allSamples$crimeCalifornia2005$City)
# allSamples$crimeCalifornia2006$City<-as.character(allSamples$crimeCalifornia2006$City)
# allSamples$crimeCalifornia2007$City<-as.character(allSamples$crimeCalifornia2007$City)
# allSamples$crimeCalifornia2008$City<-as.character(allSamples$crimeCalifornia2008$City)
# allSamples$crimeCalifornia2009$City<-as.character(allSamples$crimeCalifornia2009$City)
# allSamples$crimeCalifornia2010$City<-as.character(allSamples$crimeCalifornia2010$City)
# allSamples$crimeCalifornia2011$City<-as.character(allSamples$crimeCalifornia2011$City)
# allSamples$crimeCalifornia2012$City<-as.character(allSamples$crimeCalifornia2012$City)
# allSamples$crimeCalifornia2013$City<-as.character(allSamples$crimeCalifornia2013$City)
# allSamples$crimeCalifornia2014$City<-as.character(allSamples$crimeCalifornia2014$City)
# allSamples$crimeCalifornia2015$City<-as.character(allSamples$crimeCalifornia2015$City)
# allSamples$crimeCalifornia2016$City<-as.character(allSamples$crimeCalifornia2016$City)
# allSamples$crimeCalifornia2017$City<-as.character(allSamples$crimeCalifornia2017$City)
# allSamples$crimeCalifornia2018$City<-as.character(allSamples$crimeCalifornia2018$City)

                             #convert char to numerical values

# deleting the "," in each column and convert it into numerical


for(i in 4:ncol(allSamples$crimeCalifornia2005)){
  allSamples$crimeCalifornia2005[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2005[[i]]))
}

for(i in 4:ncol(allSamples$crimeCalifornia2006)){
  allSamples$crimeCalifornia2006[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2006[[i]]))
}

for(i in 4:ncol(allSamples$crimeCalifornia2007)){
  allSamples$crimeCalifornia2007[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2007[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2008)){
  allSamples$crimeCalifornia2008[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2008[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2009)){
  allSamples$crimeCalifornia2009[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2009[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2010)){
  allSamples$crimeCalifornia2010[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2010[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2011)){
  allSamples$crimeCalifornia2011[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2011[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2012)){
  allSamples$crimeCalifornia2012[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2012[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2013)){
  allSamples$crimeCalifornia2013[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2013[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2014)){
  allSamples$crimeCalifornia2014[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2014[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2015)){
  allSamples$crimeCalifornia2015[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2015[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2016)){
  allSamples$crimeCalifornia2016[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2016[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2017)){
  allSamples$crimeCalifornia2017[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2017[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2018)){
  allSamples$crimeCalifornia2018[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2018[[i]]))
}
for(i in 4:ncol(allSamples$crimeCalifornia2019)){
  allSamples$crimeCalifornia2019[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2019[[i]]))
}
#view( allSamples$crimeCalifornia2019)

                                  #Create new Year attribute for each list's entry
allSamples$crimeCalifornia2005$Years<-2005
allSamples$crimeCalifornia2006$Years<-2006
allSamples$crimeCalifornia2007$Years<-2007
allSamples$crimeCalifornia2008$Years<-2008
allSamples$crimeCalifornia2009$Years<-2009
allSamples$crimeCalifornia2010$Years<-2010
allSamples$crimeCalifornia2011$Years<-2011
allSamples$crimeCalifornia2012$Years<-2012
allSamples$crimeCalifornia2013$Years<-2013
allSamples$crimeCalifornia2014$Years<-2014
allSamples$crimeCalifornia2015$Years<-2015
allSamples$crimeCalifornia2016$Years<-2016
allSamples$crimeCalifornia2017$Years<-2017
allSamples$crimeCalifornia2018$Years<-2018
allSamples$crimeCalifornia2019$Years<-2019



#write.csv(allSamples$crimeCalifornia2006,"C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\crimeCalifornia2006.csv")
#allSamples$crimeCalifornia2006<-read.csv("C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\crimeCalifornia2006.csv")

#allSamples$crimeCalifornia2016<-read.csv("C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\crimeCalifornia2016.csv")
#view(allSamples$crimeCalifornia2006)

                                 #combine all enties of each list
combinedAllSamples <- bind_rows(allSamples, .id = "Dataset")
view(combinedAllSamples)

                 #delete unnessesary columns
combinedAllSamples$X.1<-NULL
combinedAllSamples$X<-NULL
combinedAllSamples$Dataset<-NULL


# write.csv(combinedAllSamples, "C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\combinedNotCleanedMurderData.csv")

                             #too many attributes, must combine some 
#combine rows with of city and City then create new column NameCity
combinedAllSamples <-combinedAllSamples%>%
  mutate(NameCity = case_when(
    !is.na(State)~State,
    !is.na(City)~City,
    TRUE~NA_character_
  ))%>%select(-City, -State)#delete original city and City


#combinedAllSamples$Violent.crime<-as.numeric(combinedAllSamples$Violent.crime)

combinedAllSamples <- combinedAllSamples %>%
  mutate(Violent = case_when(
    !is.na(Violentcrime) ~ Violentcrime,
    !is.na(Violent.crime) ~ Violent.crime,
    !is.na(Violent..crime) ~ Violent..crime,
    TRUE ~ NA_real_
  ))

combinedAllSamples$Violentcrime<-NULL
combinedAllSamples$Violent.crime<-NULL
combinedAllSamples$Violent..crime<-NULL

# combinedAllSamples <-combinedAllSamples%>%
#   mutate(Population = case_when(
#     !is.na(Population)~Population,
#     !is.na(population)~population,
#     TRUE~NA_real_
#   ))%>%select(-population)

combinedAllSamples <-combinedAllSamples%>%
  mutate(Murder = case_when(
    !is.na(Murder.and..nonnegligent..manslaughter)~Murder.and..nonnegligent..manslaughter,
    !is.na(Murder.and.nonnegligent.manslaughter)~Murder.and.nonnegligent.manslaughter,
    !is.na(Murder.andnonnegligentmanslaughter)~Murder.andnonnegligentmanslaughter,
    TRUE~NA_real_
  ))%>%select(-Murder.andnonnegligentmanslaughter,-Murder.and.nonnegligent.manslaughter,
              -Murder.and..nonnegligent..manslaughter)

summary(combinedAllSamples$Murder)
combinedAllSamples <-combinedAllSamples%>%
  mutate(Rape = case_when(
    !is.na(Rape1)~Rape1,
    !is.na(Forcible.rape)~Forcible.rape,
    !is.na(Forciblerape)~Forciblerape,
    !is.na(Rape.reviseddefinition.1)~Rape.reviseddefinition.1,
    !is.na(Rape.reviseddefinition1.)~Rape.reviseddefinition1.,
    !is.na(Rape.legacydefinition.2)~Rape.legacydefinition.2,
    !is.na(Rape.legacydefinition2.)~Rape.legacydefinition2.,
    TRUE~NA_real_
  ))%>%select(-Forcible.rape, -Forciblerape, -Rape.reviseddefinition.1, 
              -Rape.reviseddefinition1.,-Rape.legacydefinition.2, 
              -Rape.legacydefinition2., -Rape1)

#Robbery was good


combinedAllSamples <-combinedAllSamples%>%
  mutate(Assault = case_when(
    !is.na(Aggravated.assault)~Aggravated.assault,
    !is.na(Aggravatedassault)~Aggravatedassault,
    TRUE~NA_real_))%>%select(-Aggravated.assault, -Aggravatedassault)

#burglary was good

combinedAllSamples <-combinedAllSamples%>%
  mutate(PropertyCrime = case_when(
    !is.na(Property.crime)~Property.crime,
    !is.na(Propertycrime)~Propertycrime,
    TRUE~NA_real_))%>%select(-Propertycrime, -Property.crime)


combinedAllSamples <-combinedAllSamples%>%
  mutate(Theft = case_when(
    !is.na(Larceny.theft)~Larceny.theft,
    !is.na(Larceny..theft)~Larceny..theft,
    !is.na(Motor.vehicle.theft)~Motor.vehicle.theft,
    !is.na(Motor..vehicle.theft)~Motor..vehicle.theft,
    !is.na(Motorvehicletheft)~Motorvehicletheft,
    TRUE~NA_real_))%>%select(-Larceny.theft, -Larceny..theft,
                             -Motor..vehicle.theft, -Motor.vehicle.theft, -Motorvehicletheft)

combinedAllSamples <-combinedAllSamples%>%
  mutate(Arson = case_when(
    !is.na(Arson)~Arson,
    !is.na(Arson1)~Arson1,
    TRUE~NA_real_))%>%select(-Arson1)


summary(combinedAllSamples)

#write.csv(combinedAllSamples,"C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\MergedCleanedData.csv" )


#combinedAllSamples<-read.csv("C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\CleanedDataWithOutlier.csv")
                          #deal with NA
mergedMurder <-combinedAllSamples
summary(mergedMurder)
                         #population
# Calculate mean and standard deviation
means <- mean(mergedMurder$Population, na.rm=TRUE)
sds <- sd(mergedMurder$Population, na.rm=TRUE)

# Determine the range of your data for plotting
xrange <- range(mergedMurder$Population )

# Plot histogram
hist(mergedMurder$Population , breaks = 30, main = "Histogram Population no Outlier", 
     xlab = "Population", col = "lightblue", freq=FALSE, xlim=xrange)

# Adding a normal distribution curve
curve(dnorm(x, mean=means, sd=sds), 
      from=min(mergedMurder$Population ), to=max(mergedMurder$Population ), 
      add=TRUE, col="red", lwd=2)

# log transform 
log_population <- log(mergedMurder$Population)

# mean and stdv
log_means <- mean(log_population, na.rm=TRUE)
log_sds <- sd(log_population, na.rm=TRUE)

# density of the histogram
hist_density <- hist(log_population, breaks=30, plot=FALSE)

# 
hist(log_population, breaks=30, main="Histogram of Log-transformed Population",
     xlab="Log(Population)", col="lightblue", freq=FALSE)

# x range range of the histogram
xvals <- seq(min(hist_density$breaks), max(hist_density$breaks), length=200)

#  y-values for the normal distribution curve
yvals <- dnorm(xvals, mean=log_means, sd=log_sds)

#adjsut
max_density <- max(hist_density$density)
scaled_yvals <- yvals * (max_density / max(yvals))

#add curve to the histogram
lines(xvals, scaled_yvals, col="darkred", lwd=2)

   
#          #log transformation will help you with shifts
# transformData<-log(mergedMurder$Population+1)
# 
# hist(transformData,breaks = 30, main = "Histogram With Transformed LOG population", 
#      xlab = "transformed population Data", col = "lightblue",  )
# 
# # Add a normal distribution curve
# means <- mean(transformData, na.rm=TRUE)
# sds<- sd(transformData, na.rm=TRUE)
# 
# curve(dnorm(x, mean=means, sd=sds) * length(transformData) * diff(hist(transformData, plot=FALSE)$breaks)[1], 
#       add=TRUE, col="red")

#original population distr is norm dist, thus use means 
means<- mean(mergedMurder$Population, na.rm=TRUE)
mergedMurder$Population[is.na(mergedMurder$Population)]<-means
summary(mergedMurder$Population)

#view(mergedMurder)

#there is only 2 burlglary,4 property creme,  thus omit 
# Using base R to remove rows with NA in a specific column
mergedMurder <- mergedMurder[!is.na(mergedMurder$Burglary), ]
mergedMurder <- mergedMurder[!is.na(mergedMurder$PropertyCrime), ]
mergedMurder <- mergedMurder[!is.na(mergedMurder$Assault), ]
mergedMurder <- mergedMurder[!is.na(mergedMurder$Violent), ]
summary(mergedMurder)


#fill data for arson 
hist(mergedMurder$Arson, breaks = 30)
means <- mean(mergedMurder$Arson, na.rm=TRUE)
sds<- sd(mergedMurder$Arson, na.rm=TRUE)

curve(dnorm(x, mean=means, sd=sds) * length(mergedMurder$Arson) * diff(hist(mergedMurder$Arson, plot=FALSE)$breaks)[1], 
      add=TRUE, col="red")

#very skewed try to normilize for better representation
transformData<-log(mergedMurder$PropertyCrime)

hist(transformData,breaks = 30, main = "Histogram with 30 bars", 
     xlab = "transformed Data Property Crime", col = "lightblue" )

# Add a normal distribution curve
means <- mean(transformData, na.rm=TRUE)
sds<- sd(transformData, na.rm=TRUE)

curve(dnorm(x, mean=means, sd=sds) * length(transformData) * diff(hist(transformData, plot=FALSE)$breaks)[1], 
      add=TRUE, col="red")

#very skewed distribution in the beginning, thus use median 
medians<- median(mergedMurder$Arson, na.rm=TRUE)
mergedMurder$Arson[is.na(mergedMurder$Arson)]<-medians
summary(mergedMurder)

###########################we have no more NA's in our data#############################################

                        #add the area data    

#download "C:\\Users\\aiger\\Downloads\\AreaTable.csv"

area<-read.csv("C:\\Users\\aiger\\Downloads\\AreaTable.csv")

                    #CLEAN the area
                   #delete unnessary columns, add new columns
area$X<-NULL
area$X3<-NULL
area$City<-area$X3
area$SQmiles<- area$X2

area$X2<-NULL
area$X3<-NULL
#delete row 1 
area<-area[-c(1), ]
#get ridNULL#get rid of outliers 


#delete everything after comma , 
area$City<-gsub(",.*", "", area$City)
#delete sq.miles for each entry 
area$SQmiles<-gsub("s.*", "", area$SQmiles)

#convert into numeric values
area$SQmiles<-as.numeric(area$SQmiles)
#area$Density<-as.numeric(area$SQmiles/area$SQmiles)

# Extract unique city names from mergedMurder$NameCity
murder_cities <- unique(mergedMurder$NameCity)
murder_cities
# Filter area dataset to only include rows where city is in murder_cities
area <- area %>%
  filter( City %in% murder_cities)

# Remove the trailing "3" from the city names
mergedMurder$NameCity <- gsub("3$", "", mergedMurder$NameCity)

# Substitute "Yuba" with "Yuba City" in mergedMurder$NameCity
mergedMurder$NameCity <- gsub("Yuba$", "Yuba City", mergedMurder$NameCity)
#similarly 

mergedMurder$NameCity <- gsub("Big Bear$", "Big Bear Lake", mergedMurder$NameCity)
mergedMurder$NameCity <- gsub("Vallejo1$", "Vallejo", mergedMurder$NameCity)


mergedMurder$NameCity <- gsub("Rancho Santa Margarit$", "Rancho Santa Margarita", mergedMurder$NameCity)
mergedMurder$NameCity <- gsub("Nevada$", "Nevada City", mergedMurder$NameCity)
mergedMurder$NameCity <- gsub("$", "", mergedMurder$NameCity)
mergedMurder$NameCity <- gsub("$", "", mergedMurder$NameCity)


                 # Merge the datasets based on matching city names
mergedMurder<-merge(mergedMurder, area,  by.x = "NameCity", by.y = "City", all.x = TRUE)
summary(mergedMurder)


   # Find the cities in area that are not in mergedMurder
notExistingCity<- anti_join(area, mergedMurder, by = c("City" = "NameCity"))$City
notExistingCity


unique_cities_area <- unique(area$City)
unique_cities_merged <- unique(mergedMurder$NameCity)

not_in_merged <- setdiff(unique_cities_area, unique_cities_merged)
not_in_area <- setdiff(unique_cities_merged, unique_cities_area)

not_in_merged
not_in_area

            # Filter rows where sqmiles is NA 
(na_rows <- mergedMurder[is.na(mergedMurder$SQmiles), ])

         # View the rows where NameCity is NA
unique(na_rows$NameCity)

#Notice there is some misspell in data, delete them

      # Remove the trailing "3" from the city names
mergedMurder$NameCity <- gsub("3$", "", mergedMurder$NameCity)

# Now, the trailing "3" should be removmergedMurder# Now, the trailing "3" should be removed from the city names in mergedMurder$NameCity column


# Merge the datasets based on matching city names
mergedMurder<-merge(mergedMurder, area,  by.x = "NameCity", by.y = "City", all.x = TRUE)
summary(mergedMurder)


               #####DEAL with NA###SQmiles##DEAL with NA
mergedMurder$SQmiles <- as.numeric(mergedMurder$SQmiles)
hist(mergedMurder$SQmiles,breaks = 30, main = "Histogram with 30 bars", 
xlab = "SQmiles", col = "lightblue" )

# Add a normal distribution curve
means <- mean(mergedMurder$SQmiles, na.rm=TRUE)
sds<- sd(mergedMurder$SQmiles, na.rm=TRUE)

curve(dnorm(x, mean=means, sd=sds) * length(mergedMurder$SQmiles) * diff(hist(mergedMurder$SQmiles, plot=FALSE)$breaks)[1], 
      add=TRUE, col="red")


# skewed distribution in the beginning, thus use median 
medians<- median(mergedMurder$SQmiles, na.rm=TRUE)
mergedMurder$SQmiles[is.na(mergedMurder$SQmiles)]<-medians
summary(mergedMurder)
                        ############NO MORE NA#######################
                       
####   boxplot of Population in 100K
boxplot(mergedMurder$Population/100000,
        main = "Boxplot of Population (in 100K)",
        xlab = "Population",
        ylab = "Population (in 100K)",
        outline = TRUE,  # with outlier
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)


boxplot(mergedMurder$Population/100000, main = "Boxplot of Population with No outlier(in 100K)",
        xlab = "Population",
        ylab = "Population (in 100K)",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)



                         #calculate the outlier, and filter data

# Calculate the first quartile, third quartile 
Q1 <- quantile(mergedMurder$Population, 0.25)
Q3 <- quantile(mergedMurder$Population, 0.75)

# interquartile range (IQR)
IQR <- Q3 - Q1
#the lower and upper bounds
lower <- Q1 -3* IQR
upper <- Q3 + 3 * IQR

#3 is multiplier for changing the bounds

cleanedMurder<-mergedMurder[mergedMurder$Population>=lower & 
                              mergedMurder$Population<=upper,]

boxplot(cleanedMurder$Population/100000, horizontal = TRUE, 
        main = "Less Outlier PLOT of population", xlab = "Population",
        ylab = "Population (in 100K)"
        )
#nu,ber of data we lost while cleaning


####   boxplot of  Violent
boxplot(mergedMurder$Violent,
        main = "Boxplot of Violent",
        xlab = "Violent",
        ylab = "Violent ",
        outline = TRUE,  # with outlier
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)


boxplot(mergedMurder$Violent, main = "Boxplot of Violent with No outlier",
        xlab = "Violent",
        ylab = "Violent",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)


               ####   boxplot of  Robbery
boxplot(mergedMurder$Robbery,
        main = "Boxplot of Robbery",
        xlab = "Robbery",
        ylab = "Robbery ",
        outline = TRUE,  # with outlier
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)


boxplot(mergedMurder$Robbery, main = "Boxplot of Robbery with No outlier",
        xlab = "Robbery",
        ylab = "Robbery",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)

                ####   boxplot of  PropertyCrime
boxplot(mergedMurder$PropertyCrime,
        main = "Boxplot of PropertyCrime",
        xlab = "PropertyCrime",
        ylab = "PropertyCrime ",
        outline = TRUE,  # with outlier
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)


boxplot(mergedMurder$PropertyCrime, main = "Boxplot of PropertyCrime with No outlier",
        xlab = "PropertyCrime",
        ylab = "PropertyCrime",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)


                ####   boxplot of  Murder
boxplot(mergedMurder$Murder,
        main = "Boxplot of Murder",
        xlab = "Murder",
        ylab = "Murder ",
        outline = TRUE,  # with outlier
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)


boxplot(mergedMurder$Murder, main = "Boxplot of Murder with No outlier",
        xlab = "Murder",
        ylab = "Murder",
        outline = FALSE,  # Remove outliers from the plot
        col = "skyblue",  # Change boxplot color
        border = "darkblue",  # Change border color
        horizontal = TRUE)    # Display boxplot horizontally)

#write.csv(mergedMurder,"C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\CleanedDataWithOutlier.csv" )
#write.csv(cleanedMurder,"C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\CleanedData.csv" )
#blabla<-read.csv("C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\CleanedDataWithOutlier.csv")


###############Some alteration and outlier filter seprately########
#download the clean the data with outlayer and filter outlier for further needs
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
