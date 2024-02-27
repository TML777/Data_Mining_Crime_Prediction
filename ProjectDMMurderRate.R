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

#delete 2014 data, keep data 2014edited
allSamples$crimeCalifornia2014<-NULL

                             #convert char to numerical values
# deleting the "," in each column and convert it into numerical

for(i in 2:12){
  allSamples$crimeCalifornia2005[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2005[[i]]))
}

for(i in 3:ncol(allSamples$crimeCalifornia2006)){
  allSamples$crimeCalifornia2006[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2006[[i]]))
}
view(allSamples$crimeCalifornia2006)
for(i in 2:ncol(allSamples$crimeCalifornia2007)){
  allSamples$crimeCalifornia2007[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2007[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2008)){
  allSamples$crimeCalifornia2008[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2008[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2009)){
  allSamples$crimeCalifornia2009[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2009[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2010)){
  allSamples$crimeCalifornia2010[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2010[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2011)){
  allSamples$crimeCalifornia2011[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2011[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2012)){
  allSamples$crimeCalifornia2012[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2012[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2013)){
  allSamples$crimeCalifornia2013[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2013[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2014Edited)){
  allSamples$crimeCalifornia2014Edited[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2014Edited[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2015)){
  allSamples$crimeCalifornia2015[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2015[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2016)){
  allSamples$crimeCalifornia2016[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2016[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2017)){
  allSamples$crimeCalifornia2017[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2017[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2018)){
  allSamples$crimeCalifornia2018[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2018[[i]]))
}
for(i in 2:ncol(allSamples$crimeCalifornia2019)){
  allSamples$crimeCalifornia2019[[i]]<-as.numeric(gsub(",", "", allSamples$crimeCalifornia2019[[i]]))
}

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
allSamples$crimeCalifornia2014Edited$Years<-2014
allSamples$crimeCalifornia2015$Years<-2015
allSamples$crimeCalifornia2016$Years<-2016
allSamples$crimeCalifornia2017$Years<-2017
allSamples$crimeCalifornia2018$Years<-2018
allSamples$crimeCalifornia2019$Years<-2019



                               #delete unnesesary columns

allSamples$crimeCalifornia2006$X<- NULL
allSamples$crimeCalifornia2008$X<- NULL
allSamples$crimeCalifornia2008$X.1<- NULL
allSamples$crimeCalifornia2008$X.2<- NULL
allSamples$MergedCleanedData<-NULL

#write.csv(allSamples$crimeCalifornia2006,"C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\crimeCalifornia2006.csv")
#allSamples$crimeCalifornia2006<-read.csv("C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\crimeCalifornia2006.csv")

#allSamples$crimeCalifornia2016<-read.csv("C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\crimeCalifornia2016.csv")
view(allSamples$crimeCalifornia2006)

                                 #combine all enties of each list

combinedAllSamples <- bind_rows(allSamples, .id = "Dataset")
view(combinedAllSamples)


                                #too many attributes, must combine some 
#combine rows with of city and City then create new column NameCity
combinedAllSamples <-combinedAllSamples%>%
  mutate(NameCity = case_when(
    !is.na(city)~city,
    !is.na(City)~City,
    TRUE~NA_character_
  ))%>%select(-City, -city)#delete original city and City

combinedAllSamples$Violent.crime<-as.numeric(combinedAllSamples$Violent.crime)
combinedAllSamples$violent<-as.numeric(combinedAllSamples$violent)
combinedAllSamples$Violent<-as.numeric(combinedAllSamples$Violent)


combinedAllSamples <-combinedAllSamples%>%
  mutate(Violent = case_when(
    !is.na(violent)~violent,
    !is.na(Violent.crime)~Violent.crime,
    !is.na(Violent)~Violent,
    TRUE~NA_real_
  ))

combinedAllSamples$violent<-NULL
combinedAllSamples$Violent1<-NULL
combinedAllSamples$Violent.crime<-NULL

combinedAllSamples <-combinedAllSamples%>%
  mutate(Population = case_when(
    !is.na(Population)~Population,
    !is.na(population)~population,
    TRUE~NA_real_
  ))%>%select(-population)

combinedAllSamples <-combinedAllSamples%>%
  mutate(Murder = case_when(
    !is.na(Murder)~Murder,
    !is.na(murder)~murder,
    !is.na(Murder.and.nonnegligent.manslaughter)~Murder.and.nonnegligent.manslaughter,
    TRUE~NA_real_
  ))%>%select(-Murder.and.nonnegligent.manslaughter,-murder)

combinedAllSamples <-combinedAllSamples%>%
  mutate(Rape = case_when(
    !is.na(Forcible.rape)~Forcible.rape,
    !is.na(rape)~rape,
    !is.na(Rape..legacy.definition.2)~Rape..legacy.definition.2,
    !is.na(Rape..revised.definition.1)~Rape..revised.definition.1,
    TRUE~NA_real_
  ))%>%select(-rape, -Forcible.rape, -Rape..revised.definition.1, -Rape..legacy.definition.2)

combinedAllSamples <-combinedAllSamples%>%
  mutate(Robbery = case_when(
    !is.na(Robbery)~Robbery,
    !is.na(robbery)~robbery,
    TRUE~NA_real_))%>%select(-robbery)


combinedAllSamples <-combinedAllSamples%>%
  mutate(Assault = case_when(
    !is.na(Aggravated.assault)~Aggravated.assault,
    !is.na(Aggravatedassault)~Aggravatedassault,
    TRUE~NA_real_))%>%select(-Aggravated.assault, -Aggravatedassault)

combinedAllSamples <-combinedAllSamples%>%
  mutate(Burglary = case_when(
    !is.na(burglary)~burglary,
    !is.na(Burglary)~Burglary,
    TRUE~NA_real_))%>%select(-burglary)

combinedAllSamples <-combinedAllSamples%>%
  mutate(PropertyCrime = case_when(
    !is.na(Property.crime)~Property.crime,
    !is.na(Propertycrime)~Propertycrime,
    TRUE~NA_real_))%>%select(-Propertycrime, -Property.crime)



combinedAllSamples <-combinedAllSamples%>%
  mutate(Theft = case_when(
    !is.na(Larceny.threft)~Larceny.threft,
    !is.na(Larceny.theft)~Larceny.theft,
    !is.na(Larceny..theft)~Larceny..theft,
    !is.na(Motor.Vehicle.threft)~Motor.Vehicle.threft,
    !is.na(Motor.vehicle.theft)~Motor.vehicle.theft,
    !is.na(Motorvehicletheft)~Motorvehicletheft,
    TRUE~NA_real_))%>%select(-Larceny.threft, -Larceny.theft, -Larceny..theft,
                             -Motor.Vehicle.threft, -Motor.vehicle.theft, -Motorvehicletheft)

combinedAllSamples <-combinedAllSamples%>%
  mutate(Arson = case_when(
    !is.na(Arson)~Arson,
    !is.na(arson)~arson,
    !is.na(Arson1)~Arson1,
    !is.na(even.7)~even.7,
    TRUE~NA_real_))%>%select(-arson, -Arson1, -even.7)

combinedAllSamples<-combinedAllSamples%>%
  filter(!if_any(NameCity, is.na))

combinedAllSamples$NameCity<-as.character(combinedAllSamples$NameCity)
combinedAllSamples<-combinedAllSamples%>%
  filter(!if_any(Violent, is.na))

summary(combinedAllSamples)
#looks clean, but has some NA's
#write.csv(combinedAllSamples,"C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\MergedCleanedData.csv" )



                          #deal with NA
mergedMurder <-combinedAllSamples
summary(mergedMurder)
                         #population
hist(mergedMurder$Population,breaks = 30, main = "Histogram with 30 bars", 
     xlab = "Population", col = "lightblue" )

#Add a normal distribution curve
means <- mean(mergedMurder$Population, na.rm=TRUE)
sds<- sd(mergedMurder$Population, na.rm=TRUE)

curve(dnorm(x, mean=means, sd=sds) * length(mergedMurder$Population) * diff(hist(mergedMurder$Population, plot=FALSE)$breaks)[1], 
      add=TRUE, col="red")
         #skewed try to normalize for brtter visualization
         #log transformation will help with right skew to more normalize
transformData<-log(mergedMurder$Population+1)

hist(transformData,breaks = 30, main = "Histogram with 30 bars", 
     xlab = "transformed population Data", col = "lightblue" )

# Add a normal distribution curve
means <- mean(transformData, na.rm=TRUE)
sds<- sd(transformData, na.rm=TRUE)

curve(dnorm(x, mean=means, sd=sds) * length(transformData) * diff(hist(transformData, plot=FALSE)$breaks)[1], 
      add=TRUE, col="red")

#poriginal population distr is very skewed dist, thus use median 
medians<- median(mergedMurder$Population, na.rm=TRUE)
mergedMurder$Population[is.na(mergedMurder$Population)]<-medians
summary(mergedMurder$Population)

#view(mergedMurder)

#there is only 2 burlglary,4 property creme,  thus omit 
# Using base R to remove rows with NA in a specific column
mergedMurder <- mergedMurder[!is.na(mergedMurder$Burglary), ]
mergedMurder <- mergedMurder[!is.na(mergedMurder$PropertyCrime), ]
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
area$X1<-NULL
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


               #####DEAL with NA
mergedMurder$SQmiles <- as.numeric(mergedMurder$SQmiles.x)
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

                       #do not like it???????????????????ask professor

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

#the lower and upper bounds, change multiplier for changing the bound
lower <- Q1 -6 * IQR
upper <- Q3 + 6 * IQR

cleanedMurder<-mergedMurder[mergedMurder$Population>=lower & 
                              mergedMurder$Population<=upper,]
boxplot(cleanedMurder$Population/100000, horizontal = TRUE, 
        main = "Less Outlier PLOT of population", xlab = "Population",
        ylab = "Population (in 100K)"
        )
#nu,ber of data we lost while cleaning
(lostDataInPop = 6487-6380  ) #107



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

#calculate the outlier, and filter data
# Calculate the first quartile, third quartile 
Q1 <- quantile(mergedMurder$Violent, 0.25)
Q3 <- quantile(mergedMurder$Violent, 0.75)
# interquartile range (IQR)
IQR <- Q3 - Q1

#the lower and upper bounds, change multiplier for changing the bound
lower <- Q1 -6 * IQR
upper <- Q3 + 6 * IQR

cleanedMurder<-mergedMurder[mergedMurder$Violent>=lower & 
                              mergedMurder$Violent<=upper,]
boxplot(cleanedMurder$Violent, horizontal = TRUE, 
        main = "Less Outlier PLOT of Violent", xlab = "Violent",
        ylab = "Violent"
)
(lostDataInV = 6487 - 6289) #198



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

#calculate the outlier, and filter data
# Calculate the first quartile, third quartile 
Q1 <- quantile(mergedMurder$Robbery, 0.25)
Q3 <- quantile(mergedMurder$Robbery, 0.75)
# interquartile range (IQR)
IQR <- Q3 - Q1

#the lower and upper bounds, change multiplier for changing the bound
lower <- Q1 -6 * IQR
upper <- Q3 + 6 * IQR

cleanedMurder<-mergedMurder[mergedMurder$Robbery>=lower & 
                              mergedMurder$Robbery<=upper,]
boxplot(cleanedMurder$Robbery, horizontal = TRUE, 
        main = "Less Outlier PLOT of Robbery", xlab = "Robbery",
        ylab = " Rpbbery"
)




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

#calculate the outlier, and filter data
# Calculate the first quartile, third quartile 
Q1 <- quantile(mergedMurder$PropertyCrime, 0.25)
Q3 <- quantile(mergedMurder$PropertyCrime, 0.75)
# interquartile range (IQR)
IQR <- Q3 - Q1

#the lower and upper bounds, change multiplier for changing the bound
lower <- Q1 -6 * IQR
upper <- Q3 + 6 * IQR

cleanedMurder<-mergedMurder[mergedMurder$PropertyCrime>=lower & 
                              mergedMurder$PropertyCrime<=upper,]
boxplot(cleanedMurder$PropertyCrime, horizontal = TRUE, 
        main = "Less Outlier PLOT of PropertyCrime", xlab = "PropertyCrime",
        ylab = "Property Crime"
)


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

#calculate the outlier, and filter data
# Calculate the first quartile, third quartile 
Q1 <- quantile(mergedMurder$Murder, 0.25)
Q3 <- quantile(mergedMurder$Murder, 0.75)
# interquartile range (IQR)
IQR <- Q3 - Q1

#the lower and upper bounds, change multiplier for changing the bound
lower <- Q1 -6 * IQR
upper <- Q3 + 6 * IQR

cleanedMurder<-mergedMurder[mergedMurder$Murder>=lower & 
                              mergedMurder$Murder<=upper,]
boxplot(cleanedMurder$Murder, horizontal = TRUE, 
        main = "Less Outlier PLOT of Murder", xlab = "Murder",
        ylab = "Murder"
)


#write.csv(mergedMurder,"C:\\Users\\aiger\\OneDrive\\Desktop\\ComputerScience\\CS_DM_541\\ProjectDM\\CSV\\CleanedDataWithOutlier.csv" )

