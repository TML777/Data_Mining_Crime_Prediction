#read file from csv
library(tidyverse)
library(vroom)
library(readr)
                            #save all samples in one list

folder_path <- "C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV"
file_paths <- list.files(folder_path, pattern = "//.csv$", full.names = TRUE)

#create empty list and call allSamples
allSamples <- list()

#combine all cvs files into list
for(file_path in file_paths) {
  table_name <- tools::file_path_sans_ext(basename(file_path))
  allSamples[[table_name]] <- read.csv(file_path)
}

#delete 2014 data, keep data 2014edited
#allSamples$crimeCalifornia2014<-NULL

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

#write.csv(allSamples$crimeCalifornia2006,"C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2006.csv")
#allSamples$crimeCalifornia2006<-read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2006.csv")

#allSamples$crimeCalifornia2016<-read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2016.csv")
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
#write.csv(combinedAllSamples,"C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/MergedCleanedData.csv" )



                          #deal with NA
summary(mergedMurder)
                      #population
hist(mergedMurder$Population,breaks = 30, main = "Histogram with 30 bars", 
     xlab = "Value", col = "lightblue" )

#Add a normal distribution curve
means <- mean(mergedMurder$Population, na.rm=TRUE)
sds<- sd(mergedMurder$Population, na.rm=TRUE)

curve(dnorm(x, mean=means, sd=sds) * length(mergedMurder$Population) * diff(hist(mergedMurder$Population, plot=FALSE)$breaks)[1], 
      add=TRUE, col="red")
         #skewed try to normalize for brtter visualization
         #log transformation will help with right skew to more normalize
transformData<-log(mergedMurder$Population+1)

hist(transformData,breaks = 30, main = "Histogram with 30 bars", 
     xlab = "Value", col = "lightblue" )

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
hist(mergedMurder$Arson)
means <- mean(mergedMurder$Arson, na.rm=TRUE)
sds<- sd(mergedMurder$Arson, na.rm=TRUE)

curve(dnorm(x, mean=means, sd=sds) * length(mergedMurder$Arson) * diff(hist(mergedMurder$Arson, plot=FALSE)$breaks)[1], 
      add=TRUE, col="red")

#very skewed try to normilize for better representation
transformData<-log(mergedMurder$PropertyCrime)

hist(transformData,breaks = 30, main = "Histogram with 30 bars", 
     xlab = "Value", col = "lightblue" )

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

#add the area 

#download "C:/Users/aiger/Downloads/AreaTable.csv"

area<-read.csv("C:/Users/aiger/Downloads/AreaTable.csv")

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







