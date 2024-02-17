#read file from csv
library(tidyverse)
library(vroom)
library(readr)
                            #save all samples in one list
folder_path <- "C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV"
file_paths <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

allSamples <- list()

for(file_path in file_paths) {
  table_name <- tools::file_path_sans_ext(basename(file_path))
  allSamples[[table_name]] <- read.csv(file_path)
}
#delete 2014 data, keep data 2014edited
#allSamples$crimeCalifornia2014<-NULL

                             #convert char to numerical values
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

                                  #Create new Year attribute for each
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

#write.csv(allSamples$crimeCalifornia2006,"C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2006.csv")
#allSamples$crimeCalifornia2006<-read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2006.csv")

#allSamples$crimeCalifornia2016<-read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2016.csv")
view(allSamples$crimeCalifornia2006)

combinedAllSamples <- bind_rows(allSamples, .id = "Dataset")
view(combinedAllSamples)

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

write.csv(combinedAllSamples,"C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/MergedCleanedData.csv" )











# 
# #**********************************************************************************************************
# # Install and load vroom if you haven't already
# file.exists("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2019.csv")
# 
# #city	population	violent	murder	rape	robbery	aggravatedassault	propertycrime	burglary	larceny-theft	motorvehicletheft	arson
# 
# sampleOf2019<-read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2019.csv")
# #make numeric 
# 
# #download
# sampleOf2019<-read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2019.csv")
# #make numeric 
# for(i in 2:12){
#   sampleOf2019[[i]]<-as.numeric(gsub(",", "", sampleOf2019[[i]]))
# }
# 
# sampleOf2018<-read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2018.csv")
# for(i in 2:12) { # Change 2 to 11 for your actual case
#   sampleOf2018[[i]] <- as.numeric(gsub(",", "", sampleOf2018[[i]]))
# }
# view(sampleOf2018)
# 
# sampleOf2014<-read.csv("C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2014.csv")
# sampleOf2014$Rape..revised.definition.1<-as.numeric(gsub(",","",sampleOf2014$Rape..revised.definition.1))
# as.numeric(sampleOf2014$Rape..legacy.definition.2)
# #substituteNA
# sampleOf2014$Rape..revised.definition.1[is.na(sampleOf2014$Rape..revised.definition.1)]<-0
# sampleOf2014$Rape..legacy.definition.2[is.na(sampleOf2014$Rape..legacy.definition.2)]<-0
# 
# #combine two columns
# sampleOf2014$Rape..revised.definition.1<-sampleOf2014$Rape..revised.definition.1+sampleOf2014$Rape..legacy.definition.2
# sampleOf2014$Rape..legacy.definition.2<-NULL
# write.csv(sampleOf2014,"C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV/crimeCalifornia2014Edited.csv")
# #directory               
# #C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV
# 
# 
# 
# 
#                 ### how to load files  All together
# library("dplyr")                                                 
# library("plyr")                                                
# library("readr") 
# murderData<-list.files(path = "C:/Users/aiger/OneDrive/Desktop/ComputerScience/CS_DM_541/ProjectDM/CSV",  
#                       pattern = "*.csv", full.names = TRUE) %>%  lapply(read_csv) %>%bind_rows  
