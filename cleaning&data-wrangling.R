#CLEANING THE DATA
library(tidyverse)
library(dbplyr)
trainData<-read.csv("Adult Train.csv")
View(trainData)
unique(trainData$Age)
unique(trainData$Workclass)
unique(trainData$Education)
unique(trainData$Marital.Status)
unique(trainData$Occupation)
unique(trainData$Relationship)
unique(trainData$Race)
unique(trainData$Sex)
unique(trainData$capital.gain)
unique(trainData$capital.loss)
unique(trainData$hours.per.week)
unique(trainData$native.country)
unique(trainData$Income)
#Removing the rows with blank("?") values
newTrainData <-subset(trainData, Workclass!=" ?"& Occupation!=" ?" & native.country!=" ?")
View(newTrainData)
unique(newTrainData$Workclass)
unique(newTrainData$Occupation)
unique(newTrainData$native.country)

testData<-read.csv("Adult Test.csv")
View(testData)
unique(testData$Age)
unique(testData$Workclass)
unique(testData$Education)
unique(testData$Marital.Status)
unique(testData$Occupation)
unique(testData$Relationship)
unique(testData$Race)
unique(testData$Sex)
unique(testData$capital.gain)
unique(testData$capital.loss)
unique(testData$hours.per.week)
unique(testData$native.country)
unique(testData$Income)
#Removing the rows with blank("?") values
newTestData <-subset(testData, Workclass!=" ?"& Occupation!=" ?" & native.country!=" ?")
View(newTestData)
unique(newTestData$Workclass)
unique(newTestData$Occupation)
unique(newTestData$native.country)


#DATA WRANGLING/MODIFYING THE DATA

#TRAIN DATA
#Change the value of hours.per.week to two categories only (>=40,<40)
(newTrainData<-newTrainData %>% mutate(hours.per.week = case_when(
hours.per.week >= 40 ~ ">=40",
hours.per.week < 40 ~ "<40" )))
#Change the value of capital.gain to three categories only (0,>=12000,<12000)
(newTrainData<-newTrainData %>% mutate(capital.gain = case_when(
capital.gain == 0 ~ "0",
capital.gain != 0 & capital.gain >= 12000 ~ ">=12000",
capital.gain < 12000 ~ "<12000")))
#Change the value of capital.loss to three categories only (0,>=1800,<1800)
(newTrainData<-newTrainData %>% mutate(capital.loss = case_when(
capital.loss == 0 ~ "0",
capital.loss != 0 & capital.loss >= 1800  ~ ">=1800",
capital.loss < 1800 ~ "<1800")))
#Change the value of Age to six categories only (<18,18-29,30-39,40-49,50-59,>=60)
(newTrainData<-newTrainData%>% mutate(Age = case_when(
Age < 18 ~ "<18",
Age >= 18 & Age < 30 ~ "18-29",
Age >= 30 & Age < 40 ~ "30-39",
Age >= 40 & Age < 50 ~ "40-49",
Age >= 50 & Age < 60 ~ "50-59",
Age >= 60 ~ ">=60" )))

#TEST DATA
#Change the value of hours.per.week to two categories only (>=40,<40)
(newTestData<-newTestData %>% mutate(hours.per.week = case_when(
hours.per.week >= 40 ~ ">=40",
hours.per.week < 40 ~ "<40" )))
#Change the value of capital.gain to three categories only (0,>=12000,<12000)
(newTestData<-newTestData %>% mutate(capital.gain = case_when(
capital.gain == 0 ~ "0",
capital.gain != 0 & capital.gain >= 12000 ~ ">=12000",
capital.gain < 12000 ~ "<12000")))
#Change the value of capital.loss to three categories only (0,>=1800,<1800)
(newTestData<-newTestData %>% mutate(capital.loss = case_when(
capital.loss == 0 ~ "0",
capital.loss != 0 & capital.loss >= 1800  ~ ">=1800",
capital.loss < 1800 ~ "<1800")))
#Change the value of Age to six categories only (<18,18-29,30-39,40-49,50-59,>=60)
(newTestData<-newTestData%>% mutate(Age = case_when(
Age < 18 ~ "<18",
Age >= 18 & Age < 30 ~ "18-29",
Age >= 30 & Age < 40 ~ "30-39",
Age >= 40 & Age < 50 ~ "40-49",
Age >= 50 & Age < 60 ~ "50-59",
Age >= 60 ~ ">=60" )))
write.csv(newTestData,"final_testData.csv",row.names = FALSE)
write.csv(newTrainData,"final_trainData.csv",row.names = FALSE)