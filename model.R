#CREATING AND TESTING THE MODEL

library(caret)
library(randomForest)

train <- read.csv("final_trainData.csv")
test <- read.csv("final_testData.csv")

#To make sure that rows with blank values are deleted
train <- na.omit(train)
test <-na.omit(test)

#Transform the columns to factor
train <- transform(
train,
Age=as.factor(Age),
Workclass=as.factor(Workclass),
Education=as.factor(Education),
Marital.Status=as.factor(Marital.Status),
Occupation=as.factor(Occupation),
Relationship=as.factor(Relationship),
Race=as.factor(Race),
Sex=as.factor(Sex),
capital.gain=as.factor(capital.gain),
capital.loss=as.factor(capital.loss),
hours.per.week=as.factor(hours.per.week),
native.country=as.factor(native.country),
Income=as.factor(Income) )

test<- transform(
test,
Age=as.factor(Age),
Workclass=as.factor(Workclass),
Education=as.factor(Education),
Marital.Status=as.factor(Marital.Status),
Occupation=as.factor(Occupation),
Relationship=as.factor(Relationship),
Race=as.factor(Race),
Sex=as.factor(Sex),
capital.gain=as.factor(capital.gain),
capital.loss=as.factor(capital.loss),
hours.per.week=as.factor(hours.per.week),
native.country=as.factor(native.country),
Income=as.factor(Income) )

#Equate the levels of the factors of test data to train data
levels(test$Age) <-levels(train$Age)
levels(test$Workclass) <-levels(train$Workclass)
levels(test$Education) <-levels(train$Education)
levels(test$Marital.Status) <-levels(train$Marital.Status)
levels(test$Occupation) <-levels(train$Occupation)
levels(test$Relationship) <-levels(train$Relationship)
levels(test$Race) <-levels(train$Race)
levels(test$Sex) <-levels(train$Sex)
levels(test$capital.gain) <-levels(train$capital.gain)
levels(test$capital.loss) <-levels(train$capital.loss)
levels(test$hours.per.week) <-levels(train$hours.per.week)
levels(test$native.country) <-levels(train$native.country)
levels(test$Income) <-levels(train$Income)

#Create the random forest model
rf<-randomForest(Income~.,data=train,mtry=4,ntree=501)
#Print the model statistics
print(rf)
#Test the training data to the model
p1<-predict(rf,train)
#Print the prediction confusion matrix and statistics
confusionMatrix(p1,train$Income)
#Test the testing data to the model
p2<-predict(rf,test)
#Print the prediction confusion matrix and statistics
confusionMatrix(p2,test$Income)
#Plot the error rate of the model
plot(rf)