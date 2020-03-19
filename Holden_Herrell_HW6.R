library(readr)
library(e1071)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Cairo)

#Set Working Directory
setwd("C:\\Users\\herre\\OneDrive\\Desktop\\IST707")

#Import Digit Data
RawTrainDigit<-read.csv("Kaggle-digit-train.csv")
RawTestDigit<-read.csv("Kaggle-digit-test.csv")

#Create DataFrames to manipulate
TrainDigit<-RawTrainDigit
TestDigit<-RawTestDigit

#Inspect the Data
nrow(TrainDigit[!complete.cases(TrainDigit),])
nrow(TestDigit[!complete.cases(TestDigit),])
table(TrainDigit$label)

#Convert Data
TrainDigit <- data.frame(sapply(TrainDigit, as.numeric))
TestDigit <- data.frame(sapply(TestDigit, as.numeric))
TrainDigit$label<-as.factor(TrainDigit$label)
TrDigitNoHead <- data.frame(TrainDigit[,-1])
str(data.frame(TestDigit[,]))

#Create visualization of a digit image
DigImage<-matrix((TrainDigit[1,2:ncol(TrainDigit)]), nrow=28, ncol=28)
DigImage<- apply(DigImage, 2, as.numeric)
image(1:28,1:28, DigImage, col= gray((255:0)/255))

#Create Naive Bayes Model
NBTrDigit <- naiveBayes(TrainDigit$label~., data=TrainDigit, na.action = na.pass)

##Predict Train Data
NBPredictTrain <- predict(NBTrDigit, TrDigitNoHead)

##Create Confusion Matrix
NBConfusionTrain <- table(TrainDigit$label,NBPredictTrain)
NBConfusionTrain

##Check the prediction accuracy %
sum(diag(NBConfusionTrain))/nrow(TrainDigit)
###.5298333

##Predict Test Data
NBPredictTest <- predict(NBTrDigit, TestDigit)
table(NBPredictTest)

#Create Decision Tree
DTTrain <- rpart (TrainDigit$label~., data=TrainDigit, method="class")

##Visualize Decision Tree
fancyRpartPlot(DTTrain)

##Prune
DTTrain <- prune(DTTrain, cp=.01)
summary(DTTrain)

##Predict Train Data
PredictDTTrain <- predict(DTTrain, TrDigitNoHead, type = "class")

##Confusion matrix
DTConfusionTrain <- table(PredictDTTrain, TrainDigit$label)
DTConfusionTrain

##Check the prediction accuracy %
sum(diag(DTConfusionTrain))/nrow(TrainDigit)
###0.6371667

##Predict Test Data
PredictDTTest <- predict(DTTrain, TestDigit, type = "class")
table(PredictDTTest)


