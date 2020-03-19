library(tidyverse)
library(magrittr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Cairo)
library(forcats)
library(dplyr)
library(stringr)
library(e1071)
library(mlr)
library(naivebayes)
library(caret)
library(stringr)
library(randomForest)

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

#Split train data into train/test
TrainTestSplit<-.75
TrainSize <- round(nrow(TrainDigit)*TrainTestSplit,0)
TrainRows <- sample(seq_len(nrow(TrainDigit)), size = TrainSize, replace = FALSE)
TrainDigit_1 <- TrainDigit[TrainRows,]
TestDigit_1 <- TrainDigit[-TrainRows,]
TrDigit_1NoHead <- data.frame(TrainDigit_1[,-1])
TestDigit_1NoHead <- data.frame(TestDigit_1[,-1])

#Create SVM Model
SVMmodel <- e1071::svm(TrainDigit_1$label ~., data = TrainDigit_1[-1], kernel = "polynomial", cost = 10, scale = FALSE)

##Predict Training Data
predictSVMTrain <- predict(SVMmodel, TrainDigit_1[-1], type = "class")
SVMTrainConfMatrix <- table(predictSVMTrain, TrainDigit_1$label)
SVMTrainConfMatrixAcc <- sum(diag(SVMTrainConfMatrix))/nrow(TrainDigit_1)
SVMTrainConfMatrix
SVMTrainConfMatrixAcc
###1.00

##Predicting Test Data
predictSVMTest <- predict(SVMmodel, TestDigit_1[-1], type = "class")
SVMTestConfMatrix <- table(predictSVMTest, TestDigit_1$label)
SVMTestConfMatrixAcc <- sum(diag(SVMTestConfMatrix))/nrow(TestDigit_1)
SVMTestConfMatrix
SVMTestConfMatrixAcc
###0.9747619

#Create kNN Model
Ksize <- round(sqrt(nrow(TrainDigit_1)))
Ksize
###177

##Predict Train Data
TrainkNN <- class::knn(train=TrainDigit_1[-1], test =TrainDigit_1[-1], cl=TrainDigit_1$label, k = Ksize, prob=FALSE)
print(TrainkNN )
kNNTrainConfMatrix <- (table(TrainkNN, TrainDigit_1$label))
kNNTrainConfMatrixAcc <- sum(diag(kNNTrainConfMatrix))/nrow(TrainDigit_1)
kNNTrainConfMatrix
kNNTrainConfMatrixAcc

##Predict Test Data
TestkNN <- class::knn(train=TrainDigit_1[-1], test =TestDigit_1[-1], cl=TrainDigit_1$label, k = Ksize, prob=FALSE)
print(TestkNN)
kNNTestConfMatrix <- (table(TestkNN, TestDigit_1$label))
kNNTestConfMatrixAcc <- sum(diag(kNNTestConfMatrix))/nrow(TestDigit_1)
kNNTestConfMatrix
kNNTestConfMatrixAcc

#Create Random Forest Model
##Can't handle so much data
##Reduce data
ReduceSize<-.5
TrainReduceSize <- round(nrow(TrainDigit_1)*ReduceSize,0)
TrainReduceRows <- sample(seq_len(nrow(TrainDigit_1)), size = TrainReduceSize, replace = FALSE)
TrainDigit_2 <- TrainDigit_1[TrainReduceRows,]

TrainTestSplit<-.75
TrainSize1 <- round(nrow(TrainDigit_2)*TrainTestSplit,0)
TrainRows1 <- sample(seq_len(nrow(TrainDigit_2)), size = TrainSize1, replace = FALSE)
TrainDigit_3 <- TrainDigit_2[TrainRows1,]
TestDigit_2 <- TrainDigit_2[-TrainRows1,]

#Now calculate model
RFmodel <- randomForest(TrainDigit_3$label ~., data = TrainDigit_3, importance=TRUE, proximity = TRUE, ntree=10)
RFTrainConfMatrix<-RFmodel$confusion
RFTrainConfMatrixAcc <- sum(diag(RFmodel$confusion))/nrow(TrainDigit_3)
RFTrainConfMatrix
RFTrainConfMatrixAcc

##Predict Test Data
TestRF <- predict(RFmodel, TestDigit_2[-1], type = "class")
RFTestConfMatrix  <- table(TestRF,TestDigit_2$label)
RFTestConfMatrixAcc <- sum(diag(RFTestConfMatrix))/nrow(TestDigit_2)
RFTestConfMatrix 
RFTestConfMatrixAcc




