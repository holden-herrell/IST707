library(readr)
library(tm)
library(stringr)
library(wordcloud)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(caret)
library(e1071)
library(reshape)
library(FSelector)

#Set Working Directory
setwd("C:\\Users\\herre\\OneDrive\\Desktop\\IST707")

#Import Deception Data
RawDeception<-read.delim("deception_data_converted_final.csv")

#Create Working Copy
Deception<-RawDeception

#Break up Reviews
Deception$lie <- substring(Deception$lie.sentiment.review,1,1)
Deception$sentiment <- substring(Deception$lie.sentiment.review,3,3)
Deception$lie.sentiment.review <- as.character(Deception$lie.sentiment.review)
Deception$review<- substring(Deception$lie.sentiment.review,5,nchar(Deception$lie.sentiment.review))

#Remove Agg Fields
Deception <- Deception[,-1]
Deception$lie <- as.factor(Deception$lie)
Deception$sentiment <- as.factor(Deception$sentiment)

#Check Structure
str(Deception)

#Create Review Corpus
ReviewCorpus <- Corpus(VectorSource(Deception$review))

#Remove Rare Words
MinTermFreq <- length(ReviewCorpus) *.025

#Ignore Frequent Words
MaxTermFreq <- length(ReviewCorpus) * .9

#Create Review Document Matrix
ReviewDocMatrix <- DocumentTermMatrix(ReviewCorpus, control = list( stopwords = TRUE, tolower= TRUE, removeNumbers = TRUE,
                  removePunctuation=TRUE, remove_separators=TRUE, stemming = TRUE, wordLengths = c(3,Inf),
                  bounds = list(global = c(MinTermFreq, MaxTermFreq))))

#Review most common words
WordFreq <- colSums(as.matrix(ReviewDocMatrix))
ord <- order(-WordFreq)
(WordFreq[head(ord)])

#Remove Custom Stopwords
MyStopwords <- c("went","order","restaur", "food")
OriginalCol <- data.frame(colnames(ReviewDocMatrix))
RemoveColumn <- which(apply(OriginalCol,1, function(x) any(x %in% MyStopwords)))
ReviewDocMatrix <- (ReviewDocMatrix[,-RemoveColumn])
UpdatedCol <- (colnames(ReviewDocMatrix))

#Review after removing common words
WordFreq <- colSums(as.matrix(ReviewDocMatrix))
ord <- order(-WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])

#Convert to Matrix
ReviewMatrix <- as.matrix(ReviewDocMatrix)

#Remove zero word rows and rownames
ReviewMatrix <- ReviewMatrix[!c(rowSums(ReviewMatrix)==0),]
rownames(ReviewMatrix) <- NULL

#Check negative/positive sentiment
table(Deception$lie, Deception$sentiment)

#Create Word Cloud
wordcloud(colnames(ReviewMatrix),colSums(ReviewMatrix), random.order = FALSE, colors = brewer.pal(8,"Dark2"))

#Normalize
NormRevMatrix <- sweep(ReviewMatrix,2,colSums(ReviewMatrix),'/')
table(colSums(NormRevMatrix))

#Create DF with lie and sentiment
Review <- data.frame(ReviewMatrix)
Review<- merge(Deception[,1:2], Review, by="row.names", all.x=FALSE)
Review$Row.names<- as.numeric(rownames(Review))

#Chi squared
##Sentiment
SentReviewShrink <- melt(Review[,-1:-2])
SentReviewShrink<- cast(SentReviewShrink, sentiment ~ variable, sum)
SentGainSave <- gain.ratio(sentiment~.,ReviewShrink)
head(SentGainSave, n=20)

SentReviewChi2 <- chisq.test(SentReviewShrink)
SentReviewChi2$p.value
###1.639963e-20
####Low p value-->Not independant

##Lies
LieReviewShrink <- melt(Review[,c(-1,-3)])
LieReviewShrink<- cast(LieReviewShrink, lie ~ variable, sum)
LieGainSave <- gain.ratio(lie~.,LieReviewShrink)
LieGainSave<-head(LieGainSave, n=20)

LieReviewChi2<- chisq.test(LieReviewShrink)
LieReviewChi2$p.value
###0.0007533615
####Low p value-->Not independant

###Naive Bayes###
#Create Naive Bayes for Lie
TrainingSize <- round(nrow(Review)*.75/2,0)

#Create even mix of T/F
SampleRows <- as.numeric(c(sample(Review[c(Review$lie=="f"),1], TrainingSize,replace = FALSE),sample(Review[c(Review$lie=="t"),1], TrainingSize,replace = FALSE)))

#Create train/test sets, exclude sentiment
LieTrain <- Review[SampleRows,c(-1,-3)]
LieTest <- Review[-SampleRows,c(-1,-3)]
table(LieTrain$lie)

#Create NB model
NBLie<- naiveBayes(LieTrain$lie~., data=LieTrain)

#Predict Train Data
predictNBLieTrain <- predict(NBLie, LieTrain[,-1],type="class")
NBLieTrainConfMatrix <- table(predictNBLieTrain,LieTrain$lie)
NBLieTrainConfMatrix
LieTrainAcc <- sum(diag(NBLieTrainConfMatrix))/nrow(LieTrain)
LieTrainAcc
LieTrainPrecision<- diag(NBLieTrainConfMatrix)/rowSums(NBLieTrainConfMatrix)
LieTrainPrecision
LieTrainRecall <- diag(NBLieTrainConfMatrix)/colSums(NBLieTrainConfMatrix)
LieTrainRecall

#Predict Test Data
predictNBLieTest <- predict(NBLie, LieTest[,-1],type="class")
NBLieTestConfMatrix <- table(predictNBLieTest,LieTest$lie)
NBLieTestConfMatrix
LieTestAcc <- sum(diag(NBLieTestConfMatrix))/nrow(LieTest)
LieTestAcc
LieTestPrecision<- diag(NBLieTestConfMatrix)/rowSums(NBLieTestConfMatrix)
LieTestPrecision
LieTestRecall <- diag(NBLieTestConfMatrix)/colSums(NBLieTestConfMatrix)
LieTestRecall

#Create Naive Bayes for Sentiment
SentSampleRows <- as.numeric(c(sample(Review[c(Review$sentiment=="n"),1], TrainingSize,replace = FALSE),sample(Review[c(Review$sentiment=="p"),1], TrainingSize,replace = FALSE)))

#Create train/test sets, exclude lie
SentTrain <- Review[SentSampleRows,c(-1:-2)]
SentTest <- Review[-SentSampleRows,c(-1:-2)]
table(SentTrain$sentiment)

#Create NB model
NBSent<- naiveBayes(SentTrain$sentiment~., data=SentTrain)

#Predict Train Data
predictNBSentTrain  <- predict(NBSent, SentTrain[,-1],type="class")
NBSentTrainConfMatrix <- table(predictNBSentTrain,SentTrain$sentiment)
NBSentTrainConfMatrix
SentTrainAcc <- sum(diag(NBSentTrainConfMatrix))/nrow(SentTrain)
SentTrainAcc
SentTrainPrecision <- diag(NBSentTrainConfMatrix)/rowSums(NBSentTrainConfMatrix)
SentTrainPrecision
SentTrainRecall <- diag(NBSentTrainConfMatrix)/colSums(NBSentTrainConfMatrix)
SentTrainRecall

#Predict Test Data
predictNBSentTest  <- predict(NBSent, SentTest[,-1],type="class")
NBSentTestConfMatrix <- table(predictNBSentTest,SentTest$sentiment)
NBSentTestConfMatrix
SentTestAcc <- sum(diag(NBSentTestConfMatrix))/nrow(SentTest)
SentTestAcc
SentTestPrecision <- diag(NBSentTestConfMatrix)/rowSums(NBSentTestConfMatrix)
SentTestPrecision
SentTestRecall <- diag(NBSentTestConfMatrix)/colSums(NBSentTestConfMatrix)
SentTestRecall

###SVM###
#Create SVM model for Lies
SVMLie <- e1071::svm(LieTrain$lie~., data = LieTrain, kernel="linear", cost=100, scale=FALSE)

#Predict Train Data
predictSVMLieTrain <- predict(SVMLie, LieTrain[,-1],type="class")
SVMLieTrainConfMatrix <- table(predictSVMLieTrain,LieTrain$lie)
SVMLieTrainAcc <- sum(diag(SVMLieTrainConfMatrix))/nrow(LieTrain)
SVMLieTrainAcc
SVMLieTrainPrecision <- diag(SVMLieTrainConfMatrix)/rowSums(SVMLieTrainConfMatrix)
SVMLieTrainPrecision
SVMLieTrainRecall <- diag(SVMLieTrainConfMatrix)/colSums(SVMLieTrainConfMatrix)
SVMLieTrainRecall

#Predict Test Data
predictSVMLieTest <- predict(SVMLie, LieTest[,-1],type="class")
SVMLieTestConfMatrix <- table(predictSVMLieTest,LieTest$lie)
SVMLieTestAcc <- sum(diag(SVMLieTestConfMatrix))/nrow(LieTest)
SVMLieTestAcc
SVMLieTestPrecision <- diag(SVMLieTestConfMatrix)/rowSums(SVMLieTestConfMatrix)
SVMLieTestPrecision
SVMLieTestRecall <- diag(SVMLieTestConfMatrix)/colSums(SVMLieTestConfMatrix)
SVMLieTestRecall

#Create SVM model for Sentiment
SVMSent <- e1071::svm(SentTrain$sentiment~., data = SentTrain, kernel="linear", cost=100, scale=FALSE)

#Predict the Train Data
predictSVMSentTrain<- predict(SVMSent, SentTrain[,-1],type="class")
SVMSentTrainConfMatrix <- table(predictSVMSentTrain,SentTrain$sentiment)
SVMSentTrainAcc <- sum(diag(SVMSentTrainConfMatrix))/nrow(SentTrain)
SVMSentTrainAcc
SVMSentTrainPrecision <- diag(SVMSentTrainConfMatrix)/rowSums(SVMSentTrainConfMatrix)
SVMSentTrainPrecision
SVMSentTrainRecall <- diag(SVMSentTrainConfMatrix)/colSums(SVMSentTrainConfMatrix)
SVMSentTrainRecall

#Predict the Test Data
predictSVMSentTest<- predict(SVMSent, SentTest[,-1],type="class")
SVMSentTestConfMatrix <- table(predictSVMSentTest,SentTest$sentiment)
SVMSentTestAcc <- sum(diag(SVMSentTestConfMatrix))/nrow(SentTest)
SVMSentTestAcc
SVMSentTestPrecision <- diag(SVMSentTestConfMatrix)/rowSums(SVMSentTestConfMatrix)
SVMSentTestPrecision
SVMSentTestRecall <- diag(SVMSentTestConfMatrix)/colSums(SVMSentTestConfMatrix)
SVMSentTestRecall
