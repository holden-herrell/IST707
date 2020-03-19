#Import needed packages
install.packages("tm")
install.packages("stringr")
install.packages("wordcloud")
install.packages("slam")
install.packages("quanteda")
install.packages('proxy')
install.packages("tidytext")
install.packages("factoextra")
install.packages("mclust")
install.packages("useful")
install.packages("corpus")
library(useful)
library(tm)
library(stringr)
library(wordcloud)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext) 
library(plyr) 
library(ggplot2)
library(factoextra) 
library(mclust)
library(corpus)
library(rpart)
library(rpart.plot)
library(rattle)

#Set Working Directory
setwd("C:\\Users\\herre\\OneDrive\\Desktop\\IST707")

#Import Fed Papers Corpus, confirm all 85 are present
FedPapersCorpus<-Corpus(DirSource("FedPapers"))
(getTransformations())
(ndocs<-length(FedPapersCorpus))

summary(FedPapersCorpus)

#Define Special Stop Words
(SpecialStops<-c("Alexander","alexander","Hamilton","hamilton","Publius","publius",
                 "James","james","Madison","madison", "Jay","jay","John","john", stopwords("english")))


#Create Document Term Matrix
FedPapersDTM<-DocumentTermMatrix(FedPapersCorpus,
               control = list(stopwords=SpecialStops,
               wordLengths=c(4,10), 
               removePunctuation=TRUE,
               removeNumbers=TRUE,
               tolower=TRUE,
               remove_separators=TRUE))
(class(FedPapersDTM))



#Remove .txt from doc names
row.names(FedPapersDTM)<-gsub(pattern="*\\.txt$",replacement="",x=row.names(FedPapersDTM))

#Remove _fed from doc names
row.names(FedPapersDTM)<-gsub(pattern="*_fed",replacement="",x=row.names(FedPapersDTM))

#Create Matrix
FedPapersMatrix<-as.matrix(FedPapersDTM)

#Normalize Document Term Matrix
FPN1<-apply(FedPapersMatrix,1,function(i)round(i/sum(i),3))
FedPapersNorm<-t(FPN1)

#Check before and after normalization
(FedPapersMatrix[1:11,1:10])
(FedPapersNorm[1:11,1:20])

#Create Word Cloud of disputed papers
wordcloud(colnames(FedPapersMatrix),
          FedPapersMatrix[1:11,],
          max.words=30,scale=c(3,.5),
          fixed.asp=TRUE,r.layout=FALSE, rot.per=.1)

#Convert to dataframe
FedPapersDF<-as.data.frame(FedPapersNorm)

#Create column for author
FedPapersDF<-FedPapersDF%>%tibble::rownames_to_column()
names(FedPapersDF)[1]='Author'
(FedPapersDF[1:10,1:10])

#Split Paper number from Author
FedPapers<-tidyr::separate(data=FedPapersDF, col=Author, into=c("Author","PaperNo"))
FedPapers[1:85,1:4]

#Remove Paper number for decision trees
FP<-FedPapers[,]
FP$PaperNo<-NULL
FP[1:85,1:4]

#Remove Disputed papers
FP1<-FP[12:ncol(FP),]
FP2<-FP[1:11,]
FP1[1:74,1:4]

#Make train and test data with 2/3-1/3 split
trainSplit<-.67
set.seed(11)
TTsample<-sample.int(n = nrow(FP1), size = floor(trainSplit*nrow(FP1)), replace=FALSE)
FedPapersTrain<-FP1[TTsample,]
FedPapersTest<-FP1[-TTsample,]
#Check train test split
length(TTsample)/nrow(FP1)

#Make Decision Trees
##Create basic train tree
FedTrainTree<-rpart(Author ~., data=FedPapersTrain, method="class", control=rpart.control(cp=0))
summary(FedTrainTree)
printcp(FedTrainTree)

##Predict test using train tree
Pred<-predict(FedTrainTree, FedPapersTest, type="class")

#Create confusion matrix
table(FedPapersTest$Author,Pred)

#Plot Train Tree
fancyRpartPlot(FedTrainTree)+title("Unbound Authorship Decision Tree",line=-1)

#Use Train Tree to predict disputed authorship
DispPred<-predict(FedTrainTree, FP2, type="class")
DispPred

#Show what paper numbers got what author
table(FedPapers[1:11,]$PaperNo,FP2$Author,DispPred)



##Create another tree with min splits and max depth
FedTrainTree1<-rpart(Author ~., data=FedPapersTrain, method="class", control=rpart.control(cp=.07, minsplit=2,maxdepth=4))
summary(FedTrainTree1)
plotcp(FedTrainTree1)

##Predict test using train tree
Pred1<-predict(FedTrainTree1, FedPapersTest, type="class")

#Create confusion matrix
table(FedPapersTest$Author,Pred1)

#Plot Train Tree
fancyRpartPlot(FedTrainTree1)+title("Parameterized-Pruned Authorship Decision Tree",line=2.75)

#Use Train Tree to predict disputed authorship
DispPred1<-predict(FedTrainTree1, FP2, type="class")
DispPred1

#Show what paper numbers got what author
table(FedPapers[1:11,]$PaperNo,FP2$Author,DispPred1)

##Create pruned tree
FedTrainTree2<-rpart(Author ~., data=FedPapersTrain, method="class", control=rpart.control(cp=.03))
summary(FedTrainTree2)
printcp(FedTrainTree2)

##Predict test using train tree
Pred2<-predict(FedTrainTree2, FedPapersTest, type="class")

#Create confusion matrix
table(FedPapersTest$Author,Pred2)

#Plot Train Tree
fancyRpartPlot(FedTrainTree2)+title("Pruned Authorship Decision Tree",line=-1)

#Use Train Tree to predict disputed authorship
DispPred2<-predict(FedTrainTree1, FP2, type="class")
DispPred2

#Show what paper numbers got what author
table(FedPapers[1:11,]$PaperNo,FP2$Author,DispPred2)

