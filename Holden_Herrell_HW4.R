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

#Set Working Directory
setwd("C:\\Users\\herre\\OneDrive\\Desktop\\IST707")

#Import Fed Papers Corpus, confirm all 85 are present
FedPapersCorpus<-Corpus(DirSource("FedPapers"))
(getTransformations())
(ndocs<-length(FedPapersCorpus))

summary(FedPapersCorpus)

#Create Document Term Matrix
FedPapersDTM<-DocumentTermMatrix(FedPapersCorpus,
               control = list(stopwords=TRUE, 
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
(FedPapersNorm[1:11,1:10])


#Convert to dataframe
FedPapersDF<-as.data.frame(FedPapersNorm)

#Create Word Cloud of disputed papers
wordcloud(colnames(FedPapersMatrix),
          FedPapersMatrix[1:11,],
          max.words=30,scale=c(3,.5),
          fixed.asp=TRUE,r.layout=FALSE, rot.per=.1)

#Create Distance Measures

##Euclidean distance
EDistFedPapersNorm<-dist(FedPapersNorm,method="euclidean")

##Cosine similarity
CSDistFedPapersNorm<-dist(FedPapersNorm,method="cosine")

#Hierarchical Clustering

##Euclidean distance
EGroups <- hclust(EDistFedPapersNorm,method="ward.D")
plot(EGroups, cex=0.9, hang=-.5,main="Euclidean Distance Cluster Dendrogram")
rect.hclust(EGroups, k=4)

##Cosine Similarity 
CSGroups <- hclust(CSDistFedPapersNorm,method="ward.D")
plot(CSGroups, cex=0.9, hang=-.5, main="Cosine Similarity Cluster Dendrogram")
rect.hclust(CSGroups, k=5)

#Kmeans Clustering
kmeansFIT1<-kmeans(FedPapersNorm,3)
(kmeansFIT1$cluster)
kmeansTable<-table(rownames(FedPapersNorm),kmeansFIT1$cluster)
kmeansTable
plot(kmeansFIT1,FedPapersNorm, title="K-Means Result", xlab="", ylab="")

