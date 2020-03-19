install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

#Import csv
storyTeller<-read.csv(file="/Users/herre/Downloads/data-storyteller.csv", na.string = c(""))

#Copy raw data
schools<-storyTeller

#Convert schools to factor
schools$School<-as.factor(schools$School)

#Rename columns
colnames(schools)<- c(
  "School"
  , "Section"
  , "VeryAhead"
  , "Middling"
  , "Behind"
  , "MoreBehind"
  , "VeryBehind"
  , "Completed"
)

#Reorder columns
col_order<-c(
  "School"
  , "Section"
  , "VeryBehind"
  , "MoreBehind"
  , "Behind"
  , "Middling"
  , "VeryAhead"
  , "Completed"
)

schools <- schools[,col_order]

#Remove VeryAhead for no data
schools$VeryAhead<- NULL

#Calculate the number of students per section
StudentsPerSection<- c()
                   for(i in 1:nrow(schools)){
                 StudentsPerSection[i]=sum(schools[i, 3:7])}
min(StudentsPerSection)
max(StudentsPerSection)

#Add StudentsPerSection
schools<-cbind(schools,StudentsPerSection)

#Reorder columns
col_order1<-c(
  "School"
  , "Section"
  , "StudentsPerSection"
  , "VeryBehind"
  , "MoreBehind"
  , "Behind"
  , "Middling"
  , "Completed"
)

schools <- schools[,col_order1]

#Calculate ratios of status to StudentsPerSection
VeryBehindRatio<-round(100*schools$VeryBehind/schools$StudentsPerSection,2)
MoreBehindRatio<-round(100*schools$MoreBehind/schools$StudentsPerSection,2)
BehindRatio<-round(100*schools$Behind/schools$StudentsPerSection,2)
MiddlingRatio<-round(100*schools$Middling/schools$StudentsPerSection,2)
CompletedRatio<-round(100*schools$Completed/schools$StudentsPerSection,2)

#Add ratios
schools <- cbind(schools, VeryBehindRatio, MoreBehindRatio, BehindRatio, MiddlingRatio, CompletedRatio)

#Calculate concern range ratio (VeryBehind, MoreBehind, Behind)
ConcernRatio<-rowSums(cbind(schools$VeryBehindRatio,schools$MoreBehindRatio,schools$BehindRatio))

#Add ConcernRatio
schools <- cbind(schools, ConcernRatio)

#Aggregate to school level
schoolsAgg<-aggregate(. ~School, data=schools, mean, na.rm=TRUE)

#Stats summary of agg school data
summary(schoolsAgg)

#Boxplot
plotNames<-c("Students per Section","Concern Ratio")
ConcernBox<-boxplot(schoolsAgg$StudentsPerSection,schoolsAgg$ConcernRatio, horizontal=TRUE, names=plotNames,
        main="StudentsPerSection and ConcernRatio")
ConcernBox

#Bar graph
barConcernBySchool<-ggplot(data=schoolsAgg, aes(x=School, y=ConcernRatio, fill=ConcernRatio))+
                  geom_bar(colour="black", stat="identity")+
                  ggtitle("Average Concern Ratio by School")+
                  xlab("School")+
                  ylab("Average Concern Ratio (in Percent)")
barConcernBySchool

#Scatter Plot
scatterConcernBySchool<-ggplot(data=schools, aes(x=CompletedRatio, y=ConcernRatio))+
                  geom_point(aes(size=StudentsPerSection, color=School))+
                  geom_smooth(method=lm,se=FALSE,fullrange=TRUE)+
                  ggtitle("Concern vs Completion Ratio by School and Students per Section")+
                  xlab("Section Completed Ratio (in Percent)")+
                  ylab("Section Concern Ratio (in Percent)")
scatterConcernBySchool