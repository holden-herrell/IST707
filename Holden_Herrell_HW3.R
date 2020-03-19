install.packages('arules')
install.packages('arulesViz')
library(arules)
library(arulesViz)

#Load bank data from csv
bankData<-read.csv(file="/Users/herre/Downloads/bankdata_csv_all.csv", na.string = c(""))

#Check the structure of bankData
str(bankData)

#Remove id from data
bank<-bankData[,c(2:12)]
View(bank)

#Discretize Age, Income, and Children for association rule mining
bank$age<-cut(as.numeric(bank$age), breaks = c(17,25,40,60,99), 
              labels=c( "YoungAdult", "Adult", "MiddleAged", "Elderly"))
bank$income<-cut(bank$income, breaks = c(0,20000,40000,60000,100000,999999999), 
              labels=c("Very Poor","Poor","Average","Wealthy","Very Wealthy"))
bank$children<-cut(as.numeric(bank$children), breaks = c(-1,0,1,2,3),
              labels =c("None","One","Two","Three"))

#Run association rule mining on bank data
bank_rules<-apriori(bank,parameter=list(supp=0.25, conf=.7, minlen=2, maxlen=10, maxtime=5))

#Summarize bank rules
summary(bank_rules)

#Sort rules
bank_rules<-sort(bank_rules, by="lift", decreasing=TRUE)
options(digits=3)
inspect(bank_rules)

#Plot most interesting bank_rules
plot(bank_rules, method="graph")

#Run PEP specific association rule mining on bank data
PEP_rules<-peprules <- apriori(bank, parameter = list(maxlen = 4), appearance = list(rhs = c("pep=YES", "pep=NO")))

#Summarize PEP_rules
summary(PEP_rules)

#Sort PEP_rules
PEP_rules<-sort(PEP_rules, by="lift",decreasing=TRUE)
options(digits=3)
inspect(PEP_rules)

#Subset interesting rules
intPEP<-PEP_rules[1:9]
inspect(intPEP)
intPEP1<-intPEP[-7]
inspect(intPEP1)
intPEP2<-intPEP1[-2]
inspect(intPEP2)
intPEPFinal<-intPEP2[-3:-4]
inspect(intPEPFinal)

#Plot most interesting PEP_rules
plot(intPEPFinal, method="graph", main="Top 5 PEP Based Rules")