#letter-recognition.data.txt
data3=read.csv("abalone.data.txt")
row=nrow(data3)
col=ncol(data3)
row
col
View(data3)
#in order to apply knn we have to do normalization 
#and there are 0 values in between

library(dplyr)
library(irr)
library(rpart)
library(caret)
#Tree plotting
#library(rattle)
library(rpart.plot)
library(RColorBrewer)
mod<-rpart(M~.,data=data3[,],control=rpart.control(cp=0.002,maxdepth=7),method="class",parms=list(split="gini"))
View(data3)


plot(mod, margin=0.1, main="Classification Tree for Direct ABALONE")
mod1<-prune(mod,cp= 0.035)
actual<-data3$M
predicted<-predict(mod1,type = "class")

confusionMatrix(predicted,actual,positive="1")
tb1=table(actual,predicted)
a=nrow(data3)
accuracy1=((sum(diag(tb1))/a)*100)
accuracy1
#67.25% accuracy
vecta=c(accuracy,accuracy1)
names(vecta)=c("knn","decision tree")
vecta
barplot(vecta,axes=T,main="Comparison",xlab = "accuracy",ylab = "score",col=c("black","grey"))
abline(h=0)

