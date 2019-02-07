#letter-recognition.data.txt
data3=read.csv("abalone.data.txt")
row=nrow(data3)
col=ncol(data3)
row
col
View(data3)
#in order to apply knn we have to do normalization 
#and there are 0 values in between
fn1=function(x){(x-min(x))/(max(x)-min(x))}
a=append(a,0.5)
fn1(a)
normdata1=as.data.frame(lapply(data3[,-1],fn1))
View(normdata1)
#yes data is in good form after normalization
index=sample(row,0.8*row)
traindata1=normdata1[index,]
testdata1=normdata1[-index,]
pred_model=knn(traindata1,testdata1,data3[index,1],k=3)
tb=table(pred_model,data3[-index,1])
a=0.2*row
accuracy=sum(diag(tb)/a*100)
accuracy
tb
