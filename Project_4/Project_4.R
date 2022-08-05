
bors<-read.csv("~/Processed_DJI.csv")
bors<-bors[,-1]
attach(bors)
bors$MOVEMENT<-as.factor(bors$MOVEMENT)
dim(bors)
sum(bors$MOVEMENT==1)
sum(bors$MOVEMENT==0)
###

library(MASS)
bors.lda= lda(MOVEMENT ~ .,data=bors )
pred = predict(bors.lda,bors)
table.lda<-table(bors$MOVEMENT,pred$class,dnn = c('Actual Group','Predicted Group'))

Accuracy.lda<-sum(diag(table.lda))/sum(table.lda)
error.rate.lda<-1-Accuracy.lda
Accuracy.lda



####
train.df <- bors[1:847,] 
valid.df <- bors[848:1059,] 
library(MASS)
bors.lda= lda(train.df$MOVEMENT ~ .,data=train.df )
pred = predict(bors.lda,valid.df)
table.lda<-table(valid.df$MOVEMENT,pred$class,dnn = c('Actual Group','Predicted Group'))
table.lda
Accuracy.lda<-sum(diag(table.lda))/sum(table.lda)
Accuracy.lda
error.rate.lda<-1-Accuracy.lda
error.rate.lda

(Accuracy.1<-86/(86+23))
(Accuracy.0<-50/(53+50))





###### logi
library(nnet)
newfish.logd<-multinom(MOVEMENT~.,data=bors,maxit=250)
t<-table(bors$MOVEMENT,predict(newfish.logd,bors))
sum(diag(t))/dim(bors)[1]

