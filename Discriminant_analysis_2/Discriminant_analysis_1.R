library(MASS)
library(foreign)
mydata2<-read.spss("C:/Users/12345/Desktop/football.sav",to.data.frame=TRUE)
attach(mydata2)
head(mydata2)

model <- lda(group~., data = mydata2)
model



