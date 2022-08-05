library(MASS)
library(foreign)
football<-read.spss("C:/Users/12345/Desktop/football.sav",to.data.frame=TRUE)
attach(football)
head(football)

result<-manova(cbind(WDIM,CIRCUM,FBEYE,EYEHD,EARHD,JAW)~group , data=football)
football.summry<-summary(result)
football.summry$SS
E<-football.summry$SS$Residuals

(e<-E/87)
(err<-diag(e))


model <- lda(group~., data = football)
a1<-model$scaling[,1]
a1

a.star1<-t(sqrt(err)) * t(a1)
a.star1



# step-wise discriminant analysis
library(klaR)
greedy.wilks(group~., data = football, niveau = 0.1) 


###### A Function to perform step-wise discriminant analysis using F statistic
install.packages("pcaMethods")
install.packages("BiocManager")
BiocManager::install("pcaMethods")
library(multiDimBio)



dat.use<-matrix(nrow = 90,ncol = 6 ,dimnames = list(c(),c("WDIM", "CIRCUM","FBEYE", "EYEHD","EARHD", "JAW")))
dat.use[,1]<-football[,2]
dat.use[,2]<-football[,3]
dat.use[,3]<-football[,4]
dat.use[,4]<-football[,5]
dat.use[,5]<-football[,6]
dat.use[,6]<-football[,7]

GR.use<-football[,1]
FSelect(dat.use, GR.use,3)


##############################   Appel     ##############################
Appel<-read.table("C:/Users/12345/Desktop/Apple.txt",header = T)
head(Appel)


#discriminant analysis
library(MASS)
modelAppel <- lda(group~., data = Appel)
modelAppel




#discriminant analysis 
result<-manova(cbind(y1,y2,y3,y4)~group , data=Appel)
Appel.summry<-summary(result)
Appel.summry$SS
E<-Appel.summry$SS$Residuals

(e<-E/87)
(err<-diag(e))


modelAppel <- lda(group~., data = Appel)
a1<-modelAppel$scaling[,1]
a1

a.star1<-t(sqrt(err)) * t(a1)
a.star1


plot(modelAppel)


fitPredict=predict(modelAppel)
LDscores=fitPredict$x
LD1=LDscores[,1]
plot(LD1,xlab="first linear discriminant",col=factor(group))
legend("topleft", legend=levels(factor(group)), 
       text.col=seq_along(levels(factor(group))))

LD2=LDscores[,2]
plot(LD2,xlab="second linear discriminant",col=factor(group))
legend("topleft", legend=levels(factor(group)), 
       text.col=seq_along(levels(factor(group))))

# step-wise discriminant analysis
library(klaR)
greedy.wilks(group~., data = Appel, niveau = 0.1) 

