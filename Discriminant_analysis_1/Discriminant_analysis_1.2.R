
##############################   Appel     ##############################
Appel<-read.table("C:/Users/12345/Desktop/Apple.txt",header = T)
head(Appel)
group.A<-Appel$group

  
#discriminant analysis
  library(MASS)
modelAppel <- lda(group.A~., data = Appel)
modelAppel




#discriminant analysis 
result<-manova(cbind(y1,y2,y3,y4)~group.A , data=Appel)
Appel.summry<-summary(result)
Appel.summry$SS
E<-Appel.summry$SS$Residuals

(e<-E/87)
(err<-diag(e))


modelAppel <- lda(group.A~., data = Appel)
a1<-modelAppel$scaling[,1]
a1

a.star1<-t(sqrt(err)) * t(a1)
a.star1


plot(modelAppel)

fitPredict=predict(modelAppel)
LDscores=fitPredict$x
LD1=LDscores[,1]
plot(LD1,xlab="first linear discriminant",col=factor(group.A))
legend("topleft", legend=levels(factor(group.A)), 
       text.col=seq_along(levels(factor(group.A))))



# step-wise discriminant analysis
library(klaR)
greedy.wilks(group~., data = Appel, niveau = 0.1) 

