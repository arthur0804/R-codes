require(faraway)
library(faraway)
data(coagulation)
coagulation


coagulation<-coagulation[-c(17,18,19,20,21,22,23,24),]
lm<-lm(coag~ diet,coagulation)
sumary(lm)
set.seed(123)
nb<-500
count<-0
resids<-residuals(lm)
lm2<-lm(coag~1,coagulation)
summary(lm2)
preds<-fitted(lm)
sse0<-sum(residuals(lm2)^2)
sse1<-sum(residuals(lm)^2)
f1<-(sse0-sse1)/2*13/sse1
###p value
1-pf(f1,2,13)
for (i in 1: nb){
  booty<-preds+sample(resids, rep=TRUE)
  bmod<-update(lm, booty ~.)
  cmod<-update(lm2,booty~.)
  sse1<-sum(residuals(bmod)^2)
  sse0<-sum(residuals(cmod)^2)
  if (f1< (sse0-sse1)/2*13/sse1)
    count<-count+1
}
summary(bmod)

f1
count/nb


