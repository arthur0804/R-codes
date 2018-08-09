rm(list=ls(all=TRUE))
setwd("/Users/arthur_0804/Desktop/R")
source("/Users/arthur_0804/Desktop/R/dnsim.R")
library(faraway)
data(sat)
attach(sat)
lm=lm (total ~ expend + ratio + salary + takers)
summary(lm)

p<-5 ## #of coefficients 
n<-length(total) 
sigma<-summary(lm)$sigma 
inf<- lm.influence(lm)
par(mfrow=c(3,4))


## 2. residual plot 
std <- lm$resid / summary(lm)$sigma #standardize
plot(lm$fitted.value,std);abline(h=c(-2,2),lty="dotted");title("Standardized residual plot") 
indent = (max(lm$fitted.value)-min(lm$fitted.value))/20 
#why divided by 20? 
#the "indent" seems to adjust the position of the note
for(i in 1:n){ 
  if(abs(std[i])> 2)
    text(lm$fitted.value[i]+indent,std[i],i,cex=0.6)} 
#cex is font size

indent = n/25 ## indent for labels

## 3. internally standadized residual plot 
stanres<-function(fit,lms=summary(fit),lmi=lm.influence(fit)) 
{
  
  h<-lmi$hat #hat matrix
  e<-residuals(fit) 
  s<-lms$sigma
  si<-lmi$sigma 
  #note that these to sigmas are different
  #but si seems not used here
  e/(s*(1-h)^.5) } #according to the formula (4.3)
Internally.Studentized.Residual<-stanres(lm) 
plot(Internally.Studentized.Residual);title("Internally Standardized Residual") 
abline(h=c(-2,2),lty="dotted") 
for(i in 1:n){ if(abs(Internally.Studentized.Residual[i])> 2) 
  text(i+indent,Internally.Studentized.Residual[i],i,cex=0.6)}

## 4. externally studentized residual plot

studres<-function(fit,lmi=lm.influence(fit))
{ 
  h<-lmi$hat 
  e<-residuals(fit) 
  si<-lmi$sigma
  e/(si*(1-h)^.5)
} 
Externally.Studentized.Residual<-studres(lm) 
plot(Externally.Studentized.Residual);title("Externally Studentized Residual")
abline(h=c(-2,2),lty="dotted") 
for(i in 1:n){ if(abs(Externally.Studentized.Residual[i])> 2) 
  text(i+indent,Externally.Studentized.Residual[i],i,cex=0.6)}

## 5. qq plot 
qqnorm(Externally.Studentized.Residual);abline(c(0,0),c(1,1))

## 6. leverage- hat matrix 
plot(inf$hat);title("leverage plot") 
abline(h=2*p/n,lty=3) ### high leverage points 
leverage<-c(inf$hat>2*p/n) ## obs 18 is leverage 
for(i in 1:n){ if(leverage[i]==T)text(i+indent,inf$hat[i],i,cex=0.6)}

## 7. DFFITS 
dffits<-function(fit,lmi=lm.influence(fit)){
  h<-lmi$hat
  e<-residuals(fit)
  si<-lmi$sigma
  h^0.5*e/(si*(1-h)) } 
DFFITS <- dffits(lm) 
plot(DFFITS);abline(h=2*sqrt(p/n),lty=2);abline(h=-2*sqrt(p/n),lty=2);title("DFFITS") 
DF.detected <- c(abs(DFFITS)> 2*sqrt(p/n)) 
for(i in 1:n)
{ if(DF.detected[i]==T)text(i+indent,DFFITS[i],i,cex=0.6)}

## 8. DFBETAS 
sxxi<- diag(summary(lm)$cov.unscaled) 
si <- inf$sigma 
bi<- coef(inf) 
DFBETAS<-bi/(si %o% sxxi^0.5)

plot(DFBETAS[,1],pch=1,ylab='dfbetas',ylim=c(min(DFBETAS),max(DFBETAS))) 
abline(h=2*sqrt(1/n),lty=2);abline(h=-2*sqrt(1/n),lty=2);title("DFBETAS") 
if (p>1) {
  for (k in 1:p) { 
    points(DFBETAS[,k],pch=k) 
    DFB.detected<- c(abs(DFBETAS[,k])> 2*sqrt(1/n)) 
    for(i in 1:n){ if(DFB.detected[i]==T)text(i+indent,DFBETAS[i,k],i,cex=0.6)} }} 
legend("bottomleft", c("beta0","beta1"), pch = c(1:p))

## 9. Cook’s D 
plot(lm, which=4)

## 10. COVRATIO 
first <- ((n-p-1) / (n-p))+ Externally.Studentized.Residual^2/(n-p) 
COVRATIO <- first^(-p) /(1-inf$hat) 
plot(COVRATIO);abline(h=3*p/n+1,lty=2);abline(h=-3*p/n+1,lty=2);title("COVRATIO") 
CR.detected <- c(abs(COVRATIO-1)>3*p/n)
for(i in 1:n){ 
  if(CR.detected[i]==T)
    text(i+indent,COVRATIO[i],i,cex=0.6)}

########################### - Simulation Envelope plot - ############# 
source("dnsim.R") 
dnsim(total, expend + ratio + salary + takers, 1000)