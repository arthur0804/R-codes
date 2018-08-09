library(faraway)
data(fat) 
dim(fat)
fat[1:3,] 
attach(fat)
x <- c(seq(10,250,by=10))
testx <- fat[x,-c(1:3)] 
dim(testx) 
testx[1:3,]
trainx <- fat[-x,-c(1,3)] 
dim(trainx) 
trainx[1:3,]
modelA <- lm(siri~., data= trainx)

sumary(modelA)

rmse <- function(x,y) { sqrt(mean((x-y)^2)) } 
rmse(modelA$fit,trainx)

rmse(predict(modelA,testx),fat$siri[x])

modelB <- step(modelA)

sumary(modelB)

rmse(modelB$fit,trainx)

rmse(predict(modelB,testx),fat$siri[x])

modelCPCA <- prcomp(fat[,4:18])

summary(modelCPCA)

library(pls) 
modelC <- pcr(siri~.,data=trainx,ncomp=10) 
rmse(predict(modelC,ncomp=2),trainx$siri) 
rmse(predict(modelC,testx,ncomp=2),fat$siri[x])

#partial least squares
modelD <- plsr(siri~., data= trainx,ncomp=10, validation="CV") 
modelDCV <- RMSEP(modelD,estimate="CV") 
plot(modelDCV) 
rmse(predict(modelD,ncomp=4,trainx),trainx) 
rmse(predict(modelD,ncomp=4,testx),fat$siri[x])

#ridge regression
library(MASS) 
y<- trainx$siri-mean(trainx$siri) 
modelE <- lm.ridge(y~.,data=trainx[,-1],lambda=seq(0,10,0.01)) 
matplot(modelE$lambda,t(modelE$coef),type="l", xlab=expression(lambda),ylab=expression(hat(beta))) 
select(modelE) 
which.min(modelE$GCV)
abline(v=0.05,lty=2,col=4) 
ypredTrain <- cbind(1,as.matrix(trainx[,-1])) %*% coef(modelE)[6,] 
rmse(ypredTrain,trainx$siri) 
ypredTest <- cbind(1,as.matrix(testx[,])) %*% coef(modelE)[6,] 
rmse(ypredTest,fat$siri[x])