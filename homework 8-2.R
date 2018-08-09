library(faraway) 
data(seatpos) 
dim(seatpos)
head(seatpos)
attach(seatpos)
seatpos[1:3,3:9]
model1PCA <- prcomp(seatpos[,3:8])
summary(model1PCA)

par(mfrow=c(1,2)) 
plot(model1PCA)
plot(model1PCA, type="l")


matplot(1:6, model1PCA$rotation[,1:3],type="l",xlab="Variables",ylab="")
model1 <- lm(hipcenter~model1PCA$x[,1])
model1A <- lm(hipcenter~HtShoes+Ht+Seated+Arm+Thigh+Leg)
anova(model1, model1A) 
par(mfrow=c(2,4)) 
plot(model1) 
plot(model1A) 
summary(model1) 
summary(model1A)

#add age and weight
model2PCA <- prcomp(seatpos[,1:8])
summary(model2PCA)
par(mfrow=c(1,2)) 
plot(model2PCA) 
plot(model2PCA, type="l") 

model2 <- lm(hipcenter~model2PCA$x[,1:2]) 
model2A <- lm(hipcenter~Age+Weight+HtShoes+Ht+Seated+Arm+Thigh+Leg)
summary(model2) 
summary(model2A)

x1 <- data.frame(HtShoes=181.08,Ht=178.56,Seated=91.44,Arm=35.64,Thigh=40.95,Leg=38.79) 
x2 <- data.frame(Age=64.8,Weight=263.7,HtShoes=181.08,Ht=178.56,Seated=91.44, Arm=35.64,Thigh=40.95,Leg=38.79)


A <- as.matrix(summary(model1)$coef[,1])
B <- as.matrix(cbind(1,predict(model1PCA,new=x1)[1]))
B %*% A


C <- as.matrix(summary(model2)$coef[,1])
D <- rbind(1,as.matrix(predict(model2PCA,new=x2)[1:2]))
t(D) %*% C
