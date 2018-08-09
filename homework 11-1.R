library(faraway)
data(fat,package="faraway")
attach(fat)

plot(neck ~ knee)
plot(chest ~ thigh)
plot(hip ~ wrist)

head(fat)
cfat <- fat[,9:18]
prfat <- prcomp(cfat)

dim(prfat$rot)
dim(prfat$x)

summary(prfat)
round(prfat$rot[,1],2)

prfatc <- prcomp(cfat, scale=TRUE)
summary(prfatc)
round(prfatc$rot[,1],2)

lmoda <- lm(fat$brozek ~ ., data=cfat)
sumary(lmoda)

lmodpcr <- lm(fat$brozek ~ prfatc$x[,1:2])
sumary(lmodpcr)

lmodr <- lm(fat$brozek ~ scale(abdom) + I(scale(ankle)-scale(abdom)), data=cfat)
sumary(lmodr)
detach(fat)
