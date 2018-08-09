data(meatspec, package="faraway")
head(meatspec)
trainmeat <- meatspec[1:172,]
testmeat <- meatspec[173:215,]
modlm <- lm(fat ~ ., trainmeat)
summary(modlm)

rmse <- function(x,y) sqrt(mean((x-y)^2))
rmse(fitted(modlm), trainmeat$fat)

rmse(predict(modlm,testmeat), testmeat$fat)


#model selection, with default=both
modsteplm <- step(modlm)
rmse(modsteplm$fit, trainmeat$fat)
rmse(predict(modsteplm,testmeat), testmeat$fat)

#PCA
meatpca <- prcomp(trainmeat[,-101])
round(meatpca$sdev,3)

matplot(1:100, meatpca$rot[,1:3], type="l", xlab="Frequency", ylab="", col=1)

#PCR
require(pls)
pcrmod <- pcr(fat ~ ., data=trainmeat, ncomp=50)
rmse(predict(pcrmod, ncomp=4), trainmeat$fat)
