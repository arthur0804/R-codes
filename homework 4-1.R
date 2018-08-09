# hw4
# Faraway 2e, Page 49, exercise 4
library(faraway)
data(sat)
head(sat)
summary(sat)

lmod1=lm(total~expend+ratio+salary, data=sat)
summary(lmod1)

lmod2=lm(total~expend+ratio+salary+takers, data=sat)
summary(lmod2)

anova(lmod1,lmod2)

set.seed(123)
nb <- 500
coefmat <- matrix(NA,nb,4)
resids <- residuals(lmod1)
preds <- fitted(lmod1)
for(i in 1:nb){
  booty <- preds + sample(resids , rep=TRUE)
  bmod <- update(lmod1 , booty ~ .)
  coefmat[i,] <- coef(bmod)
}
coefmat <- data.frame(coefmat)
colnames(coefmat) <- c("Intercept",colnames(sat[,1:3]))
apply(coefmat,2,function(x) quantile(x,c(0.025,0.975)))




