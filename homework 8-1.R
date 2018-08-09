library(faraway)
data(divusa)
attach(divusa)
lmod <- lm(divorce ~ unemployed + femlab + marriage + birth + military)
summary(lmod)

#a condition index
x <- model.matrix(lmod)[,-1]
e <- eigen(t(x) %*% x)
e$values
sqrt(e$val[1]/e$val)

#b VIF
require(faraway)
vif(x)

#c remove some variables
lmod <- lm(divorce ~ femlab + marriage)
x <- model.matrix(lmod)[,-1]
e <- eigen(t(x) %*% x)
e$values
sqrt(e$val[1]/e$val)
vif(x)