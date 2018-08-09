y = c(62, 60, 63, 59, 63, 67, 
      71, 64, 65, 66, 68, 66, 
      71, 67, 68, 68, 56, 62, 
      60, 61, 63, 64, 63, 59)
x1 = c(1,1,1,1,0,0,
       0,0,0,0,0,0,
       0,0,0,0,0,0,
       0,0,0,0,0,0)
x2 = c(0,0,0,0,1,1,
       1,1,1,1,0,0,
       0,0,0,0,0,0,
       0,0,0,0,0,0)
x3 = c(0,0,0,0,0,0,
       0,0,0,0,1,1,
       1,1,1,1,0,0,
       0,0,0,0,0,0)
x4 = c(0,0,0,0,0,0,
       0,0,0,0,0,0,
       0,0,0,0,1,1,
       1,1,1,1,1,1)

lmod1 = lm(y ~  x1 + x2 + x3 + x4 -1)
sumary(lmod1)

lmod2 = lm(y ~ I(x1+x2+x3) + x4 -1)
sumary(lmod2)
anova(lmod2,lmod1)

#under H0, residuals are as follows:
H0_Residuals <- c(1, -1, 2, -2,
                  -3, 1, 5, -2, -1, 0,
                  0, -2, 3, -1, 0, 0,
                  -5, 1, -1, 0, 2, 3, 2,-2)
set.seed(123)
nb <- 500
coefmat <- matrix(NA, nb, 4)
resids <- H0_Residuals
preds <- fitted(lmod2)
for(i in 1:nb){
  y <- preds + sample(resids, rep=TRUE)
  bmod <- update(lmod2 , y ~ I( x1 + x2 + x3) + x4 -1)
  coefmat[i,] <- coef(bmod)
}
sumary(bmod)
deviance(bmod)
anova(bmod,lmod1)

