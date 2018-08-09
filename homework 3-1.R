### homework 3
### question 1

# question 1-a #
library(faraway)
data(prostate)
attach(prostate)

lmod1 <- lm(lpsa~lcavol+lweight+svi+age+lbph+lcp+pgg45+gleason)


summary(lmod1)

confint(lmod1, "age", level = 0.95)

confint(lmod1, "age", level = 0.90)

# question 1-b #
### install.package("ellipse")
require(ellipse)

### plot the ellipse
plot(ellipse(lmod1,c(5,6),level=0.95),type="l")

### plot the least estimate point
points(coef(lmod1)[5], coef(lmod1)[6], pch=19)
### or you can write in this way, and pch represents the symbool
### points(lmod1$coeff[5],lmod1$coeff[6], pch=19)

### plot the origin point
points(0,0,pch=10)

# question 1-c #
lms <- summary(lmod1)
summary(lmod1)$coef[5,]
nreps <- 4000
tstats <- numeric(nreps)
set.seed(123)
for(i in 1:nreps){
  lmod2 <- lm(lpsa~lcavol+lweight+svi+sample(age)+lbph+lcp+pgg45+gleason)
  tstats[i] <- summary(lmod2)$coef[5,3]
}

mean(abs(tstats) > abs(lms$coef[5,3]))



# question 1-d #
summary(lmod1)

lmod2 <- lm(lpsa~lcavol+lweight+svi)

summary(lmod2)

anova(lmod2,lmod1)

