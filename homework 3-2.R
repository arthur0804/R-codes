### homework 3
### question 2

rm(list=ls(all=TRUE))
setwd('~/Desktop/R')
co <- read.table('co.dat', header = F)
names(co) <- c('Obs', 'x', 'y')
co[1:5,]
summary(co)

n <- length(co$x) 
x_bar <- mean(co$x) 
y_bar <- mean(co$y)

# question 2-a #
lmod1 <- lm(y ~ x, data=co)
summary(lmod1)
plot(co$x,co$y, xlab = "potassium/carbon ratio (K/C)", 
     ylab = "the amount of carbon monoxid e (CO) desorbed" ) 
abline(-0.03804, 1.60313)

#sigma2
s <- summary(lmod1)$sigma  #residual standard error
s
sigma_sq_hat <- s^2   #estimator of sigma2

a <- 1-0.95 
chi_up <- qchisq(1-a/2, n-2) 
chi_low <- qchisq(a/2, n-2)

up_sigma_sq <- s^2 * (n-2)/chi_low #up limit of sigma square 
low_sigma_sq <- s^2 * (n-2)/chi_up #low limit of sigma square

#beita0
beta0_hat <- y_bar #estimator of beta0
beta0_hat
beta0_sd <- s/sqrt(n) #the standard error of beta0
t <- qt(1-a/2, n-2)

ul_beta0 <- beta0_hat + t * beta0_sd #up limit of beta0 
ll_beta0 <- beta0_hat - t * beta0_sd #low limit of beta0

ul_beta0
ll_beta0

#beita1
beta1_hat <- summary(lmod1)$coefficients[2,1]
beta1_sd <- summary(lmod1)$coefficients[2,2]
ul_beta1 <- beta1_hat + t * beta1_sd   #up limit of beta1 
ll_beta1 <- beta1_hat - t * beta1_sd   #low limit of beta1

beta1_hat
ul_beta1
ll_beta1

# question 2-b #
shapiro.test(residuals(lmod1))

# question 2-c #
co$x <- factor(co$x)
summary(co$x)

lmod2 <- lm(y ~ factor(x), co)
summary(lmod2)
anova(lmod1)
anova(lmod1, lmod2)

# question 2-d #

y_prediction <- predict(lmod1, data.frame(x= 2.0), 
    se.fit = T, interval = "prediction", level = 0.90)
y_prediction

y_prediction2 <-predict(lmod1, data.frame(x= 2.0), 
    se.fit = T, interval = "confidence", level = 0.90)
y_prediction2

