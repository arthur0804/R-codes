library(faraway)
data(truck)

truck$B <- sapply(truck$B, function(x) ifelse(x== "-", -1, 1))
truck$C <- sapply(truck$C, function(x) ifelse(x== "-", -1, 1))
truck$D <- sapply(truck$D, function(x) ifelse(x== "-", -1, 1))
truck$E <- sapply(truck$E, function(x) ifelse(x== "-", -1, 1))
truck$O <- sapply(truck$O, function(x) ifelse(x== "-", -1, 1))

summary(truck)


### question a
truck_fit = lm(height ~ B + C + D + E + O, data=truck)
truck_fit$coefficients


### question b
truck_fit_new = lm(height ~ B + C + D + E, data=truck)
truck_fit_new$coefficients
anova(truck_fit, truck_fit_new)


### question c
truck1 <- cbind (truck, "A"=truck$B+truck$C+truck$D+truck$E)
lm.truck1 <- lm(height ~ A + B + C + D + E + O, data=truck1)
lm.truck1$coefficients
summary(lm.truck1)


### question d

x <- model.matrix(~ A + B + C + D + E + O, data=truck)
y <- truck$height
xtxi <- solve(t(x) %*% x) 


### question e
truck1 <- cbind (truck, "A"=truck$B+truck$C+truck$D+truck$E)
lm.truck1 <- lm(height ~ A + B + C + D + E + O, data=truck1)
x <- model.matrix(~ A + B + C + D + E + O, data=truck)
y <- truck$height
qrx <- qr(x)
dim(qr.Q(qrx))
(f <- t(qr.Q(qrx)) %*% y)
backsolve(qr.R(qrx),f)


### question f
qr.coef(qrx,y)

