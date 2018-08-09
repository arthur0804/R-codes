library(faraway)
data(prostate)
attach(prostate)

fit1 = lm(lpsa ~ lcavol, data=prostate)
fit2 = lm(lcavol ~ lpsa, data=prostate)
plot(lcavol, lpsa)
abline(fit1$coeff[1], fit1$coeff[2])
abline(-fit2$coeff[1]/ fit2$coeff[2], 1/ fit2$coeff[2])

fit1 = lm(lcavol ~ lpsa, data=prostate)
fit2 = lm(lpsa ~ lcavol, data=prostate)
plot(lpsa,lcavol)
abline(fit1$coeff[1], fit1$coeff[2])
abline(-fit2$coeff[1]/ fit2$coeff[2], 1/ fit2$coeff[2])

mean(lpsa)
mean(lcavol)
