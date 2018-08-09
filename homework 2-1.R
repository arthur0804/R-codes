library(faraway)
data(prostate)
head(prostate) # see what the data looks like

s = c(1:8)
R2 = c(1:8)

fit1 <- lm(lpsa~lcavol, data=prostate)

names(fit1)
names(summary(fit1))

s[1] = summary(fit1)$sigma
R2[1] = summary(fit1)$r.squared

fit2 = lm(lpsa~lcavol+lweight, data=prostate)
s[2] = summary(fit2)$sigma
R2[2] = summary(fit2)$r.squared

fit3 = lm(lpsa~lcavol+lweight+svi, data=prostate)
s[3] = summary(fit3)$sigma
R2[3] = summary(fit3)$r.squared

fit4 = lm(lpsa~lcavol+lweight+svi+lbph, data=prostate)
s[4] = summary(fit4)$sigma
R2[4] = summary(fit4)$r.squared

fit5 = lm(lpsa~lcavol+lweight+svi+lbph+age, data=prostate)
s[5] = summary(fit5)$sigma
R2[5] = summary(fit5)$r.squared

fit6 = lm(lpsa~lcavol+lweight+svi+lbph+age+lcp, data=prostate)
s[6] = summary(fit6)$sigma
R2[6] = summary(fit6)$r.squared

fit7 = lm(lpsa~lcavol+lweight+svi+lbph+age+lcp+pgg45, data=prostate)
s[7] = summary(fit7)$sigma
R2[7] = summary(fit7)$r.squared

fit8 = lm(lpsa~lcavol+lweight+svi+lbph+age+lcp+pgg45+gleason, data=prostate)
s[8] = summary(fit8)$sigma
R2[8] = summary(fit8)$r.squared

plot(s)
title("Residual standard error")

plot(R2)
title("R squared")

