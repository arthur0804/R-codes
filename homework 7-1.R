rm(list=ls(all=TRUE)) 
library(leaps) 
library(faraway) 
data(prostate)

step(lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,data=prostate)
     ,direction = "backward")

step(lm(lpsa~1,data=prostate)
     ,direction = "forward"
     , scope = ~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)

step(lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,data=prostate)
     ,direction = "both")

b<-regsubsets(lpsa~.,data=prostate) 
summary(b)
