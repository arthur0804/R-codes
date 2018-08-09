library(faraway) 
library(ellipse) 
library(ggplot2)
Y = c(62, 60, 63, 59, 63, 67, 71, 64, 65, 66, 68, 66, 71, 67, 68, 68, 56, 62, 60, 61, 63, 64, 63, 59) 
X = matrix(c(rep(c(1, 0, 0, 0), 4), rep(c(0, 1, 0, 0), 6), rep(c(0, 0, 1, 0), 6), rep(c(0, 0, 0, 1), 8)), nrow = 24, ncol = 4, byrow = T)
r = 6.42857
theta_H0 = c(65.5, 65.5, 65.5, 61) 
Residuals = c(1, -1, 2, -2, -3, 1, 5, -2, -1, 0, 0, -2, 3, -1, 0, 0, -5, 1, -1, 0, 2, 3, 2, -2)

bootstrap = function(bootstrapdata){ 
  M = 500 
count = 0 
  for( i in 1:M)
  { e_star = sample(bootstrapdata, size = 24, replace = T)      #sampling 
    y_star = X %*% theta_H0 + e_star                            #plug-in
    theta_star = solve(t(X)%*%X)%*%t(X)%*%y_star                #calculate theta star
    H0 = c(rep(65.5, 16), rep(61, 8)) 
    H1 = c(rep(theta_star[1], 4), rep(theta_star[2], 6), rep(theta_star[3], 6), rep(theta_star[4], 8)) 
    SSE_0 = sum((y_star - H0)^2) 
    SSE_1 = sum((y_star - H1)^2) 
    r_m_star = ((SSE_0 - SSE_1)/2)/((SSE_1)/20) 
    if(r_m_star > r){count = count+1}
} 
return(count/M)
} 
bootstrap(Residuals)

Residuals_2 <- c(Y-X%*%theta_H0)
Residuals_2
bootstrap(Residuals_2)





SSE1 = (24-2)*5.5 
SSE0 = 184 
delta = sqrt((SSE0-SSE1)/5.5) 
phi = delta/sqrt(1+2)
pearsonhartley = function(phi,alpha,df1,df2)
  { b = 0.5*phi*phi*(1+df1) 
  q = qf(1-alpha,df1,df2) 
  y = 1-1/(1+df1*q/df2) 
  p = pbeta(y,df1/2,df2/2) 
  p1= 1 
  jmax = 1000 
  for(j in 1:jmax){ 
    p1 = p1*b/j 
    p = p + pbeta(y, j+df1/2, df2/2)*p1 } 
  1 - p*exp(-b)

}
pearsonhartley(phi, 0.05, 2, 22)

Y_new = Y - 4*X[,2] - 8*X[,3]
X_new = matrix(c(X[,1]+X[,2]+X[,3], X[,4]), nrow = 24, ncol = 2)
theta_new = solve(t(X_new)%*%X_new)%*%t(X_new)%*%Y_new
Residuals_3 = Y_new - X_new%*%theta_new
Residuals_3
bootstrap_2 = function(bootstrapdata){ 
  M = 500 
  count = 0 
  for( i in 1:M)
  { e_star = sample(bootstrapdata, size = 24, replace = T) 
  y_star_new = X_new %*% theta_new + e_star 
  theta_star = solve(t(X_new)%*%X_new)%*%t(X_new)%*%y_star_new 
  y_star = y_star_new + 4*X[,2] + 8*X[,3]
  H0 = c(rep(65.5, 16), rep(61, 8)) 
  H1 = c(rep(theta_star[1], 16), rep(theta_star[2], 8))
  SSE_0 = sum((y_star - H0)^2) 
  SSE_1 = sum((y_star_new - H1)^2)
  r_m_star = ((SSE_0 - SSE_1)/2)/((SSE_1)/22) 
  if(r_m_star > qf(0.05, 2, 22, lower.tail = F)){count = count+1}
  } 
  return(count/M)
} 
bootstrap_2(Residuals_3)

