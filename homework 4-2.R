rm(list=ls(all=TRUE))
setwd('~/Desktop/R')
SWtest <- function(Y, X, nrep = 1000){
  
  n <- length(Y) 
  p <- ncol(X) 
  #---fit linear regresson and get the residual to calculate test statistics 
  fit <- lm(Y ~ X) 
  r <- residuals(fit)
  
  #order the residual 
  r_sort <- sort(r, decreasing = FALSE)
  
  #calculate the z by qnorm 
  z <- qnorm((1:n - 0.375) / (n + 0.25))
  
  #test_statistics W is:
  
  W <- sum(r_sort * z) / sqrt(sum(r_sort ^ 2) * sum(z ^ 2))
  
  #---use monto carlo to get sampling 
  W_N <- as.numeric(nrep) 
  for(i in 1:nrep){
  y <- rnorm(n) 
  r_i <- residuals(lm(y ~X)) 
  r_i <- sort(r_i, decreasing = FALSE) 
  W_N[i] <- sum(r_i * z) / sqrt(sum(r_i ^ 2) * sum(z ^ 2))
}
#calculate the p_value based on sampling quantile 
  p_value <- sum(W_N < W) / nrep

  return(list(test_statistics = W, p_value = p_value))

} 
coal_data <- read.table(file = "co.dat", header = TRUE)
SWtest(Y = coal_data$y, X = coal_data$x, nrep = 10000)

