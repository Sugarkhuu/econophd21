r_squared <- function(beta_est, X, Y){
  # calculate R squared given beta, X and Y
  Y_hat  <- X%*%beta_est
  EY_hat <- mean(Y_hat) # <=> EY
  EY     <- mean(Y)
  r2     <- sum((Y_hat-EY_hat)^2)/sum((Y-EY)^2)
  
  return(r2)
} 

my_ols <- function(X,Y){
# OLS estimates given X, Y
  
    beta_est <- solve((t(X) %*% X)) %*% (t(X) %*% Y) # estimate the betas
  
  return(beta_est)

}

my_estimator <- function(rho=0.7,n=1000) {
  # return estimated betas and r squared for a random sample
  
  beta <- c(2,1,-1)
  VCOV_mat <- data.frame(c(1, rho),c(rho,1)) # of Xs
  MEAN_mat <- c(2,-3) # c(10,100)            # of Xs
  A = as.matrix(VCOV_mat)                    # just rename
  q = c(dim(A)[2])                           # number of X variables
  
  r_chol <- chol(A)                          # Cholesky decomp. of A
  r_mat <- matrix(rnorm(n*q), ncol=q)        # draws from standard normal dist. of size nxq
  
  X_0 <- r_mat %*% r_chol                    # random sample with demeaned Xs
  X   <-  X_0 + matrix(MEAN_mat, nrow=n, ncol=q, byrow=TRUE) # add respective means to the demeaned Xs
  X   <- cbind(X,intercept=1);                 # add the intercept
  e   <- runif(n,-2,2)                         # random error draws
  
  Y <- X %*% beta + e                        # draws of Y. Now, the observed values. 
  
  beta_est <- my_ols(X,Y) # estimate the betas
  r2 <- r_squared(beta_est, X, Y)            # r squared
  
  return(c(beta_est, r2))
  
}