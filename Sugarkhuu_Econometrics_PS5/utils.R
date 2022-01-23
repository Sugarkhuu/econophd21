conf_interval <- function(beta_true=2, beta_est=2,s_beta=1, n_size=1,alpha=0.05,ifInt=TRUE) {
#' Calculate actual coverage probability for exponential distribution
#' theta - mean value of the distribution
#' it    - number of different draws

  t_crit <- qt(1-alpha/2,n-3) # 97.5th percentile of t dist with n-3 df
  
  if(ifInt){
    conf_int <- beta_est+c(-1,1)*t_crit*s_beta/sqrt(n_size)
    # check if within the interval: Boolean
    ifInside <- rbind(conf_int[1] <= beta_true & beta_true <= conf_int[2])
  } else {
    conf_int <- beta_true+c(-1,1)*t_crit*s_beta/sqrt(n_size)
    # check if within the interval: Boolean
    ifInside <- rbind(conf_int[1] <= beta_est & beta_est <= conf_int[2])
  }
  conf_int_len <- conf_int[2]-conf_int[1]
  
  t_stat <- abs((beta_est - beta_true)/(s_beta/sqrt(n_size)))
  p_value <- 2*(1-pt(t_stat,n-3))

  return(c(ifInside,conf_int_len,conf_int,p_value))
}

vcov_beta <- function(beta_est, X, Y){
  # calculate R squared given beta, X and Y
  n <- length(Y)
  k <- length(beta_est)
  Y_hat     <- X%*%beta_est
  e_hat     <- Y_hat - Y 
  var_ehat  <- var(e_hat)
  v_beta <- n/(n-k)*var_ehat[1]*solve((t(X) %*% X))
  
  return(v_beta)
} 

vcov_beta_het <- function(beta_est, X, Y){
  # calculate R squared given beta, X and Y
  n <- length(Y)
  k <- length(beta_est)
  Y_hat     <- X%*%beta_est
  e_hat     <- Y_hat - Y 
  D_hat     <- diag(diag(e_hat %*% t(e_hat)))
  v_beta_het <- n/(n-k)*solve(t(X) %*% X) %*% (t(X) %*% D_hat %*% X) %*% solve(t(X) %*% X)
  
  return(v_beta_het)
} 

sigma_error <- function(beta_est, X, Y){
  # calculate R squared given beta, X and Y
  Y_hat  <- X%*%beta_est
  e_hat  <- Y_hat - Y 
  sigma_ehat <- sqrt(mean(e_hat^2))
 
  return(sigma_ehat)
} 

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
  v_b <- vcov_beta(beta_est, X, Y)            # r squared
  s_beta <- sqrt(v_b[1,1])
  ifInside <- conf_interval(beta_true=2, beta_est=beta_est[1],s_beta=s_beta, n_size=n)  
  ifHypo2True <- conf_interval(beta_true=2, beta_est=beta_est[1],s_beta=s_beta, n_size=n,alpha=0.01,ifInt=FALSE)   
  ifHypo18True <- conf_interval(beta_true=1.8, beta_est=beta_est[1],s_beta=s_beta, n_size=n,alpha=0.01,ifInt=FALSE)   
  
  return(c(beta_est, r2, ifInside[1],ifInside[2],ifHypo2True[1],ifHypo18True[1]))
  
}