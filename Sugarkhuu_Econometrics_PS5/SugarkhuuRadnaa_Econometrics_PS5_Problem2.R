## libraries
library("readxl")
library(ggplot2)
library(reshape2)

# import
source("utils.R")

# data
df <- read_excel("WDB.xlsx")

# regression variables
X <- as.matrix(cbind(df[c("water","rural","health_gdp")],intercept=1))
Y <- as.matrix(df["mrate"])
n <- length(Y)

# results

## a
beta_est <- my_ols(X,Y)                    # beta OLS


## b

v_b <- vcov_beta(beta_est, X, Y)           # homosk. vcov matrix of betas
v_b_het <- vcov_beta_het(beta_est, X, Y)   # heterosk. vcov matrix of betas

# summary matrix
rows <- c("beta_1","beta_2","beta_3","beta_4")
cols <- c('est','std',"null","lower","upper",'p_value')
mat_hom <- matrix(nrow = length(rows), ncol = length(cols))
rownames(mat_hom) <- rows
colnames(mat_hom) <- cols
mat_het <- mat_hom

for(j in 1:length(beta_est)){
  beta_test <- 0
  beta_est_i <- beta_est[j]
  s_beta_hom <- sqrt(v_b[j,j])
  s_beta_het <- sqrt(v_b_het[j,j])

  # confidence interval around 0
  result_hom <- conf_interval(beta_true=beta_test, beta_est=beta_est_i,s_beta=s_beta_hom, n_size=n,alpha=0.05) # ,ifInt=FALSE  
  result_het <- conf_interval(beta_true=beta_test, beta_est=beta_est_i,s_beta=s_beta_het, n_size=n,alpha=0.05) # ,ifInt=FALSE  

  lower_hom  <- beta_test + result_hom[3]
  upper_hom  <- beta_test + result_hom[4]
  lower_het  <- beta_test + result_het[3]
  upper_het  <- beta_test + result_het[4]
  p_value_hom <- result_hom[5]
  p_value_het <- result_het[5]

  mat_hom[j,] <- c(beta_est[j],s_beta_hom,beta_test,lower_hom, upper_hom,p_value_hom)         # avg*2 estimates
  mat_het[j,] <- c(beta_est[j],s_beta_het,beta_test,lower_het, upper_het,p_value_het)         # avg*2 estimates
}

print("Results for 2b: ")
print(mat_hom)
print(mat_het)

## c - It is actually available in the above table. Here just to make it easier to see how it is done.
alpha <- 0.05
t_crit <- qt(1-alpha/2,length(Y)-4)
beta_i <- 2
beta_est_i  <- beta_est[beta_i]
s_beta_hom <- sqrt(v_b[beta_i,beta_i])
s_beta_het <- sqrt(v_b_het[beta_i,beta_i])
lower_hom <- beta_est_i - t_crit*s_beta_hom
upper_hom <- beta_est_i + t_crit*s_beta_hom
lower_het <- beta_est_i - t_crit*s_beta_het
upper_het <- beta_est_i + t_crit*s_beta_het


print(paste("Lower bound of beta 2 (homosk.):   ",   round(lower_hom,3)))
print(paste("Upper bound of beta 2 (homosk.):   ",   round(upper_hom,3)))
print(paste("Lower bound of beta 2 (heterosk.):   ", round(lower_het,3)))
print(paste("Upper bound of beta 2 (heterosk.):   ", round(upper_het,3)))



## d Assuming homosk.
cntr <- c(85,46,6,1)
mrate_hat <- t(beta_est) %*% cntr
sigma_hat_hom <- sqrt(t(cntr)%*%v_b%*%cntr)
sigma_hat_het <- sqrt(t(cntr)%*%v_b_het%*%cntr)
alpha <- 0.05
t_crit <- qt(1-alpha/2,length(Y)-4)
lower_theta_hom <- mrate_hat - t_crit*sigma_hat_hom
upper_theta_hom <- mrate_hat + t_crit*sigma_hat_hom
lower_theta_het <- mrate_hat - t_crit*sigma_hat_het
upper_theta_het <- mrate_hat + t_crit*sigma_hat_het


## e
cntr_0 <- c(85,46,6,0)
X <- X - matrix(cntr_0, nrow=dim(X)[1], ncol=length(cntr), byrow=TRUE)
beta_est_theta <- my_ols(X,Y)                    # beta OLS

# assert 
stopifnot(round((t(beta_est) %*% cntr),3) == round(beta_est_theta[4],3))

## f
R_vec <- c(1,10,0,0)
betas <- beta_est

theta_test <- 0
theta_hat <- t(R_vec) %*% betas

# v_theta
v_theta_hom <- (t(R_vec) %*% v_b %*% R_vec)
v_theta_het <- (t(R_vec) %*% v_b_het %*% R_vec)

wald_stat_hom <- n*(theta_hat- theta_test)%*%solve(v_theta_hom)%*%(theta_hat- theta_test)
wald_stat_het <- n*(theta_hat- theta_test)%*%solve(v_theta_het)%*%(theta_hat- theta_test)

wald_crit <- qchisq(0.95,1)
