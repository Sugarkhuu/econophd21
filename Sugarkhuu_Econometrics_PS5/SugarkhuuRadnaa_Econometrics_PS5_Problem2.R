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
beta_est <- my_ols(X,Y)                    # beta OLS

v_b <- vcov_beta(beta_est, X, Y)
v_b_het <- vcov_beta_het(beta_est, X, Y)

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
  result_hom <- conf_interval(beta_true=beta_test, beta_est=beta_est_i,s_beta=s_beta_hom, n_size=n,alpha=0.05,ifInt=FALSE)  
  result_het <- conf_interval(beta_true=beta_test, beta_est=beta_est_i,s_beta=s_beta_het, n_size=n,alpha=0.05,ifInt=FALSE)  
  lower_hom  <- beta_test + result_hom[3]
  upper_hom  <- beta_test + result_hom[4]
  lower_het  <- beta_test + result_het[3]
  upper_het  <- beta_test + result_het[4]
  p_value_hom <- result_hom[5]
  p_value_het <- result_het[5]

  mat_hom[j,] <- c(beta_est[j],s_beta_hom,beta_test,lower_hom, upper_hom,p_value_hom)         # avg*2 estimates
  mat_het[j,] <- c(beta_est[j],s_beta_het,beta_test,lower_het, upper_het,p_value_het)         # avg*2 estimates
}

print(mat_hom)
print(mat_het)

## c
beta_i <- 2
beta_test <- beta_est[beta_i]
beta_est_i  <- beta_est[beta_i]
s_beta_hom <- sqrt(v_b[beta_i,beta_i])
s_beta_het <- sqrt(v_b_het[beta_i,beta_i])
result_hom <- conf_interval(beta_true=beta_test, beta_est=beta_est_i,s_beta=s_beta_hom, n_size=n,alpha=0.05)  
result_het <- conf_interval(beta_true=beta_test, beta_est=beta_est_i,s_beta=s_beta_het, n_size=n,alpha=0.05)  
lower_hom  <- result_hom[3]
upper_hom  <- result_hom[4]
lower_het  <- result_het[3]
upper_het  <- result_het[4]


## d
cntr <- c(85,46,6,1)
mrate_hat <- t(beta_est) %*% cntr
sigma_ehat <- sigma_error(beta_est, X, Y)
tmp <- conf_interval(beta_est=mrate_hat,s_beta=sigma_ehat,alpha=0.05)  
conf_int_theta <- tmp[3:4]

## e
cntr_0 <- c(85,46,6,0)
X <- X - matrix(cntr_0, nrow=dim(X)[1], ncol=length(cntr), byrow=TRUE)

beta_est_theta <- my_ols(X,Y)                    # beta OLS

# assert 
stopifnot(round((t(beta_est) %*% cntr),3) == round(beta_est_theta[4],3))

# f
R_vec <- c(1,10,0,0)
betas <- beta_est

theta_test <- 0
theta_hat <- t(R_vec) %*% betas

# sigma*R'*Vb*R
sd_theta <- sigma_ehat[1]*sqrt((t(R_vec) %*% solve((t(X) %*% X)) %*% R_vec)/n)

t_stat <- (theta_hat- theta_test)/sd_theta

pt(t_stat,n-length(betas))

# print(beta_est)
# print(r2)

# dfa <- melt(df[c("water","rural","health_gdp","mrate")],  id.vars = 'mrate', variable.name = 'series')

# #create line plot for each column in data frame
# ggplot(dfa, aes(value,mrate)) +
#   geom_point() +
#   geom_smooth(formula = y ~ x, method = "lm") +
#   facet_grid(. ~ series, scales = "free")

# ggsave(paste("mrate_scatter", ".png", sep=""))
