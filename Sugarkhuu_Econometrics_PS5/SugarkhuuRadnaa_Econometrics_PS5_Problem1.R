# Generate RV using Cholesky decomposition https://stats.stackexchange.com/questions/83172/generate-two-variables-with-precise-pre-specified-correlation

## libraries
library(tictoc)
library(ggplot2)
library(reshape2)

# import
source("utils.R")


# Computation
rhos  <- seq(-0.9, 0.9, 0.2)   # grid of rho
ns    <- seq(1000, 10000, 3000)  # grid of n
vars  <- c("E_beta1","Var_beta1","E_R2") # name of the variables
times <- 500

vars = c('Within confidence interval','Confidence interval length','HypoH02','HypoH018')
tic()

# save the results of each rho and n for each of the variables
summ_result <- array(0, dim = c(length(rhos), length(ns),length(vars)))

for(rho_i in 1:length(rhos)){
  
  rho <- rhos[rho_i]
  
  for(n_i in 1:length(ns)){
  
    n <- ns[n_i]
    
    matrix_out <- array(0,dim=c(times,length(vars)))    
    
    for(i in 1:times){
      print(c(rho, n))
      result <- my_estimator(rho, n)  # beta1 - beta3, and R2, VCOV_beta, ifinside 
      matrix_out[i,] = result[5:(5-1+length(vars))]      # whether in the interval 
    }
    
    freq_in_conf_len <- colMeans(matrix_out)

    summ_result[rho_i,n_i,] <- freq_in_conf_len # mean of beta1, variance of beta1, and mean of R2
  }
}
toc()

# plot

for(i in 1:length(vars)){
  m <- summ_result[,,i]
  rownames(m) <- rhos
  colnames(m) <- ns
  m_melt <- melt(m,c("rho", "n"),value.name = "result") # plotting grid
  
  # heatmap
  ggplot(data = m_melt, aes(x=rho, y=n, fill=result)) + 
    geom_tile()
  ggsave(paste(vars[i], ".png", sep=""))
}

# # Values of variance of beta1
# var_beta1_rho000_n500   <- summ_result[1,1,2]
# var_beta1_rho000_n10000 <- summ_result[1,20,2]
# var_beta1_rho095_n500   <- summ_result[20,1,2]
# var_beta1_rho095_n10000 <- summ_result[20,20,2]
