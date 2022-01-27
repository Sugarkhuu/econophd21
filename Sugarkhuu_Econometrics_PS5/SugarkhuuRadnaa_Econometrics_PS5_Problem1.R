# Generate RV using Cholesky decomposition https://stats.stackexchange.com/questions/83172/generate-two-variables-with-precise-pre-specified-correlation

## libraries
library(tictoc)
library(ggplot2)
library(reshape2)

# import
source("utils.R")


# Computation
rhos  <- seq(-0.9, 0.9, 0.3) #seq(-0.9, 0.9, 0.2)  # grid of rho
ns    <- c(100,500,1000,2500,5000,10000) # seq(1000, 10000, 3000)  # grid of n
times <- 1000

# Interested: prob in 95 conf int, length of 95 conf int, Hypo H0 2, Hypo H0 1.8
vars = c('in_95','len_95','HypoH02','HypoH018')
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
      result <- my_estimator(rho, n) # betas, R2, if in interval, interval length,  if null true, if null true  
      matrix_out[i,] = result[5:(5-1+length(vars))]      # if in interval, interval length,  if null true, if null true  
    }
    
    summ_result[rho_i,n_i,] <- colMeans(matrix_out) # if in interval, interval length,  if null true, if null true  
  }
}
toc()

# tabulate
m_in95 <- summ_result[,,1]
rownames(m_in95) <- rhos
colnames(m_in95) <- ns
m_len95 <- round(summ_result[,,2],2)
rownames(m_len95) <- rhos
colnames(m_len95) <- ns
m_if2true <- summ_result[,,3]
rownames(m_if2true) <- rhos
colnames(m_if2true) <- ns
m_if18true <- summ_result[,,4]
rownames(m_if18true) <- rhos
colnames(m_if18true) <- ns

m_in95
m_len95
m_if2true
m_if18true

# plot
# for(i in 1:length(vars)){
#   m <- summ_result[,,i]
#   rownames(m) <- rhos
#   colnames(m) <- ns
#   m_melt <- melt(m,c("rho", "n"),value.name = "result") # plotting grid
  
#   # heatmap
#   ggplot(data = m_melt, aes(x=rho, y=n, fill=result)) + 
#     geom_tile()
#   ggsave(paste(vars[i], ".png", sep=""))
# }

# # Values of variance of beta1
# var_beta1_rho000_n500   <- summ_result[1,1,2]
# var_beta1_rho000_n10000 <- summ_result[1,20,2]
# var_beta1_rho095_n500   <- summ_result[20,1,2]
# var_beta1_rho095_n10000 <- summ_result[20,20,2]
