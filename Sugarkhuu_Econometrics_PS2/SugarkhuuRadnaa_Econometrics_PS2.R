# Solutions to Problem set 2, Econometrics
# Sugarkhuu Radnaa


### Problem 1

raw_mat <- matrix(runif(100000*100,0,2), ncol=100000) # 100X100,000 matrix of random samples from U(0,2)

est_max <- apply(raw_mat, 2, max)                     # the estimate as the max from each sample 
est_avg <- apply(raw_mat, 2, function(x) 2*mean(x))   # the estimate as the 2 times the average of each sample

# summary matrix
mat <- matrix(nrow = 2, ncol = 3)
rownames(mat) <- c("theta_hat","theta_bar")
colnames(mat) <- c("mean","bias","var")

mat["theta_hat",] <- c(mean(est_avg),mean(est_avg)-2,var(est_avg))         # avg*2 estimates
mat["theta_bar",] <- c(mean(est_max),mean(est_max)-2,var(est_max))         # max estimates

print(mat)

# MSE could be there. Bjorn. 


### Problem 2

library(readxl)

df <- read_excel("cps09mar.xlsx")


# (a)

means <- apply(df[,c("education","earnings")],2,mean)
vars  <- apply(df[,c("education","earnings")],2,var)

# (b)

attach(df)
cov_edu_earn <- cov(education,earnings)
cor_edu_earn <- cor(education,earnings)
detach(df)

# (c)

t_95  <- qt(.95, df=nrow(df)-1)
t_995 <- qt(.995, df=nrow(df)-1)

tab <- matrix(nrow = 2, ncol = 2)
rownames(tab) <- c("education90","earnings99")
colnames(tab) <- c("lower","upper")

tab["education90",] <- means["education"] + 
  t_95*c(-1,1)*sqrt(vars["education"]/nrow(df))
tab["earnings99",] <- means["earnings"] + 
  t_995*c(-1,1)*sqrt(vars["earnings"]/nrow(df))


# (d) - Sample size is large



