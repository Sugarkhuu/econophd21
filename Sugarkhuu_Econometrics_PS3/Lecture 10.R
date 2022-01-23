rm(list = ls())

set.seed(4517)

nsims <- 10000


# Example: Bernoulli distribution

# Set parameters
n <- 20
p <- 0.5

# Simulate all data. Each column is one simulated data set of size n
rvs <- (replicate(nsims, rbinom(n, 1, prob=p)))

# Calculate all estimates
p_hat <- colMeans(rvs)

# Histograms
hist(p_hat,xlim = c(0,1),xlab="Estimator",ylab="Frequency", col = 'steelblue')

hist(p_hat,xlab="Estimator",ylab="Frequency", col = 'steelblue')

hist(sqrt(n)*(p_hat-p),xlim = c(-3,3),breaks = seq(-3, 3, 0.3),xlab="Standardized Estimator",ylab="Frequency", col = 'steelblue')


# P(sqrt(n)*(p_hat - p) <= z)
z = seq(-3,3,by=0.05)
cdf_p_hat <-  colMeans(sqrt(n)*(matrix(rep(p_hat,length(z)),nsims,length(z)) - p) <=t(matrix(rep(z,length(p_hat)),length(z),nsims)))

# Plot and compare to normal
plot(z,cdf_p_hat,ylim = c(0,1),xlab="z",ylab="Probability", col = 'steelblue')
lines(z,  pnorm(z, 0,sqrt(p*(1-p))),col = 'red')




# Example 2: Asymmetric distribution

# Set parameters
n <- 20
k <- 3
p <- 1/k

# Simulate all data. Each column is one simulated data set of size n
rvs <- (replicate(nsims, rbinom(n, 1, prob=p)))
ind = (rvs==0)
rvs[ind] = -1/sqrt(k-1)
rvs[!ind] = sqrt(k-1)

# Calculate all estimates
theta_hat <- colMeans(rvs)
# Here E[theta_hat] = 0

# Plot histogram
hist(theta_hat,xlab="Estimator",ylab="Frequency", col = 'steelblue')

# P(sqrt(n)*(theta_hat - 0) <= z)
z = seq(-3,3,by=0.01)
cdf_theta_hat <-  colMeans(sqrt(n)*(matrix(rep(theta_hat,length(z)),nsims,length(z)) - 0) <=t(matrix(rep(z,length(theta_hat)),length(z),nsims)))

# Plot and compare to normal
plot(z,cdf_theta_hat,ylim = c(0,1),xlab="z",ylab="Probability", col = 'steelblue',pch = 19)
lines(z,  pnorm(z, 0,1),col = 'red',lwd = 2)


# Berry Esseen Illustration
lambda = sqrt(k-1)*(k-1)/k + 1/sqrt((k-1)*k)
C = 0.4748

lim_plot = ceiling(C*lambda/sqrt(n)*10)/10
plot(z,cdf_theta_hat - pnorm(z, 0,1),ylim = c(-lim_plot,lim_plot),xlab="z",ylab="Probability", col = 'steelblue',pch = 19)
lines(z,  rep(min(C*lambda/sqrt(n),1),length(z)),col = 'red',lwd = 2)
lines(z,  -rep(min(C*lambda/sqrt(n),1),length(z)),col = 'red',lwd = 2)

C*lambda/sqrt(n)
 
