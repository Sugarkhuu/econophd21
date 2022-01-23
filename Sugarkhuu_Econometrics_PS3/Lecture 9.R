rm(list = ls())

set.seed(4519)

nsims <- 10000


# Example 1: Bernoulli distribution

# Set parameters
n <- 2
p <- 0.5

# Simulate all data. Each column is one simulated data set of size n
rvs <- (replicate(nsims, rbinom(n, 1, prob=p)))

# Calculate all estimates
p_hat <- colMeans(rvs)
hist(p_hat,xlim = c(0,1),xlab="Estimator",ylab="Frequency", col = 'steelblue')


# P(|p_hat - p| < eps)
eps = seq(0.01,0.6,by=0.01)
int_prob <-  colMeans(abs(p - matrix(rep(p_hat,length(eps)),nsims,length(eps)))<t(matrix(rep(eps,length(p_hat)),length(eps),nsims)))
plot(eps,int_prob,ylim = c(0,1),xlab="epsilon",ylab="Probability", col = 'steelblue',pch = 19)



# Example 2: Uniform distribution

# Set parameters
n <- 20
theta <- 2

# Simulate all data. Each column is one simulated data set of size n
rvs <- (replicate(nsims, runif(n,min = 0, max = theta)))

# Calculate all estimates
theta_hat <- apply(rvs, 2, max)

# Histogram of estimates
hist(theta_hat,xlim = c(1.5,2.5),xlab="Estimator",ylab="Frequency", col = 'steelblue')

# P(|theta_hat - theta| <= eps)
eps = seq(0.01,0.6,by=0.01)
int_prob <-  colMeans(abs(theta - matrix(rep(theta_hat,length(eps)),nsims,length(eps)))<=t(matrix(rep(eps,length(theta_hat)),length(eps),nsims)))
plot(eps,int_prob,ylim = c(0,1),xlab="epsilon",ylab="Probability", col = 'steelblue',pch = 19)


# P(n*(theta-theta_hat) <= z)
z = seq(0,10,by=0.1)
cdf_theta_hat <-  colMeans(n*(theta - matrix(rep(theta_hat,length(z)),nsims,length(z)))<=t(matrix(rep(z,length(theta_hat)),length(z),nsims)))

# Plot and compare to exponential
plot(z,cdf_theta_hat,ylim = c(0,1),xlab="z",ylab="Probability", col = 'steelblue')
lines(z,  pexp(z, rate = 1/2, lower.tail = TRUE, log.p = FALSE),col = 'red')





