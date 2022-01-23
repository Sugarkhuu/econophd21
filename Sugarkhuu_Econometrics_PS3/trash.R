


theta_dev <- sqrt(n)*(theta-theta0)
theta_hat <- theta0 + theta_dev

power_seq <- pnorm(crit,theta_hat,sd)
plot(theta, power_seq, type = "l", ylab = "Power", xlab = expression(theta), main="Power function" )





t_seq <- seq(0,10,0.01)
dexp(t_seq,1)
dexp(9,1)
qexp(0.9,1)
pexp(qexp(0.9,1),1)
rexp(n*it,1/theta)

rexp(n*it,1/theta)


### Problem 7.1
# (a)
# Will use 0.05th percentila value of standardized normal distribution


# (b)
n     <- 100
theta0 <- 1
sd    <- 1
alpha <- 0.05

crit <-qnorm(alpha,theta0,sd)

theta <- seq(-1, 3, 0.01)
theta_dev <- sqrt(n)*(theta-theta0)
theta_hat <- theta0 + theta_dev

power_seq <- pnorm(crit,theta_hat,sd)
plot(theta, power_seq, type = "l", ylab = "Power", xlab = expression(theta), main="Power function" )


# (c)
# [crit,\infty] - crit <-qnorm(alpha,theta0,sd)