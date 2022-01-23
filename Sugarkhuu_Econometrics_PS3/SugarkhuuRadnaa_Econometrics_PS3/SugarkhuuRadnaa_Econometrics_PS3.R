# Solutions to Problem set 3, Econometrics, Dec 2021
# Sugarkhuu Radnaa



### Problem 1

# Define a function
act_cover <- function(it=10000,theta=1) {
#' Calculate actual coverage probability for exponential distribution
#' theta - mean value of the distribution
#' it    - number of different draws
  set.seed(1234)
  
  n      <- 50               # sample size
  it     <- it               # number of different draws
  theta  <- theta            # theta given
  alpha  <- 0.05             # significance given
  z_crit <- qnorm(1-alpha/2) # 97.5th percentile of standard normal dist

  
  # [n, it] draws from exp(-x)
  data_mat <- matrix(rexp(n*it,1/theta), ncol=it) 
  
  # Confidence interval
  conf_int <- matrix(apply(data_mat, 2, function(x){mean(x)+c(-1,1)*z_crit*sd(x)/sqrt(length(x))}),ncol=it)
  
  # check if within the interval: Boolean
  ifInside <- rbind(conf_int[1,] <= theta & conf_int[2,] >= theta)
  
  act_cover_p = mean(ifInside) # percentage within interval

  return(act_cover_p)
}

# Answers 

# (a)
it <- 10000
sprintf("Actual coverage probability with %i draws is: %.3f", it,act_cover(it))
# (b)
it <- 1000
sprintf("Actual coverage probability with %i draws is: %.3f", it,act_cover(it))




### Problem 2

## Setup
n      <- 100                # sample size
theta  <- 2                  # population parameter guess 
alpha  <- 0.05               # chosen significance level 
z_crit <- qnorm(1-alpha/2)   # (1-alpha/2) critical value of N(0,1)


## Building critical values assuming the null hypothesis

# First estimator
se_hat <- theta/sqrt(n) # se for the sample size of 100
# critical values
crit_u <- theta + z_crit*se_hat
crit_l <- theta - z_crit*se_hat

# Second estimator
se_tilde <- sqrt(1.25)*theta/sqrt(n)
# critical values
crit_u_tilde <- theta + z_crit*se_tilde
crit_l_tilde <- theta - z_crit*se_tilde

## Now build alternative critical values for alternative thetas
theta_grid    <- seq(0,4,0.01)
se_hat_grid   <- theta_grid*0 # being lazy 
se_tilde_grid <- theta_grid*0 # being lazy 


# create standard errors for alternative theta values
for (i in 1:length(theta_grid)){
  theta <- theta_grid[i]
  
  # first
  se_hat_grid[i] <- theta/sqrt(n)
  # second
  se_tilde_grid[i] <- sqrt(1.25)*theta/sqrt(n) # se for the sample size of 100  
  }


# rejection probabilities of alternative thetas. If an alternative theta was true
# then how much (probability) area the interval [crit_l, crit_u] of the null hypothesis will miss 
# to include, thus rejecting the null correctly
power_seq_hat   <- pnorm(crit_l,theta_grid,se_hat_grid) + 1 - pnorm(crit_u,theta_grid,se_hat_grid)
power_seq_tilde <- pnorm(crit_l_tilde,theta_grid,se_tilde_grid) + 1 - pnorm(crit_u_tilde,theta_grid,se_tilde_grid)


# plot rejection probability (power) versus alternative thetas
plot(theta_grid, power_seq_hat, type = "l", col="black", ylab = "Power", xlab = expression(theta),lwd=2) # , main="Power function"
lines(theta_grid, power_seq_tilde, type = "l",col="blue",lwd=2)
legend(3.0, 0.25, legend=c("First estimator", "Second estimator"),
       col=c("black", "blue"), lty=1:1)

### END