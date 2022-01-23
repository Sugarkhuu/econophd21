# Drawing of pmf and calculation of mean and variance of the given random variable


library(ggplot2)

data <- data.frame(x_vals = -1:3, y_vals = c(0.05,0.2,0.25,0.1,0.4))

ggplot(data, aes(x=x_vals, y=y_vals)) +
    geom_bar(stat='identity',width = 0.5) +
    labs(title = 'pmf of X', x = "X", y = "P(X)") +
    geom_text(aes(label=y_vals), vjust=1.6, color="white", size=4) 
    

x_mean  = sum(data$x_vals * data$y_vals)
x_var   = sum((data$x_vals - x_mean)^2*data$y_vals)


# Print outcome
sprintf('The mean of the variable is: %.2f', x_mean)
sprintf('The variance of the variable is: %.2f', x_var)

help(dunif)
