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

# results
beta_est <- my_ols(X,Y)                    # beta OLS
r2 <- r_squared(beta_est, X, Y)            # r squared

print(beta_est)
print(r2)

dfa <- melt(df[c("water","rural","health_gdp","mrate")],  id.vars = 'mrate', variable.name = 'series')

#create line plot for each column in data frame
ggplot(dfa, aes(value,mrate)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  facet_grid(. ~ series, scales = "free")

ggsave(paste("mrate_scatter", ".png", sep=""))
