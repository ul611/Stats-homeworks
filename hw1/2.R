# task N3

install.packages("stats4")
library(stats4)

# Parameters

alpha = 0.4
n = 1000

# Sample

X = rweibull(n, shape = 3, scale = alpha)

# Estimated alpha

LL = function(a){
  -sum(dweibull(X, shape = 3, scale = a, log=T))
}

mle(LL, start = 0.3, method = "L-BFGS-B", lower = c(0,0))

