# task N1
# Parameters

theta = 1
n = 1000
sample1 = seq(1 / 100, 10, by = 0.01)
sample2 = seq(1 / 200, 12, by = 0.012)
sample3 = seq(1 / 150, 8, by = 0.008)

fnt1 = rep(0, n)
fnt2 = rep(0, n)
fnt3 = rep(0, n)

# theoretical function

f = function(t, theta){
  dexp(t, rate = theta, log = FALSE) / theta
}

# estimated function

fn = function(t, sum_sample){
  if (sum_sample > t){
    return((1 - t / sum_sample)^(n - 1))
  } else {
    return(0)
    }
}

for (i in 1:n){
  fnt1[i] = fn(sample1[i], sum(fn1))
  fnt2[i] = fn(sample2[i], sum(fn2))
  fnt3[i] = fn(sample3[i], sum(fn3))
}

# Plotting

plot(sample1, f(sample1, theta), type='l', col = 'blue', lwd = 2, 
     main="Distributions", xlab="t", ylab="f(t)")
legend(x = 'top', col = c('blue', 'black'), lwd = 2, 
       legend = c('Theoretical', 'Estimated with 1 sample'))
lines(sample1, fnt1, col = 'black', lwd = 2)

# Plotting mean of 3 samples

fnt_mean = rep(0, length(sample1))
for (i in 1:n){
  fnt_mean[i] = (fnt1[i] + fnt2[i] + fnt3[i]) / 3
}

plot(sample1, f(sample1, theta), type='l', col = 'blue', lwd = 2, 
     main="Distributions", xlab="t", ylab="f(t)")
legend(x = 'top', col = c('blue', 'black', 'gold'), lwd = 2, 
       legend = c('Theoretical', 
                  'Estimated with 1 sample', 
                  'Estimated with 3 samples'))
lines(sample1, fnt1, col = 'black', lwd = 2)
lines(sample1, fnt_mean, col = 'gold', lwd = 2)

# Mean value estimates theoretical distribution better than one-sample value
