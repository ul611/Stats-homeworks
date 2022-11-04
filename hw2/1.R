# task N1

n = 1000
shape = 3
rate = 4

# (i) 
# моделируем выборку
X = rgamma(n, shape = shape, rate = rate)

# (ii)
# значения bandwidth
bws = seq(from = 0.1, to = 5, by = 0.1)
MISE_E = rep(0, length(bws))

# х из отрезка [0, 2]
xms = seq(from = 0, to = 2, by = 0.01)
ps = rep(0, length(xms)) # истинные значения плотности
pns = rep(0, length(xms)) # значения плотности с ядром Епанечникова
dps = rep(0, length(xms)) # разность значений плотности

for(j in 1:length(bws)){
  pns = density(X, n = 201, from=0, to=2, bw=bws[j])$y
  for(i in 1:length(ps)){
    ps[i]=dgamma(xms[i], shape=shape, rate=rate)
    dps[i] = (ps[i] - pns[i])^2
  }
  MISE_E[j] = mean(dps)
}

bws[which.min(MISE_E)] # минимум MISE при bandwidth = 0.1

# (iii)
# ядро с  первыми 3 многочленами Лежандра на отрезке [−1, 1]
K = function(x){
  9/8 - 15/8 * x^2
}

pns2 = rep(0, length(xms)) # значения плотности с ядром из задания
ks = rep(0, length(X)) # значения плотности с ядром из задания
MISE_L = rep(0, length(bws))

for (a in length(bws)){
  for (i in length(pns2)){
    xi = xms[i]
    n = 0
    ks = rep(0, length(X))
    for (j in length(ks)){
      dlt = (X[j] - xi) / bws[a]
      if ((dlt < 1) & (dlt > -1)){
        ks[j] = K(dlt)
        n = n + 1
      } else {
        ks[j] = 0
      }
    }
    pns2[i] = sum(ks) / (bws[a] * n)
    dps[i] = (ps[i] - pns2[i])^2
  }
  MISE_L[a] = mean(dps)
}

bws[which.min(MISE_L)] # минимум MISE при bandwidth = 0.1

# Графически сравниваем оценки MISE с ядром Епанечникова и с ядром (2),
# в зависимости от значения параметра bandwidth.

plot(bws, MISE_E, type='l', col = 'blue', lwd = 2, 
     main="MISE(bw) plot", xlab="bandwidth", ylab="MISE(bw)")
lines(bws, MISE_L, col = 'red', lwd = 2)
legend(x = 'topleft', col = c('blue', 'red'), lwd = 2, 
       legend = c('MISE_Epanechnikov', 'MISE_Legendre'))