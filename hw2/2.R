# task N2

N=1000
eta=sample(1:6, size=N, prob=c(0.5,rep(0.1,5)),
           replace=TRUE)
hist(eta)
table(eta)
m=c(0,-1,-0.5,0,0.5,1)
s=c(1,rep(0.1,5))
X=rep(0,N)
for (i in 1:N){
  X[i]=rnorm(1,mean=m[eta[i]],sd=s[eta[i]])
}