Nsim=5000 # initial values
rho=0.7
X=Y=array(0,dim=c(Nsim,1)) # init arrays
X[1]=rnorm(1) # init chains
Y[1]=rnorm(1)
for (i in 2:Nsim){ # sampling loop
  Y[i]=rnorm(1,rho*X[i-1],1-rho^2)
  X[i]=rnorm(1,rho*Y[i],1-rho^2)
}
mcmc=cbind(X,Y)
par(mfrow=c(2,2))
plot(ts(mcmc[,1]), xlab="Trace Plot", ylab="")
plot(ts(mcmc[,2]), xlab="Trace Plot", ylab="")
hist(mcmc[,1],40, main="", xlab="")
hist(mcmc[,2],40, main="", xlab="")
par(mfrow=c(1,1))



