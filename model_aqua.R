model{
	# Prediction
  A[1] <- beta[1] + beta[2]*0
  A[2] <- beta[1] + beta[2]*1
  for(i in 1:100){
    Yp[i,1] <- A[1] + b*X[i]
    Yp[i,2] <- A[2] + b*X[i]
  }
  
	# Likelihood
	for(i in 1:Nind){
		y[i] ~ dnorm(mu[i], tau[1])
		mu[i] <- a[block[i]] + b*x[i]
	}
		
		for(j in 1:Nblock){
			a[j] ~ dnorm(mu.a[j], tau[2])
			mu.a[j] <- beta[1] + beta[2]*inf[j]
		}
	
	# Prior
  ninfo <- 1.0E-4
	b ~ dnorm(0, ninfo)
  for(i in 1:2){ beta[i] ~ dnorm(0, ninfo) }
  for(i in 1:2){
		tau[i] ~ dgamma(ninfo, ninfo)
		sigma[i] <- 1/sqrt(tau[i])
	}
	
}
