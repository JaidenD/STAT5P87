mixture <- function(mu=c(mu1, mu2), sigma= c(sigma1,sigma2), p, n){
  observations = rep(NA, n)

  for(i in 1:n){
    if(rbinom(n = 1, size = 1, prob = p) == 1){observations[i] = rnorm(1, mu[1], sigma[1])}
    else{observations[i] = rnorm(1, mu[2], sigma[2])}
  }
  observations
}

mixture(c(0,0), c(1,0), 1, 100) # For testing














