# mu - mean
# sigma = variance
# p = probability of observing value from first distribution
# n = number of samples
mixture <- function(mu=c(mu1, mu2), sigma= c(sigma1,sigma2), p, n){
  observations = vector()

  for(i in 1:n){
    if(rbinom(n = 1, size = 1, prob = p) == 1){observations[i] = rnorm(1, mu[1], sigma[1])}
    else{observations[i] = rnorm(1, mu[2], sigma[2])}
  }
  observations
}














