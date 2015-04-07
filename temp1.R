g = 9.8 # gravitational acceleration in meters/second^2

h0 = 56.67 # initial distance in meters = height of the tower of Pisa

v0 = 0 # initial velocity in meters per second

n = 25 # number of measurements

tt = seq(0,3.4,len=n) # time in secs (not using t because it is a base function)

# equation for distance travelled by an object for time t
# n.b.: g = -2*beta2
y = h0 + v0 *tt - 0.5* g *tt^2 + rnorm(n,sd=1)

X = cbind(1,tt,tt^2)

A = solve(crossprod(X))%*%t(X)

ghat = -2*(A %*% y)[3]


# Monte Carlo simulation to generate the sampling distribution of ghat

ghat_sampling_distro = replicate(100000, (function(){
  y = h0 + v0 *tt - 0.5* g *tt^2 + rnorm(n,sd=1)
  
  X = cbind(1,tt,tt^2)
  
  A = solve(crossprod(X))%*%t(X)
  
  ghat = -2*(A %*% y)[3]
  
  ghat
})())

ghat_mean = mean(ghat_sampling_distro)
ghat_se = sd(ghat_sampling_distro)

###############################################################################

library(UsingR)
library(dplyr)

data(father.son)

X = father.son$fheight

Y = father.son$sheight

n = 50

betahat_sampling_distro = replicate(10000, (function(){
  index = sample(n, N)
  
  sampledat = sample_n(father.son, n)
  
  x = sampledat$fheight
  
  y = sampledat$sheight
  
  betahat = lm(y~x)$coefficients[2]
  
  betahat
})())

betahat_mean = mean(betahat_sampling_distro)
betahat_se = sd(betahat_sampling_distro)

###############################################################################

# covariance
mean((Y-mean(Y))*(X-mean(X))) # almost same as cov(Y, X)

###############################################################################

library(UsingR)
library(dplyr)

data(father.son)

X = father.son$fheight

Y = father.son$sheight

n = 50

set.seed(1)

index = sample(n, N)
  
sampledat = sample_n(father.son, n)
  
x = sampledat$fheight
  
y = sampledat$sheight
  
fit = lm(y~x)

yhat = fit$fitted.values

ssr = sum((y - yhat)^2)
# same as
ssr = sum(fit$residuals^2)

# slop estimate
sigma2 = ssr/(n-2) # (sample size (n) - number of terms in the model (p))

# build the design matrix
X = cbind(1, x)

# calculate the slop standard error
sqrt(sigma2 * diag(solve(t(X)%*%X)))

###############################################################################
