N = 40
p = 4
group = factor(rep(1:p, each=N/p))
X = model.matrix(~ group)


# In regression, mean squares (MS and MSE) are used to determine whether terms in the model are significant.
# The term mean square (MS) is obtained by dividing the term sum of squares (SS) by the degrees of freedom.
# The mean square of the error (MSE) is obtained by dividing the sum of squares of the residual error by the degrees of freedom.
# The MSE is the variance (s2) around the fitted regression line.
# Dividing the MS by the MSE gives F, which follows the F-distribution.
# with degrees of freedom for the term and degrees of freedom for error.

# In ANOVA, mean squares are used to determine whether factors (treatments) are significant.
# The treatment mean square is obtained by dividing the treatment sum of squares by the degrees of freedom.
# The treatment mean square represents the variation between the sample means.
# The mean square of the error (MSE) is obtained by dividing the sum of squares of the residual error by the degrees of freedom.
# The MSE represents the variation within the samples.


# generate some random, null data, where the mean is the same for all groups

Y = rnorm(N, mean=42, 7)

plot(Y, ylim=c(0,80))

# initial model is Yhat = mean(Y) = Beta0 = intercept = horizontal line
# simple linear regression with no slope term

Yhat = mean(Y)

abline(h=Yhat, col=2)

# calculate the sum of squares of the model without the term
initial.ss = sum((Y - Yhat)^2)

# calculate the sum of squares of the model with the term ('group' in this case)
s = split(Y, group)
after.group.ss = sum(sapply(s, function(x) sum((x - mean(x))^2)))

# calculate the term sum of squares (SS)
group.ss = initial.ss - after.group.ss

# calculate the term mean square (MS) by diving the term sum of squares by the degrees of freedom
group.ms = group.ss / (p - 1)

# calculate the mean square of the error (MSE) by dividing the sum of squares of the residual error by the degrees of freedom
after.group.ms = after.group.ss / (N - p)

# calculate the F-value by dividing the MS by the MSE
fval = group.ms / after.group.ms

# under the null hypothesis, the F-value has a nice mathematical formula that describe its distribution
# and can be used for hypothesis testing


# Montecarlo simulation to inspect the F-distribution

fvals = replicate(1000, (function(){
  Y = rnorm(N, mean=42, 7)
  Yhat = mean(Y)
  initial.ss = sum((Y - Yhat)^2)
  s = split(Y, group)
  after.group.ss = sum(sapply(s, function(x) sum((x - mean(x))^2)))
  group.ss = initial.ss - after.group.ss
  group.ms = group.ss / (p - 1)
  after.group.ms = after.group.ss / (N - p)
  fval = group.ms / after.group.ms
  return(fval)
})())

mean(fvals)
hist(fvals, col="grey", border="white", breaks=50, freq=FALSE)

xs <- seq(from=0,to=6,length=100)
lines(xs, df(xs, df1 = p - 1, df2 = N - p), col="red")
