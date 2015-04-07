# gravitational acceleration
# g = 9.8 m/s^2
g = 9.8

# samples
n = 34

# time vector in seconds (use tt because t is the transpose function in R)
time = seq(0,3.4,len=n)

# http://en.wikipedia.org/wiki/Equations_for_a_falling_body
# Distance (meters) travelled by an object falling for time (seconds)
# from the tower of Pisa (56.67m) [experiment by Galileo Galilei]
distance = 56.67 - 0.5*g*time^2

# simulate experimental data based on equation
y = distance + rnorm(n,1)
x = time

plot(x, y, xlab="time (s)", ylab="distance (m)")
lines(time, distance, xlab="time (s)", ylab="distance (m)", col=2)

# if we only had the experimental data (y) and didn't know the equation parameters
# but could guess the general formula, then we can use the data as training examples
# for the corresponding linear model:
# y = Beta0 + Beta1*x + Beta2*x^2
# Beta0 = 56.67
# Beta1 = 0
# Beta2 = - 0.5*g

rss = function(Beta0, Beta1, Beta2){
  # linear model
  r = y - (Beta0 + Beta1*x + Beta2*x^2)
  # sum of squares
  sum(r^2)
}

RSS = rss(55,0,5)
RSS

# convert model to matrix algebra form
X = cbind(1, x, x^2)
Beta = matrix(c(55,0,5), 3, 1)

r = y - X %*% Beta

RSS = t(r) %*% r
# same as
RSS = crossprod(r) # same as t(r) %*% r
RSS

Betahat = solve(t(X) %*% X) %*% t(X) %*% y
# same as
Betahat = solve(crossprod(X)) %*% crossprod(X,y)
Betahat

# backsolve is more stable than solve
QR = qr(X)
Q = qr.Q(QR)
R = qr.R(QR)
Betahat = backsolve(R, crossprod(Q,y))
Betahat

# same but using lm instead of matrix algebra
x1 = x
x2 = x^2
fit = lm(y ~ x1 + x2)
summary(fit)

