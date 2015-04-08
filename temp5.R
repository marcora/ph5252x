sex = factor(rep(c("female","male"),each=4))
trt = factor(c("A","A","B","B","C","C","D","D"))

X = model.matrix( ~ sex + trt)

ncol(X)
qr(X)$rank

Y = 1:8

makeYstar <- function(a,b) Y - X[,2] * a - X[,5] * b

fitTheRest <- function(a,b) {
  Ystar <- makeYstar(a,b)
  Xrest <- X[,-c(2,5)]
  betarest <- solve(t(Xrest) %*% Xrest) %*% t(Xrest) %*% Ystar
  residuals <- Ystar - Xrest %*% betarest
  sum(residuals^2)
}

(fitTheRest(1,2))

outer(-2:8,-2:8,Vectorize(fitTheRest))
