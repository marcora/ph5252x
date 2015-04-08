spider = read.csv("spider_wolff_gorb_2013.csv", skip=1)

fit = lm(friction ~ type + leg, data=spider)

(betahat = coef(fit))

Y = matrix(spider$friction, ncol=1)
X = model.matrix(~ type + leg, data=spider)

QR <- qr(X)
Q <- qr.Q( QR )
R <- qr.R( QR )
(betahat = backsolve(R, crossprod(Q,Y) ) )
