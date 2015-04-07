species = factor(c("A","A","B","B"))
condition = factor(c("control","treated","control","treated"))

X = model.matrix(~ species + condition)

# library(devtools); install_github("ririzarr/rafalib")
library(rafalib)
imagemat(X, main="Model matrix for spider.sub linear model")

spider = read.csv("spider_wolff_gorb_2013.csv", skip=1)

fit = lm(friction ~ type + leg, data=spider)
summary(fit)
(coefs = coef(fit))

library(contrast)

(L3vsL2 = contrast(fit, list(leg="L4", type="pull"), list(leg="L2", type="pull")))


X <- model.matrix(~ type + leg, data=spider)

(Sigma = sum(fit$residuals^2)/(nrow(X) - ncol(X)) * solve(t(X) %*% X))

C <- matrix(c(0,0,-1,0,1),1,5)

