dat = read.csv("femaleMiceWeights.csv")

with(dat, stripchart(Bodyweight ~ Diet, vertical=TRUE, pch=1, method="jitter", las=2,
                     main="Bodyweight over Diet",
                     xlim=c(0,3),
                     ylim=c(0,40)))


## A linear model with one variable

levels(dat$Diet) # => "chow", "hf"

# by default, levels are in alphabetical order and the first one is the base level
# to relevel on "hf"
# dat$Diet = relevel(dat$Diet, ref="hf")

X = model.matrix(~ Diet, data=dat)

X

colnames(X)

# solve the linear model

fit = lm(Bodyweight ~ Diet, data=dat)
summary(fit)

# same as a t-test in this simple case, but linear models extend the t-test to complex cases

t.test(dat[dat$Diet == "hf",]$Bodyweight, dat[dat$Diet == "chow",]$Bodyweight, var.equal=TRUE)


# the math behind it
Y = dat$Bodyweight
X = model.matrix(~ Diet, data=dat)
solve(t(X) %*% X) %*% t(X) %*% Y

(coefs = coef(fit))


# the intercept is the average weight of mice on chow diet and
# the Diethf coefficient is the difference between the intercept and the average weight of mice on hf diet

library(RColorBrewer)

cols = brewer.pal(3, "Dark2")

offset = 0.25

abline(h=0)

abline(h=coefs[1], col=cols[1])
arrows(1-offset,0, 1-offset, coefs[1], col=cols[1], length = 0.1)

abline(h=coefs[1] + coefs[2], col=cols[2])
arrows(2+offset,coefs[1], 2+offset, coefs[1]+coefs[2], col=cols[2], length = 0.1)

legend("topright", names(coeffs), fill=cols)
