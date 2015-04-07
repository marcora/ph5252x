library(dplyr)
library(magrittr)

par(mfrow=c(1,2))

spider = read.csv("spider_wolff_gorb_2013.csv", skip=1)

with(spider, boxplot(friction ~ type * leg, col=c("grey90", "grey45"), las=2,
                     main="Comparison of friction coefficients of different leg pairs"))

spider %<>% mutate(log2friction = log2(friction))

with(spider, boxplot(log2friction ~ type * leg, col=c("grey90", "grey45"), las=2,
                     main="Comparison of log2friction coefficients of different leg pairs"))

par(mfrow=c(1,1))

fit = lm(log2friction ~ type * leg, data=spider)
summary(fit)
(coefs = coef(fit))

# Question: Is the difference between push and pull in L4 significantly different from the difference between push and pull in L1?
# (grey (push:L4) + orange (push)) - orange (push) = grey (push:L4)
# [see summary of fit for t-value of grey (push:L4) coefficient estimate]
summary(fit)
# same as
(L4pull.vs.L1pull = contrast(fit, list(leg="L4", type="pull"), list(leg="L1", type="pull")))
L4pull.vs.L1pull$X # model/contrast matrix

# Question: Is the difference between pull and push the same for all leg pairs?
anova(fit)

# Question: Is the difference between L2 pull and L1 pull significant?
# (green blue (L2) + (Intercept/L1)) - green (Intercept/L1) = green blue (L2)
# [see summary of fit for t-value of grey (L2) coefficient estimate]
summary(fit)
# same as
(L2pull.vs.L1pull = contrast(fit, list(leg="L2", type="pull"), list(leg="L1", type="pull")))
L2pull.vs.L1pull$X # model/contrast matrix

# Question: Is the difference between L2 push and L1 push significant?
# same as
(L2push.vs.L1push = contrast(fit, list(leg="L2", type="push"), list(leg="L1", type="push")))
L2push.vs.L1push$X # model/contrast matrix
