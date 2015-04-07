library(dplyr)
library(magrittr)

spider = read.csv("spider_wolff_gorb_2013.csv", skip=1)

with(spider, boxplot(friction ~ type * leg, col=c("grey90", "grey45"), las=2,
                     main="Comparison of pull and push friction coefficients of different legs"))

#################################
# let's start with just one leg #
#################################

spider.sub = filter(spider, leg == "L1")

fit = lm(friction ~ type, data=spider.sub)
summary(fit)
(coefs = coef(fit))

# same as the t-test
t.test(subset(spider.sub, type=="pull")$friction, subset(spider.sub, type=="push")$friction, var.equal = TRUE)

spider.sub %>%
  group_by(type) %>%
  summarise(mean_friction = mean(friction)) %>%
  print

X = model.matrix(~ type, data=spider.sub)
colnames(X)
nrow(X)
head(X)

# library(devtools); install_github("ririzarr/rafalib")
library(rafalib)
imagemat(X, main="Model matrix for linear model")

with(spider.sub, stripchart(friction ~ type, vertical=TRUE, pch=1, method="jitter", las=2,
                        main="Comparison of pull and push friction coefficients of all legs",
                        xlim=c(0,3),
                        ylim=c(0,2)))

library(RColorBrewer)

cols = brewer.pal(3, "Dark2")

offset = 0.25

abline(h=0)
abline(h=coefs[1], col=cols[1])
abline(h=coefs[1] + coefs[2], col=cols[2])

arrows(1-offset, 0, 1-offset, coefs[1], col=cols[1], length = 0.1)
arrows(2-offset, coefs[1], 2-offset, coefs[1]+coefs[2], col=cols[2], length = 0.1)

legend("topright", names(coefs), fill=cols)


####################################
# let's condition on all four legs #
####################################

fit = lm(friction ~ type + leg, data=spider)
summary(fit)
(coefs = coef(fit))

spider %>%
  group_by(type, leg) %>%
  summarise(mean_friction = mean(friction)) %>%
  print

X = model.matrix(~ type + leg, data=spider)
colnames(X)
nrow(X)
head(X)

imagemat(X, main="Model matrix for linear model")

with(spider, stripchart(friction ~ type + leg, vertical=TRUE, pch=1, method="jitter", las=2,
                            main="Comparison of pull and push friction coefficients of different legs",
                            xlim=c(0,12),
                            ylim=c(0,2)))

library(RColorBrewer)

cols = brewer.pal(5, "Dark2")

offset = 0.25

abline(h=0)
abline(h=coefs[1], col=cols[1])
abline(h=coefs[1]+coefs[2], col=cols[2])

arrows(1-offset, 0, 1-offset, coefs[1], col=cols[1], length = 0.1)

arrows(3-offset, coefs[1], 3-offset, coefs[1]+coefs[3], col=cols[3], length = 0.1)
arrows(5-offset, coefs[1], 5-offset, coefs[1]+coefs[4], col=cols[4], length = 0.1)
arrows(7-offset, coefs[1], 7-offset, coefs[1]+coefs[5], col=cols[5], length = 0.1)

segments(3-offset, coefs[1]+coefs[3], 4-offset, coefs[1]+coefs[3], col=cols[3])
segments(5-offset, coefs[1]+coefs[4], 6-offset, coefs[1]+coefs[4], col=cols[4])
segments(7-offset, coefs[1]+coefs[5], 8-offset, coefs[1]+coefs[5], col=cols[5])

arrows(2-offset, coefs[1], 2-offset, coefs[1]+coefs[2], col=cols[2], length = 0.1)
arrows(4-offset, coefs[1]+coefs[3], 4-offset, coefs[1]+coefs[3]+coefs[2], col=cols[2], length = 0.1)
arrows(6-offset, coefs[1]+coefs[4], 6-offset, coefs[1]+coefs[4]+coefs[2], col=cols[2], length = 0.1)
arrows(8-offset, coefs[1]+coefs[5], 8-offset, coefs[1]+coefs[5]+coefs[2], col=cols[2], length = 0.1)

legend("topright", names(coefs), fill=cols)


#############################################################################
# let's condition on all four legs with an interaction between type and leg #
#############################################################################

fit = lm(friction ~ type + leg + type:leg, data=spider)
# same as
fit = lm(friction ~ type*leg, data=spider)
summary(fit)
(coefs = coef(fit))

X = model.matrix(~ type + leg + type:leg, data=spider)
# same as
X = model.matrix(~ type*leg, data=spider)
colnames(X)
nrow(X)
imagemat(X, main="Model matrix for linear model")


with(spider, stripchart(friction ~ type + leg, vertical=TRUE, pch=1, method="jitter", las=2,
                        main="Comparison of push and pull friction coefficients of different legs",
                        xlim=c(0,12),
                        ylim=c(0,2)))

library(RColorBrewer)

cols = brewer.pal(8, "Dark2")

offset = 0.25

abline(h=0)
abline(h=coefs[1], col=cols[1])
abline(h=coefs[1]+coefs[2], col=cols[2])

arrows(1-offset, 0, 1-offset, coefs[1], col=cols[1], length = 0.1)

arrows(3-offset, coefs[1], 3-offset, coefs[1]+coefs[3], col=cols[3], length = 0.1)
arrows(5-offset, coefs[1], 5-offset, coefs[1]+coefs[4], col=cols[4], length = 0.1)
arrows(7-offset, coefs[1], 7-offset, coefs[1]+coefs[5], col=cols[5], length = 0.1)

segments(3-offset, coefs[1]+coefs[3], 4-offset, coefs[1]+coefs[3], col=cols[3])
segments(5-offset, coefs[1]+coefs[4], 6-offset, coefs[1]+coefs[4], col=cols[4])
segments(7-offset, coefs[1]+coefs[5], 8-offset, coefs[1]+coefs[5], col=cols[5])

arrows(2-offset, coefs[1], 2-offset, coefs[1]+coefs[2], col=cols[2], length = 0.1)

arrows(4-offset, coefs[1]+coefs[3], 4-offset, coefs[1]+coefs[2]+coefs[3], col=cols[2], length = 0.1)
arrows(6-offset, coefs[1]+coefs[4], 6-offset, coefs[1]+coefs[2]+coefs[4], col=cols[2], length = 0.1)
arrows(8-offset, coefs[1]+coefs[5], 8-offset, coefs[1]+coefs[2]+coefs[5], col=cols[2], length = 0.1)

arrows(4-offset, coefs[1]+coefs[2]+coefs[3], 4-offset, coefs[1]+coefs[2]+coefs[3]+coefs[6], col=cols[6], length = 0.1)
arrows(6-offset, coefs[1]+coefs[2]+coefs[4], 6-offset, coefs[1]+coefs[2]+coefs[4]+coefs[7], col=cols[7], length = 0.1)
arrows(8-offset, coefs[1]+coefs[2]+coefs[5], 8-offset, coefs[1]+coefs[2]+coefs[5]+coefs[8], col=cols[8], length = 0.1)

legend("topright", names(coefs), fill=cols)


## A contrast is a combination of coefficients
library(contrast)

# Question: Is the difference between L3 and L2 significant (pull or push, it doesn't matter!)?
# pink(L3) - blue (L2)
(L3.vs.L2 = contrast(fit, list(leg="L3", type="pull"), list(leg="L2", type="pull")))
L3.vs.L2$X # contrast matrix
# (Intercept) typepush legL2 legL3 legL4 typepush:legL2 typepush:legL3 typepush:legL4
#           0        0    -1     1     0              0              0              0

# Question: Is the difference between L2 push and L2 pull significant?
# orange (push) + yellow (push:L2)
(L2push.vs.L2pull = contrast(fit, list(leg="L2", type="push"), list(leg="L2", type="pull")))
L2push.vs.L2pull$X # contrast matrix
# (Intercept) typepush legL2 legL3 legL4 typepush:legL2 typepush:legL3 typepush:legL4
#           0        1     0     0     0              1              0              0

## Testing differences of differences
# Question: Is the difference between push and pull in L3 the same as the difference between push and pull in L2?
# brown (push:L3) - yellow (push:L2)
library(multcomp) # can't use the contrast package, use multcomp package instead!
X = matrix(c(0,0,0,0,0,-1,1,0), 1) # multcomp matrix
# (Intercept) typepush legL2 legL3 legL4 typepush:legL2 typepush:legL3 typepush:legL4
#           0        0     0     0     0             -1              1              0

L2pushpull.vs.L3pushpull = glht(fit, linfct=X)
summary(L2pushpull.vs.L3pushpull)

## Testing all differences of differences
# Question: Is the difference between pull and push the same for all leg pairs?
anova(fit)



###############################################################################

# alternatively, add a group variable that include leg and type to spider
# and generate a model based on this new group variable w/o intercept term
spider %<>% mutate(group = paste0(leg, type))

fit.alt = lm(friction ~ 0 + group, data=spider)
summary(fit.alt)
(coefs = coef(fit.alt))
X.alt = model.matrix(~ 0 + group, data=spider)
colnames(X.alt)
nrow(X.alt)
imagemat(X.alt, main="Model matrix for linear model")


with(spider, stripchart(friction ~ group, vertical=TRUE, pch=1, method="jitter", las=2,
                        main="Comparison of friction coefficients of different groups",
                        xlim=c(0,12),
                        ylim=c(0,2)))

library(RColorBrewer)

cols = brewer.pal(8, "Dark2")

offset = 0.25

abline(h=0)

arrows(1-offset, 0, 1-offset, coefs[1], col=cols[1], length = 0.1)
arrows(2-offset, 0, 2-offset, coefs[2], col=cols[2], length = 0.1)
arrows(3-offset, 0, 3-offset, coefs[3], col=cols[3], length = 0.1)
arrows(4-offset, 0, 4-offset, coefs[4], col=cols[4], length = 0.1)
arrows(5-offset, 0, 5-offset, coefs[5], col=cols[5], length = 0.1)
arrows(6-offset, 0, 6-offset, coefs[6], col=cols[6], length = 0.1)
arrows(7-offset, 0, 7-offset, coefs[7], col=cols[7], length = 0.1)
arrows(8-offset, 0, 8-offset, coefs[8], col=cols[8], length = 0.1)

legend("topright", names(coefs), fill=cols)


# use contrast and multcomp to obtain the comparisons, i.e.,
# differences and differences of differences, respectively

# Question: Is the difference between L2 push and L2 pull significant?
(L2push.vs.L2pull = contrast(fit.alt, list(group="L2push"), list(group="L2pull")))
L2push.vs.L2pull$X # contrast matrix
# groupL1pull groupL1push groupL2pull groupL2push groupL3pull groupL3push groupL4pull groupL4push
#           0           0         -1           1           0           0           0           0

# Question: Is the difference between push and pull in L3 the same as the difference between push and pull in L2?
# = (L3push - L3pull) - (L2push - L2pull)
X = matrix(c(0,0,1,-1,-1,1,0,0), 1) # multcomp matrix
# groupL1pull groupL1push groupL2pull groupL2push groupL3pull groupL3push groupL4pull groupL4push
#           0           0           1          -1          -1           1           0           0
L2pushpull.vs.L3pushpull = glht(fit.alt, linfct=X)
summary(L2pushpull.vs.L3pushpull)
