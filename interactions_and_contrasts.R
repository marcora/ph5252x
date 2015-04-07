library(dplyr)
library(magrittr)

spider = read.csv("spider_wolff_gorb_2013.csv", skip=1)

with(spider, boxplot(friction ~ type + leg, col=c("grey90", "grey45"), las=2,
                     main="Comparison of friction coefficients of different leg pairs"))

#################################
# let's start with just one leg #
#################################

spider.sub = filter(spider, leg == "L1")

fit = lm(friction ~ type, data=spider.sub)
summary(fit)
(coefs = coef(fit))

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
                        main="Comparison of friction coefficients of different leg pairs",
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
                            main="Comparison of friction coefficients of different leg pairs",
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

# A contrast is a combination of coefficient
library(contrast)

(L3vsL2 = contrast(fit, list(leg="L3", type="pull"), list(leg="L2", type="pull")))


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
head(X)

imagemat(X, main="Model matrix for linear model")

with(spider, stripchart(friction ~ type + leg, vertical=TRUE, pch=1, method="jitter", las=2,
                        main="Comparison of friction coefficients of different leg pairs",
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

