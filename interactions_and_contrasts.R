library(dplyr)
library(magritts)

spider = read.csv("spider_wolff_gorb_2013.csv", skip=1)
with(spider, boxplot(friction ~ type + leg, col=c("grey90", "grey45"),
                     main="Comparison of friction coefficients of different leg pairs"))


spider$group = factor(paste0(spider$leg, spider$type))

# let's start with just one leg
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

# library(devtools); install_github("ririzarr/rafalib")
library(rafalib)
imagemat(X, main="Model matrix for linear model")

# let's condition on leg
fit = lm(friction ~ type + leg, data=spider)
summary(fit)
(coefs = coef(fit))

X = model.matrix(~ type + leg, data=spider)
colnames(X)

imagemat(X, main="Model matrix for linear model")

stripchart(split(spider$friction, spider$group), vertical=TRUE, pch=1, method="jitter", xlim=c(0,11), ylim=c(0,2), las=2)

library(RColorBrewer)

cols = brewer.pal(5, "Dark2")

offset = 0.25

abline(h=0)

abline(h=coefs[1], col=cols[1])
arrows(1-offset,0, 1-offset, coefs[1], col=cols[1], length = 0.1)

legend("topright", names(coefs), fill=cols)

