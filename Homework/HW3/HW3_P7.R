# (a)
rm(list=ls())
set.seed(1000)
load('/Users/yangchenye/Downloads/OJ.rda')
dim(OJ)
OJ=na.omit(OJ) # remove incomplete cases
dim(OJ)
names(OJ)

# 800: the sample size
smp_size = floor(800)
train_ind = sample(seq_len(nrow(OJ)), size = smp_size)
# select train data and test data
train = subset(OJ[train_ind, ])
test = subset(OJ[-train_ind, ])

# (b)
library(tree)
tree.fit = tree(train$Purchase~., data=train)
summary(tree.fit)
# (c)
tree.fit
# (d)
plot(tree.fit)
text(tree.fit,pretty=0)
# (e)
tree.pred = predict(tree.fit, test, type="class")
with(test, table(tree.pred, Purchase))
# (f)
cv.fit = cv.tree(tree.fit)
cv.fit
cv.fit$size[which.min(cv.fit$dev)]
# (g)
par(mfrow = c(1, 2))
# default plot
plot(cv.fit)
# better plot
plot(cv.fit$size, cv.fit$dev / smp_size, type = "b",
     xlab = "Tree Size", ylab = "CV Classification Error Rate")
# (h)
cv.best = cv.fit$size[which.min(cv.fit$dev / smp_size)] # misclassification rate of each tree
cv.best
# (i)
prune.fit = prune.tree(tree.fit, best = 5)
plot(prune.fit)
text(prune.fit, pretty=0)
# (j)
summary(prune.fit)
# (k)
tree.pred = predict(prune.fit, test, type="class")
with(test, table(tree.pred, Purchase))



