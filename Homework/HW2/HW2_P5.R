# (a)
rm(list=ls())
set.seed(1)
College = read.csv("/Users/yangchenye/Downloads/College.csv",header=T,na.strings="?") 
dim(College)
College=na.omit(College) # remove incomplete cases
dim(College)
names(College)

# 75% of the sample size
smp_size = floor(0.75 * nrow(College))
train_ind = sample(seq_len(nrow(College)), size = smp_size)
# select train data and test data, drop the name 'X' of College
train = subset(College[train_ind, ],select = -X)
test = subset(College[-train_ind, ],select = -X)

# (b)
train_fit = lm(train$Grad.Rate~., data = train)
summary(train_fit)
test_lm_predict = predict(train_fit, test[-18]) # predict
test_lm_MSE = mean((test$Grad.Rate - test_lm_predict) ^ 2) # test MSE

# (c) ridge
library("glmnet")
x_ridge = model.matrix(train$Grad.Rate~., data = train)[,-1]
y_ridge = train$Grad.Rate
# Ridge: Alpha = 0
cv.ridge = cv.glmnet(x_ridge, y_ridge, alpha=0, nfolds = 10, parallel=TRUE, standardize=TRUE, type.measure='mse')
# CVE~lambda
plot(cv.ridge$lambda, cv.ridge$cvm, main=expression(paste("Select ",lambda, ' with cross validation')),xlab=expression(lambda),ylab="Mean Cross-Validation Error", type = 'l')
abline(v=cv.ridge$lambda.min, lty=2, col="red")
points(cv.ridge$lambda.min, cv.ridge$cvm[which(cv.ridge$lambda==cv.ridge$lambda.min)],col="red")
plot(cv.ridge$glmnet.fit, xvar="lambda", main=expression(paste("Coefficients changes with ",lambda)),xlab=expression(paste('log ',lambda)),ylab="Coefficients of predictors")
abline(v=log(cv.ridge$lambda.min), lty=2, col="red")
# best lambda
cv.ridge$lambda.min
coef(cv.ridge, s=cv.ridge$lambda.min)
# best ridge regression with best lambda
ridge_best = glmnet(x_ridge, y_ridge, alpha=0, lambda = cv.ridge$lambda.min, standardize=TRUE)
test_ridge_predict = predict(ridge_best, model.matrix(test$Grad.Rate~., data = test)[,-1], s="lambda.min") # predict
test_ridge_MSE = mean((test$Grad.Rate - test_ridge_predict) ^ 2) # test MSE

# (d) lasso
library("glmnet")
x_lasso = model.matrix(train$Grad.Rate~., data = train)[,-1]
y_lasso = train$Grad.Rate
# Lasso: Alpha = 1
cv.lasso = cv.glmnet(x_lasso, y_lasso, alpha=1, nfolds = 10, parallel=TRUE, standardize=TRUE, type.measure='mse')
# CVE~lambda
plot(cv.lasso$lambda, cv.lasso$cvm, main=expression(paste("Select ",lambda, ' with cross validation')),xlab=expression(lambda),ylab="Mean Cross-Validation Error", type = 'l')
abline(v=cv.lasso$lambda.min, lty=2, col="red")
points(cv.lasso$lambda.min, cv.lasso$cvm[which(cv.lasso$lambda==cv.lasso$lambda.min)],col="red")
plot(cv.lasso$glmnet.fit, xvar="lambda", main=expression(paste("Coefficients changes with ",lambda)),xlab=expression(paste('log ',lambda)),ylab="Coefficients of predictors")
abline(v=log(cv.lasso$lambda.min), lty=2, col="red")
# best lambda
cv.lasso$lambda.min
cv.lasso$nzero[which(cv.lasso$lambda==cv.lasso$lambda.min)]
coef(cv.lasso, s=cv.lasso$lambda.min)
# best ridge regression with best lambda
lasso_best = glmnet(x_lasso, y_lasso, alpha=0, lambda = cv.lasso$lambda.min, standardize=TRUE)
test_lasso_predict = predict(lasso_best, model.matrix(test$Grad.Rate~., data = test)[,-1], s="lambda.min") # predict
test_lasso_MSE = mean((test$Grad.Rate - test_lasso_predict) ^ 2) # test MSE







