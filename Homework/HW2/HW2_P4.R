# (a)
set.seed(1)
X = rnorm(100, mean = 0, sd = 1)
e = rnorm(100, mean = 0, sd = 1)
# (b)
beta0 = 1
beta1 = 2
beta2 = 3
beta3 = 5
Y = beta0 + beta1*X + beta2*X^2 + beta3*X^3 + e
# (c)
# create a single data set containing both X and Y 
simulatedData = data.frame(
  response = Y,
  X_1 = X,
  X_2 = X^2,
  X_3 = X^3,
  X_4 = X^4,
  X_5 = X^5,
  X_6 = X^6,
  X_7 = X^7,
  X_8 = X^8,
  X_9 = X^9,
  X_10 = X^10,
  stringsAsFactors = FALSE
)
str(simulatedData) # Get the structure of the data frame.
library(leaps)
# perform best subset selection, using 10 predictors
regfit.full = regsubsets(response~.,simulatedData,nvmax = 10)
reg.summary = summary(regfit.full)
# RSS
plot(reg.summary$rss,main="Best subset selection",xlab="Number of Variables",ylab="RSS",type = "l")
# Adjusted RSq
plot(reg.summary$adjr2,main="Best subset selection",xlab="Number of Variables",ylab="Adjusted RSq",type = "l")
abline(v=which.max(reg.summary$adjr2), lty=2, col="red")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="red")
coef(regfit.full,which.max(reg.summary$adjr2))
# Cp
plot(reg.summary$cp,main="Best subset selection",xlab="Number of Variables",ylab="Cp",type = "l")
abline(v=which.min(reg.summary$cp), lty=2, col="red")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red")
coef(regfit.full,which.min(reg.summary$cp))
# BIC
plot(reg.summary$bic,main="Best subset selection",xlab="Number of Variables",ylab="BIC",type = "l")
abline(v=which.min(reg.summary$bic), lty=2, col="red")
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red")
coef(regfit.full,which.min(reg.summary$bic))

# (d)
# perform forward stepwise selection, using 10 predictors
regfit.fwd = regsubsets(response~.,simulatedData,nvmax = 10,method = "forward")
reg.summary = summary(regfit.fwd)
# RSS
plot(reg.summary$rss,main="Forward stepwise selection",xlab="Number of Variables",ylab="RSS",type = "l")
# Adjusted RSq
plot(reg.summary$adjr2,main="Forward stepwise selection",xlab="Number of Variables",ylab="Adjusted RSq",type = "l")
abline(v=which.max(reg.summary$adjr2), lty=2, col="red")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="red")
coef(regfit.fwd,which.max(reg.summary$adjr2))
# Cp
plot(reg.summary$cp,main="Forward stepwise selection",xlab="Number of Variables",ylab="Cp",type = "l")
abline(v=which.min(reg.summary$cp), lty=2, col="red")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red")
coef(regfit.fwd,which.min(reg.summary$cp))
# BIC
plot(reg.summary$bic,main="Forward stepwise selection",xlab="Number of Variables",ylab="BIC",type = "l")
abline(v=which.min(reg.summary$bic), lty=2, col="red")
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red")
coef(regfit.fwd,which.min(reg.summary$bic))
# perform backward stepwise selection, using 10 predictors
regfit.bwd = regsubsets(response~.,simulatedData,nvmax = 10,method = "backward")
reg.summary = summary(regfit.bwd)
# RSS
plot(reg.summary$rss,main="Backward stepwise selection",xlab="Number of Variables",ylab="RSS",type = "l")
# Adjusted RSq
plot(reg.summary$adjr2,main="Backward stepwise selection",xlab="Number of Variables",ylab="Adjusted RSq",type = "l")
abline(v=which.max(reg.summary$adjr2), lty=2, col="red")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="red")
coef(regfit.bwd,which.max(reg.summary$adjr2))
# Cp
plot(reg.summary$cp,main="Backward stepwise selection",xlab="Number of Variables",ylab="Cp",type = "l")
abline(v=which.min(reg.summary$cp), lty=2, col="red")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red")
coef(regfit.bwd,which.min(reg.summary$cp))
# BIC
plot(reg.summary$bic,main="Backward stepwise selection",xlab="Number of Variables",ylab="BIC",type = "l")
abline(v=which.min(reg.summary$bic), lty=2, col="red")
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red")
coef(regfit.bwd,which.min(reg.summary$bic))
# (e)
library("glmnet")
x_lasso = model.matrix(response~., simulatedData)[,-1]
y_lasso = simulatedData$response
# alpha=1 is assumed by default, Lasso: Alpha = 1
cv.lasso <- cv.glmnet(x_lasso, y_lasso, nfolds = 10, parallel=TRUE, standardize=TRUE, type.measure='mse')
# plot(cv.lasso)
plot(cv.lasso$lambda, cv.lasso$cvm, main=expression(paste("Select ",lambda, ' with cross validation')),xlab=expression(lambda),ylab="Mean Cross-Validation Error", type = 'l')
abline(v=cv.lasso$lambda.min, lty=2, col="red")
points(cv.lasso$lambda.min, cv.lasso$cvm[which(cv.lasso$lambda==cv.lasso$lambda.min)],col="red")
plot(cv.lasso$glmnet.fit, xvar="lambda", main=expression(paste("Coefficients changes with ",lambda)),xlab=expression(paste('log ',lambda)),ylab="Coefficients of predictors")
abline(v=log(cv.lasso$lambda.min), lty=2, col="red")
cv.lasso$lambda.min
# cv.lasso$lambda.1se
coef(cv.lasso, s=cv.lasso$lambda.min)

# (f)
beta0 = 1
beta7 = 13
Y = beta0 + beta7*X^7 + e
# best subset selection
# create a single data set containing both X and Y 
simulatedData = data.frame(
  response = Y,
  X_1 = X,
  X_2 = X^2,
  X_3 = X^3,
  X_4 = X^4,
  X_5 = X^5,
  X_6 = X^6,
  X_7 = X^7,
  X_8 = X^8,
  X_9 = X^9,
  X_10 = X^10,
  stringsAsFactors = FALSE
)
str(simulatedData) # Get the structure of the data frame.
library(leaps)
# perform best subset selection, using 10 predictors
regfit.full = regsubsets(response~.,simulatedData,nvmax = 10)
reg.summary = summary(regfit.full)
# RSS
plot(reg.summary$rss,main="Best subset selection",xlab="Number of Variables",ylab="RSS",type = "l")
# Adjusted RSq
plot(reg.summary$adjr2,main="Best subset selection",xlab="Number of Variables",ylab="Adjusted RSq",type = "l")
abline(v=which.max(reg.summary$adjr2), lty=2, col="red")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="red")
coef(regfit.full,which.max(reg.summary$adjr2))
# Cp
plot(reg.summary$cp,main="Best subset selection",xlab="Number of Variables",ylab="Cp",type = "l")
abline(v=which.min(reg.summary$cp), lty=2, col="red")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red")
coef(regfit.full,which.min(reg.summary$cp))
# BIC
plot(reg.summary$bic,main="Best subset selection",xlab="Number of Variables",ylab="BIC",type = "l")
abline(v=which.min(reg.summary$bic), lty=2, col="red")
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red")
coef(regfit.full,which.min(reg.summary$bic))

# lasso
library("glmnet")
x_lasso = model.matrix(response~., simulatedData)[,-1]
y_lasso = simulatedData$response
cv.lasso <- cv.glmnet(x_lasso, y_lasso, nfolds = 10, parallel=TRUE, standardize=TRUE, type.measure='mse')
# plot(cv.lasso)
plot(cv.lasso$lambda, cv.lasso$cvm, main=expression(paste("Select ",lambda, ' with cross validation')),xlab=expression(lambda),ylab="Mean Cross-Validation Error", type = 'l')
abline(v=cv.lasso$lambda.min, lty=2, col="red")
points(cv.lasso$lambda.min, cv.lasso$cvm[which(cv.lasso$lambda==cv.lasso$lambda.min)],col="red")
plot(cv.lasso$glmnet.fit, xvar="lambda", main=expression(paste("Coefficients changes with ",lambda)),xlab=expression(paste('log ',lambda)),ylab="Coefficients of predictors")
abline(v=log(cv.lasso$lambda.min), lty=2, col="red")
cv.lasso$lambda.min
# cv.lasso$lambda.1se
coef(cv.lasso, s=cv.lasso$lambda.min)


