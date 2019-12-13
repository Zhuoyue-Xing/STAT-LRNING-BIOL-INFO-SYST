# (a)
rm(list=ls())
set.seed(1)
p = 20
n = 1000
X = rnorm(n, mean = 0, sd = 1)
e = rnorm(n, mean = 0, sd = 1)
c = seq(from = 1, to = 20, length.out = p)
beta = rep(0, p)
beta0 = 1
beta[1] = 2
beta[2] = 3
beta[3] = 5
Y = beta0 + beta[1]*X + beta[2]*X^2 + beta[3]*X^3 + e

# (b)
simulatedData = data.frame(
  response = Y,
  X_1 = X^c[1],
  X_2 = X^c[2],
  X_3 = X^c[3],
  X_4 = X^c[4],
  X_5 = X^c[5],
  X_6 = X^c[6],
  X_7 = X^c[7],
  X_8 = X^c[8],
  X_9 = X^c[9],
  X_10 = X^c[10],
  X_11 = X^c[11],
  X_12 = X^c[12],
  X_13 = X^c[13],
  X_14 = X^c[14],
  X_15 = X^c[15],
  X_16 = X^c[16],
  X_17 = X^c[17],
  X_18 = X^c[18],
  X_19 = X^c[19],
  X_20 = X^c[20],
  stringsAsFactors = FALSE
)
str(simulatedData) # Get the structure of the data frame.
# training set containing 100 observations, 1/10
smp_size = floor(0.1 * nrow(simulatedData))
train_ind = sample(seq_len(nrow(simulatedData)), size = smp_size)
# select train data and test data
train = simulatedData[train_ind, ]
# test = simulatedData[-train_ind, ]
test = model.matrix(~., data=simulatedData[-train_ind, ])
# test = as.data.frame(test)

# (c)
library(leaps)
# perform best subset selection, using 20 predictors
regfit.full = regsubsets(response~.,simulatedData,nvmax = p)
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

# the training set MSE associated with the best model of each size
plot(regfit.full$rss[-1]/n,main="Training MSE", xlab="Number of predictors", ylab = "MSE", col = "blue", type = "l", lty=2)
legend("topright", legend = "Training", col = "blue", lty=2)
which.min(regfit.full$rss[-1]/n)

# (d)
# Plot the test set MSE associated with the best model of each size
test.full.MSE = rep(NA, p) # test set MSE
for (i in 1:p) {
  coefi = coef(regfit.full, id = i)
  pred = test[, names(coefi)] %*% coefi
  test.full.MSE[i] = mean((test[, "response"] - pred)^2)
}
plot(test.full.MSE, main="Testing MSE", xlab="Number of predictors", ylab = "MSE", col = "red", type = "l", lty=1)
legend("topright", legend = "Testing", col = "red", lty=1)

# (e)
which.min(test.full.MSE)
coef(regfit.full, id = which.min(test.full.MSE))

# (g)
y = rep(NA, p)
for (r in 1:p){
  temp = 0
  coefi = coef(regfit.full,id=r) # intercept, X_1, X_2,...
  for (j in names(coefi)[-1]){
    num = type.convert(substr(j,3,4), 'int')
    temp = temp + (beta[num] - coefi[[j]])^2
  }
  y[r] = temp
}
y = sqrt(y)

plot(y, xlab="Number of predictors", ylab = "y", col = "black", type = "l", lty=1)

