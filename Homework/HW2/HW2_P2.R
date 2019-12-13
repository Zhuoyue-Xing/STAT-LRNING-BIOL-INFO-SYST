# (a)
y1 = 1
lambda = 0.5
beta1 = seq(from=-1, to=3, by=0.1)
f = (y1 - beta1)^2 + lambda*(beta1^2)
plot(beta1, f, type="l", lwd=2, xlab=expression(paste(beta, '1')), ylab=expression(paste('f(',beta, '1)')),main=expression(paste("ridge regression as a function of ",beta, '1')))
abline(v=y1/(1+lambda), lty=2, col="red")
beta1 = y1/(1+lambda)
f = (y1 - beta1)^2 + lambda*(beta1^2)
points(beta1, f, col="red")

# (b)
# y1 > lambda/2
y1 = 1
lambda = 0.5
beta1 = seq(from=-1, to=3, by=0.1)
g = (y1 - beta1)^2 + lambda*(abs(beta1))
plot(beta1, g, type="l", lwd=2, xlab=expression(paste(beta, '1')), ylab=expression(paste('g(',beta, '1)')),main=expression(paste("lasso as a function of ",beta, '1')))
abline(v=y1-lambda/2, lty=2, col="red")
beta1 = y1-lambda/2
g = (y1 - beta1)^2 + lambda*(abs(beta1))
points(beta1, g, col="red")
# y1 < -lambda/2
y1 = -1
lambda = 0.5
beta1 = seq(from=-3, to=1, by=0.1)
g = (y1 - beta1)^2 + lambda*(abs(beta1))
plot(beta1, g, type="l", lwd=2, xlab=expression(paste(beta, '1')), ylab=expression(paste('g(',beta, '1)')),main=expression(paste("lasso as a function of ",beta, '1')))
abline(v=y1+lambda/2, lty=2, col="red")
beta1 = y1+lambda/2
g = (y1 - beta1)^2 + lambda*(abs(beta1))
points(beta1, g, col="red")
# |y1| <= lambda/2
y1 = 0.1
lambda = 0.5
beta1 = seq(from=-2, to=2, by=0.1)
g = (y1 - beta1)^2 + lambda*(abs(beta1))
plot(beta1, g, type="l", lwd=2, xlab=expression(paste(beta, '1')), ylab=expression(paste('g(',beta, '1)')),main=expression(paste("lasso as a function of ",beta, '1')))
abline(v=0, lty=2, col="red")
beta1 = 0
g = (y1 - beta1)^2 + lambda*(abs(beta1))
points(beta1, g, col="red")

