# a
set.seed(1) # ensure consistent results
x=rnorm(100, mean=0, sd=1) # feature X

# b
eps=rnorm(100, mean=0, sd=1)

# c
y = -1 + 0.5*x + eps
length(y)

# d scatterplot
plot(x, y, xlab="X", ylab="Y", main="Relationship between X and Y")

# e least squares linear model
lm.fit = lm(y~x)
confint(lm.fit)
summary(lm.fit)
plot(x, y, xlab="X", ylab="Y", main="Relationship between X and Y")
abline(lm.fit)

# f 
par(col='black')
plot(x, y, xlab="X", ylab="Y", main="Relationship between X and Y")
abline(lm.fit, col='blue', lty=5) # least squares line
abline(a=-1, b=0.5, col='red', lty=1) # population regression line
legend('topleft', inset=0.05, c('least squares line', 'population regression line'), lty=c(5, 1), col=c('blue', 'red'), bty = "o")

# g
# polynomial regression
Poly_fit = lm(y ~ poly(x,2))
# show regression result
summary(Poly_fit)
# creat points to draw fitting line
xlims = range(x)
x.grid = seq(from=xlims[1], to=xlims[2])
preds = predict(Poly_fit, newdata=list(x=x.grid), se=TRUE)
# use standard error to creat wrapper line
se.bands = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
# draw 
plot(x, y, xlab="X", ylab="Y", main='Degree-2 Polynomial', col="black")
lines(x.grid, preds$fit, lwd=2, col="blue", lty=1)
matlines(x.grid, se.bands, lwd=1, col="red", lty=5)
legend('topleft', inset=0.05, c('polynomial regression line', 'SE wrapper line'), lwd=c(2, 1), lty=c(1, 5), col=c('blue', 'red'), bty = "o")
