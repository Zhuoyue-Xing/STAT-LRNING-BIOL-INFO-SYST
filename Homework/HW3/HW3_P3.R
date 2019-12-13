beta_old = c(0,0)
beta_new = c(0,0)
# initiate X
X = matrix(data = rep(0,12), nrow = 2, ncol = 6)
for (i in 1:6){
  X[,i] = c(1,(i-1)/5)
}
# initiate y
y = c(0, 0, 0, 1, 0, 1)
# initiate derivatives
l_b = c(0, 0)
for (i in 1:6){
  l_b = l_b + X[,i]*(y[i]-exp(X[,i] %*% beta_old)/(1+exp(X[,i] %*% beta_old)))
}
# initiate second derivatives
l_bb = matrix(data = rep(0,4), nrow = 2, ncol = 2)
for (i in 1:6){
  l_bb = l_bb - (X[,i] %*% t(X[,i]))*((exp(X[,i] %*% beta_old)/(1+exp(X[,i] %*% beta_old)))*(1-(exp(X[,i] %*% beta_old)/(1+exp(X[,i] %*% beta_old)))))[1]
}
# iteration
for (i in 1:10){
  beta_new = t(t(beta_old)) - solve(l_bb) %*% t(t(l_b))
  beta_old = t(t(beta_new))
}
t(t(beta_new))

