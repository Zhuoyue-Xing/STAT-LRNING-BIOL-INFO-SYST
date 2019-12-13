Advertising = read.csv("/Users/yangchenye/Downloads/Advertising.csv",header=T,na.strings="?") 
dim(Advertising)
Advertising=na.omit(Advertising) # remove incomplete cases
dim(Advertising)
names(Advertising)
attach(Advertising)

# TV
TV_fit = lm(sales~TV) # least squares model
TV_CI = confint(TV_fit, level = 0.92) # 92% confidence intervals

plot(TV, sales, xlab="TV advertising", ylab="Sales", main="Relationship between Sales and TV advertising") # scatterplot
abline(TV_fit, col='blue', lty=1) # least squares line
abline(a=TV_CI[1,1], b=TV_CI[2,1], col='red', lty=5) # 92% confidence intervals line
abline(a=TV_CI[1,2], b=TV_CI[2,2], col='red', lty=5) # 92% confidence intervals line
legend('topleft', inset=0.05, c('least squares line', '92% confidence intervals line'), lty=c(1, 5), col=c('blue', 'red'), bty = "o")

# Radio
Radio_fit = lm(sales~radio) # least squares model
Radio_CI = confint(Radio_fit, level = 0.92) # 92% confidence intervals

plot(radio, sales, xlab="Radio advertising", ylab="Sales", main="Relationship between Sales and Radio advertising") # scatterplot
abline(Radio_fit, col='blue', lty=1) # least squares line
abline(a=Radio_CI[1,1], b=Radio_CI[2,1], col='red', lty=5) # 92% confidence intervals line
abline(a=Radio_CI[1,2], b=Radio_CI[2,2], col='red', lty=5) # 92% confidence intervals line
legend('topleft', inset=0.05, c('least squares line', '92% confidence intervals line'), lty=c(1, 5), col=c('blue', 'red'), bty = "o")

# Newspaper
Newspaper_fit = lm(sales~newspaper) # least squares model
Newspaper_CI = confint(Newspaper_fit, level = 0.92) # 92% confidence intervals

plot(newspaper, sales, xlab="Newspaper advertising", ylab="Sales", main="Relationship between Sales and Newspaper advertising") # scatterplot
abline(Newspaper_fit, col='blue', lty=1) # least squares line
abline(a=Newspaper_CI[1,1], b=Newspaper_CI[2,1], col='red', lty=5) # 92% confidence intervals line
abline(a=Newspaper_CI[1,2], b=Newspaper_CI[2,2], col='red', lty=5) # 92% confidence intervals line
legend('bottomright', inset=0.05, c('least squares line', '92% confidence intervals line'), lty=c(1, 5), col=c('blue', 'red'), bty = "o")

# pairs(~ TV + radio + newspaper + sales, Advertising)