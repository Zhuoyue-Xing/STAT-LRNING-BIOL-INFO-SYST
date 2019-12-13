# a
Auto = read.csv("/Users/yangchenye/Downloads/Auto.csv",header=T,na.strings="?") 
dim(Auto)
Auto=na.omit(Auto) # remove incomplete cases
dim(Auto)
names(Auto)
attach(Auto)
# pairs(~ mpg + cylinders + displacement + horsepower + weight + acceleration + year + origin + name, Auto)
pairs(Auto)

# b
CRM = cor(Auto[,1:8])

# c
mlm_fit = lm(mpg~cylinders + displacement + horsepower + weight + acceleration + year + origin)
summary(mlm_fit)

# d
mlm_fit_log = lm(log10(mpg)~log10(cylinders) + log10(displacement) +log10(horsepower) + log10(weight) + log10(acceleration) + log10(year) + log10(origin))
summary(mlm_fit_log)

mlm_fit_sqrt = lm(sqrt(mpg)~sqrt(cylinders) + sqrt(displacement) +sqrt(horsepower) + sqrt(weight) + sqrt(acceleration) + sqrt(year) + sqrt(origin))
summary(mlm_fit_sqrt)

mlm_fit_power = lm(mpg^2~cylinders^2 + displacement^2 + horsepower^2 + weight^2 + acceleration^2 + year^2 + origin^2)
summary(mlm_fit_power)
