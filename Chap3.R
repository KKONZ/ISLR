library(MASS); library(ISLR)
fix(Boston)

attach(Boston)
names(Boston)
?Boston


attach(Boston)
lm.fit=lm(medv~lstat)
summary(lm.fit)

names(lm.fit)
coef(lm.fit)
# produce confidence intervals
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")

# prediction interval (should be wider)
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")

# plot medv and lstat with least squares regression line
plot(lstat,medv)
abline(lm.fit)

# additional plot settings:
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)


# Studentized residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# leverage statistics of predictors
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


# multiple predictors 
lm.fit=lm(medv ~ lstat+age, data=Boston)
summary(lm.fit)



lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
?summary.lm

# gives us R^2
summary(lm.fit)$r.sq

# gives us RSE
summary(lm.fit)$sigma



library(car)
vif(lm.fit)


# The following uses a regressing using all predictors except age
lm.fit1=lm(medv~., -age, data=Boston)
summary(lm.fit1)

# alternatively use update
lm.fit1=update(lm.fit, ~.-age)
summary(lm.fit1)



# interaction terms
summary(lm(medv~lstat*age, data=Boston))


# non-linear transformations of the predictors
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

# near zero value for p associated with the quadratic term suggests we need to 
# improve the model

# Anova shows that the model with lstat^2 is far superior; f stat 135.2 and nzv for p
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

# See that lstat^2 included yeilds little discernible pattern in the residuals
par(mfrow=c(2,2))
plot(lm.fit2)


# in order to create a cubic fit with either I(X^3) or for high order polynomials
# use poly()
# create fifth order poly
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

# output suggests that additional polynomial terms, up to fifth order leads to an improvement in the model fit

# Also try a log transformation of predictors
summary(lm(medv~log(rm),data=Boston))
