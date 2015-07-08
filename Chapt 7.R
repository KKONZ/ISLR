# Chapter 7 Lab: Non-linear Modeling

library(ISLR)
attach(Wage)

# Polynomial Regression and Step Functions

# poly command is for polynomial
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

# using raw does not affect the model in a meaningful way
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))

# This creates the polynomial basis function on the fly, taking
# care to protect terms like age^2 with wrapper function I() 
# the ^ symbol has a special meaning in formulas
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)

# essentially do the same thing with cbind
fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
summary(fit2b)


# create a grid of values for age at which we want predictions and then call the generic predict()
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# Plot the Degree-4 polynomial
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


# orthongonal set of basis functions produced in the poly() function
# will not affect the model in a meaningful way
preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))

# Must decide degree of polynomial to use, one way to do so is using hypothesis testing

# Fit models from linear to 5 degree polynomial and determine the simplest model which 
# is sufficient to explain the relationship between wage and age with ANOVA(), f-test
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)

# notice that the p value is too low for upto model 3 then models 3/4 (cubic, degree-4 poly)
# are sufficient, while model 5 is unecessary because its p-value is 5%
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

# instead of using ANOVA() we could have obtained the p-values more 
# succinctly by exploiting poly() creates orthogonal polynomials
coef(summary(fit.5))

# notice that the p-values are the same
# and that the square of the t -stats are equal to the F-stats from the anova()
(-11.983)^2


# however the ANOVA() method works whether we used orthogonal polynomials or not.
# For example:
fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)
# alternatively we could choose the polynomial degree using cross-validation 

# fit a polynomial logisitic regression for considering the task of predicting
# wheter individual earns more than 250,000 per year.
# use I() to create the response variable on the fly, the expression wage>250
# evaluates to a logical variable containing Ts and Fs, which glm() coerces to binary
fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)


preds=predict(fit,newdata=list(age=age.grid),se=T)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

# could have computed the probabilities directly by using type="response" option in predict()
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)
# however the corresponding CI would not be sensible as we would end up with negative probabilites


# create right hand plot
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# Use cut to fit a stepwise model
# automatically picks cutpoints of 33.5, 49, and 64.5 years of age
table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

# Splines

library(splines)
# bs() function generates entire matrix of basis functions for splines with specified set of knots
# by default cubic lines are produced
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")
# Here we have prespecified knots at ages 25, 40, 60
# produces 6 basis functions. (cubic spline with 3 knots has 7 DFs)

# also use df option to create spline with knots at uniform quantiles of the data
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")

# use the ns() function for a natural spline, here we have 4 degrees of greedom
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)


# Also could specify the knots directly using knots options

# fit smoothing spline, here with 16 DFs, the function then determines which value of 
# lambda leadsto 16 DFs freedom
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)


# local regression
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# GAMs

# predict wage using natural spline of year and age, treating education as a qualitative predictor
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)


library(gam)
# using smoothing splines or other componenets taht cannot be expressed in terms
# of basis functions and then fit using least sq we need gam

# the s() function, is used to indicate that we would like to use a smoothing spline
# here we have specified that year should have 4 degrees of freedom.
# Since education is qulalitative, we leave it as is and it is converted to four dummy variables.
gam.m3=gam(wage∼s(year ,4)+s(age ,5)+education ,data=Wage)
# all of ther terms are fit simultaneously taking each other into account to explain the response
par(mfrow=c(1,3))



plot(gam.m3, se=TRUE,col="blue")
# generic plot() for gam.me is an object of class gam, and invokes the appropriate plot.gam() method
plot.gam(gam1, se=TRUE, col="red")

# gam1 is not a class of gam but rather of lm(), we can still use plot.gam():


# Then we perform a series of ANOVA tests in order to determine which of these three models is best:
# gam exluding year
# gam LM of year
# gam spline function of year
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
# find compelling evidence that a GAM with LM of year is better than one that does not include year
# (p-value .00014)

# however, no evidence that a non-linear function of year is needed (p-value=.349)
summary(gam.m3)

# re-enforces p -value large for year that a linear function is adequate for this term
# however non-linear term is required for age
preds=predict(gam.m2,newdata=Wage)

# local regression using lo() function
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
plot.gam(gam.lo, se=TRUE, col="green")
# fits a 2 term model in which the first term is an interaction between year and age, fit by a local regression
gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)

library(akima)
plot(gam.lo.i)

# In order to fit a logistic regression GAM, we once again use I() function in 
# constructing the binary response variable, and set family=binomial
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

# easy to see that there are no high earners in the < HS cat.
table(education,I(wage>250))

# Hence we fit a logistic regression GAM using all but this cat:
gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")
