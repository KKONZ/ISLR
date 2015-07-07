library(ISLR); library(leaps)
fix(Hitters)
names(Hitters)

dim(Hitters)
sum(is.na(Hitters$Salary))


Hitters=na.omit(Hitters)
dim(Hitters)


regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)


regfit.full=regsubsets(Salary~.,data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)

names(reg.summary)


# plotting rss, adjusted R^2, Cp, and BIC for all the models at once helps 
# choose an appropriate model

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type = "l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")


which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red", cex=2,pch=20)

plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp",
     type="l")
which.min (reg.summary$cp )
points (10, reg.summary$cp [10], col ="red",cex =2, pch =20)
which.min (reg.summary$bic )
plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC",
     type="l")
points (6, reg.summary$bic [6], col =" red",cex =2, pch =20)



# regsubsets has built in plotting function

plot(regfit.full ,scale ="r2")
plot(regfit.full ,scale ="adjr2")
plot(regfit.full ,scale ="Cp")
plot(regfit.full ,scale ="bic")

coef(regfit.full,6)

# forward and backward stepwise selection
regfit.fwd=regsubsets (Salary∼.,data=Hitters ,nvmax =19,
                        method ="forward")
summary(regfit.fwd)
plot(regfit.fwd, scale="r2")
plot(regfit.fwd ,scale ="bic")


regfit.bwd=regsubsets(Salary∼.,data=Hitters ,nvmax =19,
                        method ="backward")
summary(regfit.bwd)


# models using forward stepwise selection, backwards stepwise selection, and best subset selection 
# are different

coef(regfit.full ,7)
coef(regfit.fwd ,7)
coef(regfit.bwd ,7)


# choose among models using the validation set approach and cross validation

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters), replace=T)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,], nvmax=19)
test.mat=model.matrix(Salary~., data=Hitters[test,])

val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

# find that the best model is the one that contains nine/ten variables
val.errors
which.min(val.errors)
coef(regfit.best,9)

# That was a slightly tedious route, partially because there is no predict()
# function for regsubsets

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}


regfit.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(regfit.best,10)


# choose among the models of different sizes using cross validation, 
# subset selection within each of the k training sets.

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters), replace =TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

# loop perfoming cross-validation
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],
                     nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type="b")


# We see that cross validation selects an 11 variable modle
reg.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best,11)



# model.matrix() automatically transforms qualitative variables into dummy variables
# and produces a matrix corresponding to the 19 predictors

# glmnet only takes quantitative variables
library(glmnet)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# in glmnet if alpha =0 then ridge regression is fit,
# if alpha = 1 then lasso was fit

# by default glmnet performs ridge regression for an automatically selected range of 
# lambda values

# however here we have chosen to implement the function over a grid of values raning from 
# lambda = 10^10 to lambda = 10^-2

# by default glmnet standardizes variables
# can be turned off with standardize=FALSE
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

# associated with each value of lambda is a vector of ridge regression coefficients,
# stored in a matrix that can be accessed by coef()

dim(coef(ridge.mod))
coef(ridge.mod)



ridge.mod$lambda[50]
coef(ridge.mod)[,50]


ridge.mod$lambda[60]
coef(ridge.mod)[,60]

sqrt(sum(coef(ridge.mod)[-1,60]^2))


predict(ridge.mod,s=50,type="coefficients")[1:20,]

# another way to split data into training/testing
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,
                 thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)


mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y[test]^2))
     
lm(y~x, subset =train)
predict(ridge.mod,s=0, exact=T,type="coefficient")[1:20,]
     
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
