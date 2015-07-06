

library(boot); library(ISLR)

cv.error=rep(0,5)
for ( i in 1:5){
  glm.fit = glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}

cv.error


set.seed(17)
cv.error.10=rep(0, 10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit,K=10)$delta[1]
}
cv.error.10

# bootstrapping

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)- cov(X,Y))/var(X)+var(Y)-2*cov(X,Y))
}

alpha.fn(Portfolio,1:100)


set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))

boot(Portfolio,alpha.fn,R=1000)

boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto, sample(392,392,replace=T))


boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower, data=Auto))$coef


boot.fn=function(data,index)
coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef
