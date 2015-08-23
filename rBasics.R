library(ISLR)
x <- seq(-pi, pi, length=50)
head(x)
y=x

# The outer product of the arrays X and Y is the array A with dimension c(dim(X), dim(Y)) 
# where element A[c(arrayindex.x, arrayindex.y)] = FUN(X[arrayindex.x], Y[arrayindex.y], ...). 
f=outer(x,y,function(x,y)cos(y)/(1+x^2))

# Contour Plot
contour(x,y,f)
contour(x,y,f,nlevels=45, add=T)
fa=(f-t(f)/2)
contour(x,y,fa,nlevels=15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30, phi=20)
persp(x,y,fa,theta=30, phi=70)
persp(x,y,fa,theta=30, phi=40)
A=matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
dim(A)


library(ISLR)
fix(Auto)
x <-data.frame(Auto)
x
rm(x)


Auto
#Column Names
names(Auto)
Auto=na.omit(Auto)
dim(Auto)

attach(mtcars)
plot(cylinders,mpg)
plot(Auto$cylinders,Auto$mpg)
plot(cylinders,mpg)
plot(Auto$cylinders,Auto$mpg,col="red", data=Auto)
plot(Auto$cylinders,Auto$mpg,col="red", carwidth=T)
plot(Auto$cylinders,Auto$mpg,col="red", varwidth=T)
plot(Auto$cylinders,Auto$mpg,col="red", varwidth=T, horizontal=T)
plot(Auto$cylinders,Auto$mpg,col="red", varwidth=T, xlab="cylinders",ylab="miles per gallon")
plot(Auto$horsepower,Auto$mpg)

# identify reads the position of the graphics pointer when the (first) mouse button is pressed.
identify(Auto$horsepower,Auto$mpg,Auto$name)
dev.off()
summary(Auto)
