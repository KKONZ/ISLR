# Smarket data.

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)


cor(Smarket[,-9])
attach(Smarket)
plot(Volume)



# logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4 +Lag5 + Volume, family =binomial, data = Smarket)
summary(glm.fit)


# get coefficients
coef(glm.fit)
summary(glm.fit)$coef


glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]

contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"

# see confusion matrix to determine how many observations were correctly or incorrectly classified
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)

Direction.2005=Direction[!train]


glm.fit=glm(Direction~Lag1+Lag2+Lag3 +Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")


glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction.2005)

mean(glm.pred==Direction.2005)


glm.fit=glm(Direction~Lag1+Lag2, data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm=pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
predict(glm.fit,newdata=data.frame(Lag1= c(1.2, 1.5), 
                                   Lag2=c(1.1, -0.8)), type="response")
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2, data=Smarket,subset=train)
lda.fit


# KNN
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)


table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)


dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822


# STANDARDIZE WITH SCALE()

standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
# not in 1:1000 range
mean(test.Y!="No")


table(knn.pred,test.Y)
9/(68+9)
knn.pred=knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
knn.pred=knn(train.X,test.X, train.Y, k=5)
table(knn.pred,test.Y)
4/15

# note will produce error
glm.fit=glm(Purchase~.,data=Caravan,family=binomial, subset=-test)

glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No", 1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)

glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)
