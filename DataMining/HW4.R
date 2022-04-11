#### Chapter 4 R Lab ####

# load packages
setwd('C:/Users/YJ-HWANG/Desktop/22-1/DataMining')
library(MASS)
library(ISLR)
library(tidyverse)
library(class)

# 4.6.1 The Stock Market Data
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

cor(Smarket)  # error
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)



# 4.6.2 Logistic Regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)

coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)

glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"

table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)

predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")




# 4.6.3 Linear Discriminant Analysis
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

sum(lda.pred$posterior[,1]>.9)




# 4.6.4 Quadratic Discriminant Analysis
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)




# 4.6.5 K-Nearest Neighbors
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252

knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)




# 4.6.6 An Application to Caravan Insurance Data
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

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
mean(test.Y!="No")

table(knn.pred,test.Y)
9/(68+9)

knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15

glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)



#### Chapter 4 exercise 10 ####
head(Weekly)

# a)
library(GGally)
lower = function(data, mapping, method='lm'){
  p <- ggplot(data=data, mapping=mapping) +
    geom_point(colour='lightcoral', size=1) +
    geom_smooth(method=method, colour='brown3', se=FALSE)
}
ggpairs(Weekly[, 1:8],
        lower=list(continuous=lower))+theme_light()

# b)
glm10.b <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
               data=Weekly, family=binomial)
summary(glm10.b)

# c)
glm10.probs <- predict(glm10.b, type="response")
glm10.preds <- rep("Down", nrow(Weekly))
glm10.preds[glm10.probs>0.5] <- "Up"  # threshold=0.5
table(glm10.preds, Weekly$Direction)
(54+557)/1089

# d)
train <- Weekly$Year < 2009
test <- Weekly[!train, ]
testy <- Weekly$Direction[!train]
glm10.d <- glm(Direction ~ Lag2, data=Weekly, 
               family='binomial', subset=train)

glm10.d.probs <- predict(glm10.d, test, type="response")
glm10.d.preds <- rep("Down", nrow(test))
glm10.d.preds[glm10.d.probs>0.5] <- "Up"  # threshold=0.5
table(glm10.d.preds, testy)
(9+56)/nrow(test)

# e)
lda10.e <- lda(Direction~Lag2, data=Weekly, subset=train)
lda_pred <- predict(lda10.e, test)
table(lda_pred$class, testy)

# f)
qda10.f <- qda(Direction~Lag2, data=Weekly, subset=train)
qda_pred <- predict(qda10.f, test)
table(qda_pred$class, testy)
61/nrow(test)


# g)
train_x <- Weekly[train, 3]
test_x <- Weekly[!train, 3]
trainy <- Weekly$Direction[train]

# nrow x 1 vector·Î º¯È¯
dim(train_x) <- c(985, 1)
dim(test_x) <- c(104, 1)

knn_pred <- knn(train_x, test_x, trainy, k=1)
table(knn_pred, testy)
53/nrow(test)
