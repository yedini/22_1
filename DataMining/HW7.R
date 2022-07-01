setwd('C:/Users/YJ-HWANG/Desktop/22-1/DataMining')
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)
library(glmnet)
library(tidyverse)
##### Chapter 8 Lab: Decision Trees ####

# Fitting Classification Trees
attach(Carseats)
High=factor(ifelse(Sales<=8,"No","Yes"))
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200



# Fitting Regression Trees
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)


# Bagging and Random Forests
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)


# Boosting
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)




#### Chapter 8 exercise 8
# (a)
set.seed(777)
train <- sample(1:nrow(Carseats), round(nrow(Carseats)*0.7))
Carseats.train <- Carseats[train, ]
Carseats.test <- Carseats[-train, ]

# (b)
tree.carseats <- tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)


plot(tree.carseats)
text(tree.carseats, pretty = 0)

yhat1 <- predict(tree.carseats, Carseats.test)
treemse1 <- mean((yhat1 - Carseats.test$Sales)^2)
treemse1


# (c)
cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
tree.min <- which.min(cv.carseats$dev)
points(tree.min, cv.carseats$dev[tree.min], col = "red", cex = 2, pch = 20)
# size=9에서 가장 낮은 값을 가짐 -> 9개의 node를 갖는 tree로 pruning
prune.carseats <- prune.tree(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
yhat2 <- predict(prune.carseats, newdata = Carseats.test)
treemse2 <- mean((yhat2 - Carseats.test$Sales)^2)
treemse2


# (d)
bag.carseats <- randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, importance = TRUE)
yhat3 <- predict(bag.carseats, newdata = Carseats.test)
treemse3 <- mean((yhat3 - Carseats.test$Sales)^2)
treemse3
importance(bag.carseats)


# (e)
rf.carseats <- randomForest(Sales ~ ., data = Carseats.train, mtry = 3, ntree = 500, importance = TRUE)
yhat4 <- predict(rf.carseats, newdata = Carseats.test)
treemse4 <- mean((yhat4 - Carseats.test$Sales)^2)
treemse4
importance(rf.carseats)



#### Chapter 8 exercise 9
# (a)
set.seed(2022)
train <- sample(1:nrow(OJ), 800)
OJ.train <- OJ[train, ]
OJ.test <- OJ[-train, ]

# (b)
OJ.test <- OJ[-train, ]
tree.oj <- tree(Purchase ~ ., data = OJ.train)
summary(tree.oj)

# (c)
tree.oj

# (d)
plot(tree.oj)
text(tree.oj, pretty = 0)

# (e)
tree.pred <- predict(tree.oj, OJ.test, type = "class")
table(tree.pred, OJ.test$Purchase)
1 - (147+75)/nrow(OJ.test)   # test error rate

# (f)
cv.oj <- cv.tree(tree.oj, FUN = prune.misclass)
cv.oj

# (g)
plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree size", ylab = "Deviance")

# (h)
# 7일 때 error rate가 가장 낮다.

# (i)
prune.oj <- prune.misclass(tree.oj, best = 7)
plot(prune.oj)
text(prune.oj, pretty = 0)

# (j)
summary(tree.oj)  # not pruned
summary(prune.oj)    # pruned

# (k)
prune.pred <- predict(prune.oj, OJ.test, type = "class")
table(prune.pred, OJ.test$Purchase)
1 - (147+75)/nrow(OJ.test)   # pruned test error rate



#### Chapter 8 exercise 10
# (a)
Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)

# (b)
train <- 1:200
Hitters.train <- Hitters[train, ]
Hitters.test <- Hitters[-train, ]

# (c)
pows <- seq(-8, -0.1, by = 0.1)
lambdas <- 10^pows
train.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.hitters <- gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  pred.train <- predict(boost.hitters, Hitters.train, n.trees = 1000)
  train.err[i] <- mean((pred.train - Hitters.train$Salary)^2)
}
plot(lambdas, train.err, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")

# (d)
test.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.hitters <- gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  yhat <- predict(boost.hitters, Hitters.test, n.trees = 1000)
  test.err[i] <- mean((yhat - Hitters.test$Salary)^2)
}
plot(lambdas, test.err, type = "b", xlab = "Shrinkage values", ylab = "Test MSE")

lambdas[which.min(test.err)]
min(test.err)

# (e)
# regression
fitlm <- lm(Salary ~ ., data = Hitters.train)
pred1 <- predict(fitlm, Hitters.test)
mean((pred1 - Hitters.test$Salary)^2)

# ridge
x <- model.matrix(Salary ~ ., data = Hitters.train)
x.test <- model.matrix(Salary ~ ., data = Hitters.test)
y <- Hitters.train$Salary
fitridge <- glmnet(x, y, alpha = 0)
pred2 <- predict(fitridge, s = 0.01, newx = x.test)
mean((pred2 - Hitters.test$Salary)^2)

# (f)
boost.hitters <- gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[which.min(test.err)])
summary(boost.hitters)
s <- summary(boost.hitters)
s %>% ggplot(aes(reorder(var, rel.inf), rel.inf)) + geom_bar(stat='identity', fill="lightcoral")+
  ggtitle("Variable importance plot")+coord_flip()+
  theme_light()+theme(plot.title=element_text(hjust=0.5))


# (g)
set.seed(777)
bag.hitters <- randomForest(Salary ~ ., data = Hitters.train, mtry = 19, ntree = 500)
yhat.bag <- predict(bag.hitters, newdata = Hitters.test)
mean((yhat.bag - Hitters.test$Salary)^2)



#### Chapter 8 exercise 11
# (a)
train <- 1:1000
Caravan$Purchase <- ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train <- Caravan[train, ]
Caravan.test <- Caravan[-train, ]

# (b)
set.seed(777)
boost.caravan <- gbm(Purchase ~ ., data = Caravan.train, 
                     distribution = "gaussian", n.trees = 1000, 
                     shrinkage = 0.01)
summary(boost.caravan)
s <- summary(boost.caravan)
s[1:15, ] %>% ggplot(aes(reorder(var, rel.inf), rel.inf)) + geom_bar(stat='identity', fill="lightcoral")+
  ggtitle("Variable importance plot")+coord_flip()+
  theme_light()+theme(plot.title=element_text(hjust=0.5))

# (c)
probs.test <- predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response")
pred.test <- ifelse(probs.test > 0.2, 1, 0)
table(Caravan.test$Purchase, pred.test) # confusion matrix
12/(12+29) # 1로 예측된 데이터 중 실제로 1인 비율

# logistic regresion
logreg <- glm(Purchase~., data=Caravan.train,
              family="binomial")
lr.test <- predict(logreg, Caravan.test, type = "response")
lr.pred <- ifelse(lr.test > 0.2, 1, 0)
table(Caravan.test$Purchase, lr.pred) # confusion matrix
58/(350+58) # 1로 예측된 데이터 중 실제로 1인 비율


#### Chapter 8 exercise 12
Smarket$Direction <- ifelse(Smarket$Direction == "Up", 1, 0)
train <- Smarket %>% filter(Year != 2005)
test <- Smarket %>% filter(Year == 2005)

# logistic regression
lr <- glm(Direction~.-Year-Today, data=train,
              family="binomial")
lr.test <- predict(lr, test, type = "response")
lr.pred <- ifelse(lr.test > 0.5, 1, 0)
table(test$Direction, lr.pred) # confusion matrix
(77+44)/nrow(test)

# boosting
set.seed(777)
boost.sma <- gbm(Direction~.-Year-Today, data=train,
                     distribution = "gaussian", n.trees = 1000, 
                     shrinkage = 0.1)
probs.test <- predict(boost.sma, test, n.trees = 1000, type = "response")
pred.test <- ifelse(probs.test > 0.5, 1, 0)
table(test$Direction, pred.test) # confusion matrix
(67+55)/nrow(test)

# bagging
bagg.sma <- randomForest(Direction~.-Year-Today, data=train,
                         mtry = 6, ntree = 500, importance = TRUE)
bagg.test <- predict(bagg.sma, test, type = "response")
bagg.pred <- ifelse(bagg.test > 0.5, 1, 0)
table(test$Direction, bagg.pred) # confusion matrix
(59+65)/nrow(test)

# random forest
rf.sma <- randomForest(Direction~.-Year-Today, data=train,
                         mtry = 3, ntree = 500, importance = TRUE)
rf.test <- predict(rf.sma, test, type="response")
rf.pred <- ifelse(rf.test > 0.5, 1, 0)
table(test$Direction, rf.pred) # confusion matrix
(53+68)/nrow(test)
