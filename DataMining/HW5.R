#### Chaper 5 Lab: Cross-Validation and the Bootstrap ####
setwd("C:/Users/YJ-HWANG/Desktop/22-1/DataMining")
library(tidyverse)
library(ISglm5b)
library(boot)
library(MASS)

# 5.3.1 The Validation Set Approach


set.seed(1)
train=sample(392,196)

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)



# 5.3.2 Leave-One-Out Cross-Validation
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)

lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error




# 5.3.3 k-Fold Cross-Validation
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10




# 5.3.4 The Bootstrap

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)



# Estimating the Accuracy of a Linear Regression Model
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower,data=Auto))$coef

boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef




#### Chapter5 exercise 2 ####
# (g)
n <- 1:100000
prob <- 1-(1-1/n)^n
data.frame(n, prob) %>% ggplot(aes(n, prob))+
  geom_point(colour="lightcoral")+theme_light()+
  labs(x="# of data", y="probability", title="Prob. of jth obs is in bootstrap sample")+
  theme(plot.title=element_text(hjust=0.5))+
  coord_cartesian(ylim=c(0, 1))

# (h)
store=rep(NA, 10000)
for(i in 1:10000){
  store[i]=sum(sample (1:100 , rep =TRUE)==4) >0
}
mean(store)




#### Chapter5 exercise 5 ####
# (a)
set.seed(1)
glm5a <- glm(default~balance+income, data=Default, family="binomial")
summary(glm5a)

# (b)
set.seed(1)
trainidx <- sample(1:nrow(Default), round(nrow(Default)*0.7))  # 전체 데이터의 70%를 train으로 사용
train <- Default[trainidx, c("default", 'balance', 'income')]
valid <- Default[-trainidx, c("default", 'balance', 'income')]

glm5b <- glm(default~balance+income, data=train, family="binomial")
glm5b.probs = predict(glm5b, valid, type="response")
glm5b.preds = rep("No", length(valid$default)) 
glm5b.preds[glm5b.probs>0.5] = "Yes"
table(glm5b.preds,valid$default)
mis <- (2+78)/nrow(valid)
mis

# (c)
## split 1
set.seed(2022)
trainidx1 <- sample(1:nrow(Default), round(nrow(Default)*0.7))  # 전체 데이터의 70%를 train으로 사용
train <- Default[trainidx1, c("default", 'balance', 'income')]
valid <- Default[-trainidx1, c("default", 'balance', 'income')]

glm5c1 <- glm(default~balance+income, data=train, family="binomial")
glm5c1.probs = predict(glm5c1, valid, type="response")
glm5c1.preds = rep("No", length(valid$default)) 
glm5c1.preds[glm5c1.probs>0.5] = "Yes"
table(glm5c1.preds,valid$default)
mis1 <- (11+74)/nrow(valid)
mis1

## split 2
set.seed(4)
trainidx2 <- sample(1:nrow(Default), round(nrow(Default)*0.7))  # 전체 데이터의 70%를 train으로 사용
train <- Default[trainidx2, c("default", 'balance', 'income')]
valid <- Default[-trainidx2, c("default", 'balance', 'income')]

glm5c2 <- glm(default~balance+income, data=train, family="binomial")
glm5c2.probs = predict(glm5c2, valid, type="response")
glm5c2.preds = rep("No", length(valid$default)) 
glm5c2.preds[glm5c2.probs>0.5] = "Yes"
table(glm5c2.preds,valid$default)
mis2 <- (13+68)/nrow(valid)
mis2

## split 3
set.seed(18)
trainidx3 <- sample(1:nrow(Default), round(nrow(Default)*0.7))  # 전체 데이터의 70%를 train으로 사용
train <- Default[trainidx3, c("default", 'balance', 'income')]
valid <- Default[-trainidx3, c("default", 'balance', 'income')]

glm5c3 <- glm(default~balance+income, data=train, family="binomial")
glm5c3.probs = predict(glm5c3, valid, type="response")
glm5c3.preds = rep("No", length(valid$default)) 
glm5c3.preds[glm5c3.probs>0.5] = "Yes"
table(glm5c3.preds,valid$default)
mis3 <- (12+60)/nrow(valid)
mis3


# (d)
train <- Default[trainidx3, ]
valid <- Default[-trainidx3, ]

glm5d <- glm(default~., data=train, family="binomial")
glm5d.probs = predict(glm5d, valid, type="response")
glm5d.preds = rep("No", length(valid$default)) 
glm5d.preds[glm5d.probs>0.5] = "Yes"
table(glm5d.preds,valid$default)
mis <- (10+59)/nrow(valid)
mis



#### Chapter 5 exercise 7 ####
# (a)
glm7a <- glm(Direction~Lag1+Lag2, data=Weekly, family="binomial")
summary(glm7a)

# (b)
glm7b <- glm(Direction~Lag1+Lag2, data=Weekly[-1, ], family="binomial")
summary(glm7b)

# (c)
obs1 <- Weekly[1,]
probs7c <- predict(glm7b, obs1, type="response")
preds7c <- ifelse(probs7c>0.5, "Up", "Down")
preds7c

#(d)
preds = rep(0, nrow(Weekly))
errors = rep(0, nrow(Weekly))
for (i in 1:nrow(Weekly)) {
  m <- glm(Direction~Lag1+Lag2, data=Weekly[-i, ], family="binomial")
  probs <- predict(m, Weekly[i,], type="response")
  
  preds[i] <- ifelse(probs>0.5, "Up", "Down")
  errors[i] <- ifelse(preds[i] != Weekly$Direction[i], 1, 0)
}

#(e)
mean(errors)



#### Chapter 5 exercise 9 ####
# (a)
mu <- mean(Boston$medv)
mu

# (b)
sd <- sqrt(sum((Boston$medv - mu)^2) / (nrow(Boston)-1))
se <- sd/sqrt(nrow(Boston))
se

# (c)
calmu <- function(data, index){
  return(mean(data$medv[index]))
}

set.seed(777)
boot(Boston, calmu, 1000)

# (d)
ci <- c(mu - 2*se, mu + 2*se)
ci

t.test(Boston$medv)

# (e)
med <- median(Boston$medv)
med
# (f)
calmed <- function(data, index){
  return(median(data$medv[index]))
}

set.seed(777)
boot(Boston, calmed, 1000)

# (g)
pct <- quantile(Boston$medv, 0.1)
pct

# (h)
calpct <- function(data, index){
  return(quantile(Boston$medv[index], 0.1))
}

set.seed(777)
boot(Boston, calpct, 1000)
