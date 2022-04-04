##### Chapter 3 Lab ####

# load packages
setwd('C:/Users/YJ-HWANG/Desktop/22-1/DataMining')
library(MASS)
library(ISLR)
library(tidyverse)


# 3.6.2 Simple Linear Regression
fix(Boston)
names(Boston)
lm.fit=lm(medv~lstat)   # data 지정 안했으므로 error

lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit

summary(lm.fit)

names(lm.fit)
coef(lm.fit)

confint(lm.fit)

predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")

plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

par(mfrow=c(1,1))



# 3.6.3 Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

library(car)
vif(lm.fit)

lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

lm.fit1=update(lm.fit, ~.-age)



# 3.6.4 Interaction Terms

summary(lm(medv~lstat*age,data=Boston))




# 3.6.5 Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

summary(lm(medv~log(rm),data=Boston))




# 3.6.6 Qualitative Predictors

fix(Carseats)
names(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)

# 3.6.7 Writing Functions

LoadLibraries
LoadLibraries()
LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()



#### Chapter 3 Exercise 8 ####
head(Auto)

## (a)
lm1 <- lm(mpg~horsepower, data=Auto)
summary(lm1)
predict(lm1, data.frame(horsepower=c(98)), interval='prediction')
predict(lm1, data.frame(horsepower=c(98)), interval='confidence')

## (b)
par(mfrow=c(1,1))
Auto %>% ggplot(aes(horsepower, mpg))+geom_point(color="lightcoral")+
  geom_smooth(method="lm", color="lightseagreen")+
  ggtitle("predictor vs response plot")+
  theme_light()+theme(plot.title=element_text(hjust=0.5))

## (c)
par(mfrow=c(2,2))
plot(lm1)


#### Chapter 3 Exercise 9 ####

## (a)
pairs(Auto)

## (b)
cor(Auto[, 1:8])

## (c)
lm2 <- lm(mpg~+., data=Auto[, 1:8])
summary(lm2)

## (d)
par(mfrow=c(2,2))
plot(lm2)

## (e)
lm3 <- lm(mpg~.+displacement:weight+horsepower:acceleration, data=Auto[, 1:8])
summary(lm3)

## (f)
lm4 <- lm(mpg~.+log(weight)+sqrt(year), data=Auto[, 1:8])
summary(lm4)



#### Chapter 3 Exercise 13
## (a)
set.seed(1)
x <- rnorm(100)
head(x)

## (b)

eps <- rnorm(100, 0, sqrt(0.25))
head(eps)

## (c)
y <- -1+0.5*x+eps
length(y)

## (d)
par(mfrow=c(1, 1), col="black")
plot(x, y, main="Scatterplot of x and y", col="lightcoral")

## (e)
lm13.1 <- lm(y~x)
summary(lm13.1)

## (f)
plot(x, y, main="Scatterplot of x and y", col="lightcoral")
abline(lm13.1, col=2, lwd=2)
abline(-1, 0.5, col=3, lwd=2)
legend(-2, legend = c("fitted", "real"), col=2:3, lwd=2)

## (g)
lm13.2 <- lm(y~x+I(x^2))
summary(lm13.2)

## (h)
set.seed(1)
x <- rnorm(100)
eps2 <- rnorm(100, 0, sqrt(0.125))

y2 <- -1+0.5*x+eps2
lm13.3 <- lm(y2~x)
summary(lm13.3)

plot(x, y2, main="Scatterplot of x and y", col="lightcoral")
abline(lm13.3, col=2, lwd=2)
abline(-1, 0.5, col=3, lwd=2)
legend(-1.5, legend = c("fitted", "real"), col=2:3, lwd=2)

## (i)
set.seed(1)
x <- rnorm(100)
eps3 <- rnorm(100, 0, sqrt(0.5))

y3 <- -1+0.5*x+eps3
lm13.4 <- lm(y3~x)
summary(lm13.4)

plot(x, y3, main="Scatterplot of x and y", col="lightcoral")
abline(lm13.4, col=2, lwd=2)
abline(-1, 0.5, col=3, lwd=2)
legend(-1.5, legend = c("fitted", "real"), col=2:3, lwd=2)

## (j)
confint(lm13.1)
confint(lm13.3)
confint(lm13.4)


#### Chapter3 Exercise 14
## (a)
set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)

## (b)
cor(x1, x2)
plot(x1, x2, col="lightcoral", main="Scatterplot of x1 and x2")

## (c)
lm14.1 <- lm(y~x1+x2)
summary(lm14.1)

## (d)
lm14.2 <- lm(y~x1)
summary(lm14.2)

## (e)
lm14.3 <- lm(y~x2)
summary(lm14.3)

## (g)
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

lm14.4 <- lm(y~x1+x2)
summary(lm14.4)

lm14.5 <- lm(y~x1)
summary(lm14.5)

lm14.6 <- lm(y~x2)
summary(lm14.6)

par(mfrow=c(2,2))
plot(lm14.4)
