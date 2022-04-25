setwd('C:/Users/YJ-HWANG/Desktop/22-1/DataMining')
library(ISLR)
library(leaps)
library(glmnet)
library(pls)
library(MASS)

#### Chapter 6 R Lab ####
#### Lab 1: Subset Selection Methods

# Best Subset Selection
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)

regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)

names(reg.summary)
reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

coef(regfit.full,6)


# Forward and Backward Stepwise Selection
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)


# Choosing Among Models
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

test.mat=model.matrix(Salary~.,data=Hitters[test,])

val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best,10)

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)





#### Lab 2: Ridge Regression and the Lasso

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary



# ridge Regression
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type="coefficients")[1:20,]

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]




# The Lasso
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef

lasso.coef[lasso.coef!=0]





#### Lab 3: pcr and PLS Regression

# Principal Components Regression
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")

set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)



# Partial Least Squares
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")

pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)

pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)




#### Chapter6 exercise 9 ####
#### (a)
set.seed(777)
trainidx <- sample(1:nrow(College), round(nrow(College)/3))
train <- College[trainidx,]
test <- College[-trainidx,]
test_y <- test$Apps

#### (b)
lm9b <- lm(Apps~., data=train)
pred9b <- predict(lm9b, test)
mean((pred9b - test_y)^2)

#### (c)
trainmat <- model.matrix(Apps~., data=train)
testmat <- model.matrix(Apps~., data=test)
grid <- 10^seq(10, -2, length=100)

ridge <- glmnet(trainmat, train$Apps, alpha=0, lambda=grid, thresh=1e-12)
ridge.cv <- cv.glmnet(trainmat, train$Apps, alpha=0, lambda=grid, thresh=1e-12)
ridge_bestl <- ridge.cv$lambda.min
ridge_bestl  # best lambda in ridge
pred_ridge <- predict(ridge, s=ridge_bestl, newx=testmat)
mean((pred_ridge - test_y)^2)

#### (d)
lasso <- glmnet(trainmat, train$Apps, alpha=1, lambda=grid, thresh=1e-12)
lasso.cv <- cv.glmnet(trainmat, train$Apps, alpha=1, lambda=grid, thresh=1e-12)
lasso_bestl <- lasso.cv$lambda.min
lasso_bestl  # best lambda in lasso
pred_lasso <- predict(lasso, s=lasso_bestl, newx=testmat)
mean((pred_lasso - test_y)^2)

lasso.coef <- predict(lasso, type="coefficients", s=lasso_bestl)
length(lasso.coef[lasso.coef!= 0])
lasso.coef

#### (e)
pcr <- pcr(Apps~., data=train, scale=T, validation="CV")
par(mfrow=c(1,1))
validationplot(pcr, val.type="MSEP")

pred_pcr <-  predict(pcr, test, ncomp=16)
mean((pred_pcr-test_y)^2)

#### (f)
plsr <- plsr(Apps~., data=train, scale=T, validation="CV")
validationplot(plsr, val.type="MSEP")

pred_plsr <-  predict(plsr, test, ncomp=15)
mean((pred_plsr-test_y)^2)





#### Chapter6 exercise 11 ####
#### (a)
set.seed(777)
trainidx <- sample(1:nrow(Boston), round(nrow(Boston)/3))
train <- Boston[trainidx,]
test <- Boston[-trainidx,]
test_y <- test$crim




lm11 <- lm(crim~., data=train)
pred11 <- predict(lm11, test)
mean((pred11 - test_y)^2)


k=10
set.seed(777)
folds=sample(1:k,nrow(train),replace=TRUE)
cv.errors=matrix(NA,k,13, dimnames=list(NULL, paste(1:13)))

for(j in 1:k){
  best.fit=regsubsets(crim~.,data=train[folds!=j,],nvmax=13)
  for(i in 1:13){
    pred=predict(best.fit,train[folds==j,],id=i)
    cv.errors[j,i]=mean( (train$crim[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
which.min(mean.cv.errors)

reg.best=regsubsets(crim~.,data=train, nvmax=13)
coef(reg.best,2)
bs <- lm(crim~rad+medv, data=train)
predbs <- predict(bs, test)
mean((predbs - test_y)^2)


trainmat <- model.matrix(crim~., data=train)
testmat <- model.matrix(crim~., data=test)
grid <- 10^seq(10, -2, length=100)

ridge <- glmnet(trainmat, train$crim, alpha=0, lambda=grid, thresh=1e-12)
ridge.cv <- cv.glmnet(trainmat, train$crim, alpha=0, lambda=grid, thresh=1e-12)
ridge_bestl <- ridge.cv$lambda.min
ridge_bestl  # best lambda in ridge
pred_ridge <- predict(ridge, s=ridge_bestl, newx=testmat)
mean((pred_ridge - test_y)^2)


lasso <- glmnet(trainmat, train$crim, alpha=1, lambda=grid, thresh=1e-12)
lasso.cv <- cv.glmnet(trainmat, train$crim, alpha=1, lambda=grid, thresh=1e-12)
lasso_bestl <- lasso.cv$lambda.min
lasso_bestl  # best lambda in lasso
pred_lasso <- predict(lasso, s=lasso_bestl, newx=testmat)
mean((pred_lasso - test_y)^2)

lasso.coef <- predict(lasso, type="coefficients", s=lasso_bestl)
length(lasso.coef[lasso.coef!= 0])
lasso.coef


pcr <- pcr(crim~., data=train, scale=T, validation="CV")
par(mfrow=c(1,1))
validationplot(pcr, val.type="MSEP")

pred_pcr <-  predict(pcr, test, ncomp=13)
mean((pred_pcr-test_y)^2)

#### (c)
ridge.coef <- predict(ridge, type="coefficients", s=ridge_bestl)
length(ridge.coef[ridge.coef!= 0])
ridge.coef