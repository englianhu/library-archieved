##
## chapter 4.1
product <- "rb"
source("d:/liwei/book/code/helper.r")


all.contracts <- get.dates(product)
all.contracts
all.data <- c()
for (contract in all.contracts) {
  load(contract)
  if (length(all.data)==0) 
    all.data <- data[data$continuous,]
  else 
    all.data <- rbind(all.data, data[data$continuous,])
}
dim(all.data)
## [1] 74565    21
names(all.data)


n.train <- 6
train.range <- 1:n.train
test.range <- (n.train+1):9
train.contracts <- all.contracts[train.range]
test.contracts <- all.contracts[test.range]

## Since we don't have prepare.data, we load the data instead
#train.mat <- prepare.data(model$signals, model$y, product, train.contracts, over.night=-1)
#test.mat <- prepare.data(model$signals, model$y, product, test.contracts, over.night=-1)
#colnames(train.mat) <- c(paste("x",1:19,sep="."),"y")
#colnames(test.mat) <- c(paste("x",1:19,sep="."),"y")
load(file="d:/liwei/book/data/chpt.4.data.1.RData")
dim(train.mat)
## [1] 21825    20
dim(test.mat)
## [1] 12756    20


## Figure 4-1
plot(train.mat$y, type="l", ylab="return", main="y")

fit <- lm(y~.+0, data=train.mat)
summary(fit)
pred <- predict(fit, newdata=test.mat)
R2(pred, test.mat$y,"traditional")
## [1] -0.002399677
R2

## chapter 4.2

train.range <- 1:3
valid.range <- 4:6
test.range <- 7:9
valid.contracts <- all.contracts[valid.range]
train.contracts <- all.contracts[train.range]
test.contracts <- all.contracts[test.range]
#train.mat <- prepare.data(model$signals, model$y, product, train.contracts, over.night=-1)
#valid.mat <- prepare.data(model$signals, model$y, product, valid.contracts, over.night=-1)
#test.mat <- prepare.data(model$signals, model$y, product, test.contracts, over.night=-1)
#save(train.mat, valid.mat, test.mat, file="d:/liwei/book/data/chpt4.data.2.RData")
load(file="d:/liwei/book/data/chpt4.data.2.RData")
dim(train.mat)
## [1] 10485    20
dim(valid.mat)
## [1] 11340    20
dim(test.mat)
## [1] 12756    20

x <- as.matrix(train.mat[,1:(ncol(train.mat)-1)])
x.valid <- as.matrix(valid.mat[,1:(ncol(valid.mat)-1)])
n.sample <- nrow(x)
x.train <- x
x.test <- as.matrix(test.mat[,1:(ncol(test.mat)-1)])
n.signals <- ncol(x)
signals <- names(train.mat)[1:n.signals]

library(glmnet)
n.coef <- 100
grid=10^seq(-4,3,length=n.coef)
fit.ridge <- glmnet(x,train.mat$y,intercept=FALSE, alpha=0, lambda = grid)
coef.mat <- coef(fit.ridge)[-1,]
oos.mat <- rep(0,n.coef)
for (i in 1:n.coef) {
  cur.coef <- coef.mat[,i]
  pred <- x.valid%*%cur.coef
  oos.mat[i] <- R2(pred,valid.mat$y, "traditional")
}
## Figure 4-2
plot(oos.mat, type="l", ylab="R2", main="ridge")


best <- which.max(oos.mat)
best
## 61
train.valid.mat <- rbind(train.mat,valid.mat)
x.test <- as.matrix(test.mat[,1:(ncol(test.mat)-1)])
x <- as.matrix(train.valid.mat[,1:(ncol(train.valid.mat)-1)])
fit.ridge <- glmnet(x,train.valid.mat$y,intercept=FALSE, alpha=0, lambda = grid)
coef.mat <- coef(fit.ridge)[-1,]
cur.coef <- coef.mat[,best]
pred <- x.test%*%cur.coef
R2(pred,test.mat$y, "traditional")
## [1] -0.001568153

coef.compare <- as.matrix(cbind(fit$coefficients, cur.coef))
rownames(coef.compare) <- paste("x",1:19,sep=".")
colnames(coef.compare) <- c("ols","ridge")
coef.compare

grid=10^seq(-10,10,length=n.coef)
x <- as.matrix(train.mat[,1:(ncol(train.mat)-1)])
x.new <- as.matrix(train.valid.mat[,1:(ncol(train.mat)-1)])
fit.lasso <- glmnet(x,train.mat$y,intercept=FALSE, lambda = grid)
coef.mat <- coef(fit.lasso)[-1,]
oos.mat <- rep(0,n.coef)
for (i in 1:n.coef) {
  cur.coef <- coef.mat[,i]
  pred <- x.valid%*%cur.coef
  oos.mat[i] <- R2(pred,valid.mat$y, "traditional")
}
plot(oos.mat, type="l")
best <- which.max(oos.mat)
best
coef.mat[,best]
x <- as.matrix(train.valid.mat[,1:(ncol(train.valid.mat)-1)])
fit.lasso <- glmnet(x,train.valid.mat$y,intercept=FALSE, lambda = grid)
coef.mat <- coef(fit.lasso)[-1,]
pred.test <- x.test %*% coef.mat[,best]
R2(pred.test, test.mat$y, "traditional")
## [1] -0.001875301

coef.compare <- as.matrix(cbind(fit$coefficients, coef.mat[,best]))
rownames(coef.compare) <- paste("x",1:19,sep=".")
colnames(coef.compare) <- c("ols","lasso")
coef.compare

## chapter 4.3
## Figure 4.3
plot(density(train.valid.mat$y), main="y") ## 画出y的概率密度函数

## Figure 4.4
qqnorm(train.valid.mat$y) ## 画出y的QQ图
qqline(train.valid.mat$y) ## 加入理论上的QQ线
