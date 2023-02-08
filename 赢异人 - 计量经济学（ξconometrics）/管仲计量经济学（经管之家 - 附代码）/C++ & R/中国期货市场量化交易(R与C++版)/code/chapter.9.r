## chapter 9
## use a lot of data from Chapter 8
## please run this script right after chapter 8
source("d:/liwei/book/code/helper.r")
RiskParity <- function(Sub, short=FALSE) { 
  m = ncol(Sub)
  Cov = matrix(cov(Sub, use = "na.or.complete"), m, m)
  TotalTRC = function(x, use.short=short) {
    if (!use.short) {
      if (sum(x)>1 || sum(x<0)>0) return (10^13)
    }
    x = matrix(c(x, 1-sum(x)))
    TRC = as.vector((Cov %*% x) * x)
    sum(outer(TRC, TRC, "-")^2)
  }
  sol = optim(par = rep(1/m,m-1), TotalTRC)
  w = c(sol$par, 1-sum(sol$par))
  return(w)
}


load(file="d:/liwei/book/data/all.product.pnl.RData")
Sub <- data
w.parity <- RiskParity(Sub,FALSE)
w.parity
## [1] 0.18426632 0.16338415 0.14353022 0.04498637 0.16620539 0.16364004 0.09674564 0.03724188

Cov <- cov(data)
Cov%*% w.parity^2
##[,1]
## rb  8892.737
## hc  9116.452
## ru  9201.539
## cu  9117.859
## zn  9453.542
## bu  8004.097
## ag 11101.847
## au 14643.231
parity.weight <-round(w.parity*capital/contract.value*n.product)
parity.weight
## [1] 40 33  9  2 10 54 13  1

parity.mv <- as.matrix(all.pnl) %*%　parity.weight

## Figure 9-1
plot(parity.mv, type="l", col=3)
points(portfolio.mv, type="l")
points(portfolio, type="l", col=2)
legend("topleft", legend=c("mean-variance","equal capital", "risk parity"), lty=1, col=1:3)

library(FinCovRegularization)
FinCovRegularization::RiskParity(Cov)
## rb     hc     ru     cu     zn     bu     ag     au 
## 0.1877 0.1588 0.1343 0.0427 0.1645 0.1842 0.0910 0.0367 

w.parity

FinCovRegularization::RiskParity
parity.stat <- get.each.pnl.stat(diff(parity.mv))
mv.stat <- get.each.pnl.stat(diff(portfolio.mv))
equal.stat <- get.each.pnl.stat(diff(portfolio))
parity.stat$sharp
## [1] 3.827667
mv.stat$sharp
## [1] 3.792253
equal.stat$sharp
## [1] 3.868946


## 9.2
product <- "rb"
load(file=paste("d:/liwei/book/data/",product,".new.RData",sep=""))
load(file=paste("d:/liwei/book/data/",product,".final.rolling.model.RData",sep=""))
all.contracts <- get.dates(product)
n.contracts <- length(all.contracts)
chosen <- 4
final.model <- list()
final.model$final.signal <- final.rolling.model[[chosen]]$signals
final.model$coef <- final.rolling.model[[chosen]]$coef
y.str <- final.rolling.model[[chosen]]$y.str
regression.list <- list()
for (k in 1:n.contracts) regression.list[[k]] <- final.model

open.list <- product.info[[product]]$open.thre.list
thre.matrix <- data.frame(open=open.list, close=-open.list)
result <- read.csv(file="d:/liwei/chpt9.2.csv", header=TRUE)

## Table 9-1
result

## Follow code cannot be run because lacking of get.linear.5m.test()
# all.pnl <- c()
# all.date <- c()
# all.time <- c()
# for (cur in 4:n.contracts) {
#   train <- get.linear.5m.test(product, all.contracts[1:(cur-1)],regression.list,y.str,thre.matrix,real=FALSE)
#   train$thre$recent.drawdown <- apply(train$mm.pnl.mat, 2, function(x) (max(x)-tail(x,1))/max(x))
#   train$thre$avg.num <- train$thre$num/(cur-1)
#   rule.chosen <- which(with(train$thre, sharp>1 & avg.num>6 & avg.profit>8))
#   if (length(rule.chosen)==0) {
#     ##test <- get.linear.5m.test(product, all.contracts[cur],regression.list,y.str,thre.matrix[1,],real=FALSE)
#     test.pnl <- rep(0, length(test$mm.pnl.mat))
#   } else {
#     test <- get.linear.5m.test(product, all.contracts[cur],regression.list,y.str,thre.matrix[rule.chosen,],real=FALSE)
#     if (length(rule.chosen)>1) {
#       test.pnl <- rowSums(test$mm.pnl.mat)/length(rule.chosen)
#     } else {
#       test.pnl <- test$mm.pnl.mat
#     }
#   } 
#   all.pnl <- c(all.pnl, test.pnl[1], diff(test.pnl))
#   all.date <- c(all.date, test$date)
#   all.time <- c(all.time, test$time)
#   cat(cur, rule.chosen,"\n")
# }
# test <- get.linear.5m.test(product, all.contracts[4:15],regression.list,y.str,thre.matrix,real=FALSE)

save(all.date, all.time, all.pnl, train, test, file="d:/liwei/book/data/chpt.9.rolling.RData")

load("d:/liwei/book/data/chpt.9.rolling.RData")
## Figure 9-2
plot(get.date.time(all.date,all.time),cumsum(all.pnl), type="l", ylab="pnl", main="rolling",
     xlab="date")

## Figure 9-3
plot(rep(get.date.time(all.date,all.time),2),c(cumsum(all.pnl),test$mm.pnl.mat[,7]), type="n", ylab="pnl", main="rolling",
     xlab="date")
points(get.date.time(all.date, all.time),cumsum(all.pnl), type="l", col=1)
points(get.date.time(all.date, all.time), test$mm.pnl.mat[,7], type="l", col=2)
legend("topleft", c("rolling", "best"), lty=1, col=1:2)


train$thre$max.drawdown <- apply(train$mm.pnl.mat, 2, function(x) max(cummax(x)-x))
train$thre$return.drawdown <- train$thre$pnl/train$thre$max.drawdown
result <- train$thre[,c("open", "sharp", "pnl", "prob", "ratio", "num", "max.drawdown", "return.drawdown")]

result <- read.csv("d:/liwei/book/data/chpt9.3.csv", header = TRUE)

## Table 9-2
result


contract.len <- rep(0, n.contracts)
contract.day <- rep(0, n.contracts)
for (i in 1:n.contracts) {
  load(all.contracts[i])
  contract.len[i] <- sum(data$continuous)
  contract.day[i] <- sum(data$continuous & data$time=="15:00:00")
}
contract.cum <- cumsum(contract.len)
contract.day.count <- cumsum(contract.day)

load("d:/liwei/book/data/chpt9.pp.RData")
end.day <- which(pp$time=="15:00:00")
n.days <- length(end.day)


n.bar <- nrow(pp$mm.pnl.mat)
n.strat <- ncol(pp$mm.pnl.mat)

rolling.recent.drawdown <- matrix(0, nrow=n.days, ncol=n.strat)
rolling.sharp <- rolling.recent.drawdown
rolling.drawdown <- rolling.recent.drawdown
rolling.num <- rolling.recent.drawdown
rolling.avg.profit <- rolling.recent.drawdown
rolling.pnl <- rolling.recent.drawdown
for (cur.day in (contract.day.count[3]+1):n.days) {
  cur.len <- end.day[cur.day]
  sub.pnl <- pp$mm.pnl.mat[1:cur.len,]
  sub.pos <- pp$position.mat[1:cur.len,]
  sub.daily <- pp$mm.pnl.mat[1:n.bar<cur.len & pp$time=="15:00:00",]
  sub.daily.pnl <- rbind(sub.daily[1,], diffM(sub.daily))
  recent.drawdown <- apply(sub.pnl, 2, function(x) (max(x)-tail(x,1))/max(x))
  drawdown <- apply(sub.pnl, 2, function(x) max(cummax(x)-x)/tail(x,1))
  num <- colSums(diffM(sub.pos)!=0)
  pnl <- sub.pnl[cur.len,]
  avg.profit <- pnl/num
  sharp <- apply(sub.daily.pnl, 2, function(x) mean(x)/sd(x)*sqrt(246))
  rolling.recent.drawdown[cur.day,] <- recent.drawdown
  rolling.sharp[cur.day,] <- sharp
  rolling.num[cur.day,] <- num
  rolling.avg.profit[cur.day,] <- avg.profit
  rolling.pnl[cur.day,] <- pnl
  rolling.drawdown[cur.day,] <- drawdown
}

## Figure 9-4
plot(rolling.avg.profit[(contract.day.count[3]+1):n.days,7], type="l",
     main="avg profit 7", ylab="profit")

## Figure 9-5
plot(rolling.avg.profit[(contract.day.count[3]+1):n.days,11], type="l",
     main="avg profit 11", ylab="profit")


rank.avg.profit <- t(apply(rolling.avg.profit, 1, function(x) sort.int(x, index.return = TRUE, decreasing=TRUE)$ix))
start <- contract.day.count[3]+1

## Figure 9-6
plot(rank.avg.profit[start:n.days,7], type="l", main="avg profit 7 rank",
     ylab="rank") ## 画图


## Figure 9-7
plot(rolling.sharp[start:n.days,7], type="l", main="sharp 7", ylab="sharp") ## 画图

rank.sharp <- t(apply(rolling.sharp, 1, function(x) sort.int(x, index.return = TRUE, decreasing=TRUE)$ix))
## Figure 9-8
plot(rank.sharp[start:n.days,7], type="l", main="rank.sharp 7", ylab="sharp") ## 画图

rank.recent.drawdown <- t(apply(rolling.recent.drawdown, 1, function(x) sort.int(x, index.return = TRUE, decreasing=FALSE)$ix))

## Figure 9-9
plot(rank.recent.drawdown[start:n.days,7], type="l", main="recent.drawdown 7", 
     ylab="ratio") ## 画图


daily.pnl <- pp$mm.pnl.mat[end.day,]
each.day.pnl <- rbind(daily.pnl[1], diffM(daily.pnl))


all.rolling.pnl <- matrix(0, nrow=n.days, ncol=n.strat) ## 总体滚动结果
for (i.rank in 1:n.strat) { ## 遍历所有排序指标阈值
  rolling.pnl <- rep(0,n.days)  ## 当前滚动的资金曲线
  available <- matrix(FALSE, nrow=n.days, ncol=n.strat) ## 可以使用的策略
  for (cur.day in (contract.day.count[3]+1):(n.days-1)) {
    available[cur.day,] <- rolling.recent.drawdown[cur.day,]<0.1 & rank.avg.profit[cur.day,]<=i.rank &  rank.sharp[cur.day,]<=i.rank ##选择策略的条件
    each.pnl <- each.day.pnl[cur.day+1,available[cur.day,]]　## 用于下一天交易
    if (length(each.pnl)>0) rolling.pnl[cur.day+1] <- sum(each.pnl)/length(each.pnl) ## 取策略盈利的平均值，即默认一手交易
  }
  all.rolling.pnl[,i.rank] <- rolling.pnl ## 代入当前策略
}

pnl.sum <- apply(all.rolling.pnl, 2, sum)

## Figure 9-10
plot(pnl.sum, type="l")
best <- which.max(pnl.sum)
best
## [1] 8

rolling.best <- cumsum(all.rolling.pnl[(contract.day.count[3]+1):n.days,best]) ## 最佳滚动策略
backtest.best <- daily.pnl[(contract.day.count[3]+1):n.days,7]-daily.pnl[contract.day.count[3],7]
## 最佳回测策略
best.len <- length(rolling.best) ## 测试长度

## Figure 9-11
plot(rep(1:best.len,2), c(rolling.best, backtest.best), type="n", ylab="pnl") ## 画图
points(rolling.best, type="l",col=1) ## 画出滚动策略
points(backtest.best, type="l", col=2) ## 画出回测策略
legend("topleft", legend=c("rolling","best"), col=1:2, lty=1) ## 标注

daily.num <- apply(available, 1, sum) ## 每天策略的总数

## Figure 9-12
plot(daily.num[start:n.days], type="l", xlab="day", ylab="num") ## 画出每天策略的总数

## Figure 9-13
plot(table(daily.num[start:n.days]), type="l", main="daily strat num", xlab="#strat", ylab="#day")
## 策略数目分布

strat.num <- apply(available,2,sum)
## Figure 9-14
plot(strat.num, type="l")

dim(available)

strat.index <- t(t(available)*(1:n.strat))
## Figure 9-15
plot(rep(1:n.days,n.strat), strat.index, type="n", ylab="strat")
for (i in 1:n.strat)
  points(1:n.days, strat.index[,i], col=i)


all.rolling.pnl <- matrix(0, nrow=n.days, ncol=n.strat)
for (i.rank in 1:n.strat) {
  rolling.pnl <- rep(0,n.days)  
  available <- matrix(FALSE, nrow=n.days, ncol=n.strat)
  for (cur.day in (contract.day.count[3]+1):(n.days-1)) {
    available[cur.day,] <- rolling.recent.drawdown[cur.day,]<0.1 & rank.avg.profit[cur.day,]<=i.rank & rank.sharp[cur.day,]<=i.rank 
    available[cur.day,c(6,7,12)] <- FALSE ## 删除这些策略
    each.pnl <- each.day.pnl[cur.day+1,available[cur.day,]]
    if (length(each.pnl)>0) rolling.pnl[cur.day+1] <- sum(each.pnl)/length(each.pnl)
  }
  all.rolling.pnl[,i.rank] <- rolling.pnl
}

pnl.sum <- apply(all.rolling.pnl, 2, sum)
plot(pnl.sum, type="l")
best <- which.max(pnl.sum)
best
## [1] 6
rolling.best <- cumsum(all.rolling.pnl[(contract.day.count[3]+1):n.days,best])
backtest.best <- daily.pnl[(contract.day.count[3]+1):n.days,8]-daily.pnl[contract.day.count[3],8]
## 此时6、7、12已经消失，全局最优的是第8个策略
best.len <- length(rolling.best)

## Figure 9-16
plot(rep(1:best.len,2), c(rolling.best, backtest.best), type="n", ylab="pnl")
points(rolling.best, type="l",col=1)
points(backtest.best, type="l", col=2)
legend("topleft", legend=c("rolling","best"), col=1:2, lty=1)


optimize.update.len <- rep(0,80) ## 不同更新频率的结果
update.list <- 1:80 ## 所有更新频率
for (update.len in update.list) { ## 遍历所有更新频率
  all.rolling.pnl <- matrix(0, nrow=n.days, ncol=n.strat) ## 每种频率的结果
  cat(update.len,"\n")
  for (i.rank in 1:n.strat) {
    rolling.pnl <- rep(0,n.days)  
    available <- matrix(FALSE, nrow=n.days, ncol=n.strat)
    for (cur.day in (contract.day.count[3]+1):(n.days-update.len)) { ## 靠近结尾不更新
      if (cur.day %% update.len!=0) next ## 不需要更新
      available[cur.day,] <- rolling.recent.drawdown[cur.day,]<0.1 & rank.avg.profit[cur.day,]<=i.rank & rank.sharp[cur.day,]<=i.rank 
      for (i in 1:(update.len-1)) ## 更新策略后，后面几天也是这个策略
        available[cur.day+i,] <- available[cur.day,]
      for (i in 1:update.len) { ## 计算后面几天到盈亏
        each.pnl <- each.day.pnl[cur.day+i,available[cur.day,]]
        if (length(each.pnl)>0) rolling.pnl[cur.day+i] <- sum(each.pnl)/length(each.pnl)
      }
    }
    all.rolling.pnl[,i.rank] <- rolling.pnl
  }
  pnl.sum <- apply(all.rolling.pnl, 2, sum)
  optimize.update.len[update.len] <- max(pnl.sum) ## 取最优的策略
}
## Figure 9-17
plot(cumsum(optimize.update.len)/(1:80), type="l", xlab="frequency", ylab="pnl") ## 画出随着更新频率提高的结果


