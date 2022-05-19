## chapter 6
rm(list=ls())
source("d:/liwei/book/code/helper.r")
product <- "rb"
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

load(file=paste("d:/liwei/book/data/",product,".new.RData",sep=""), verbose=TRUE)
period <- 16
past.return <- cum(all.data$wpr.log.ret, period)
trend.index <- rep(0,nrow(thre.mat))
drawdown <- apply(mm.pnl.mat, 2, function(x) {
  max(cummax(x)-x)
})
thre.mat$drawdown <- drawdown[thre.mat$contract]
thre.mat$ret.drawdown <- thre.mat$pnl/thre.mat$drawdown
for (i in 1:nrow(thre.mat)) {
  contract <- thre.mat$contract[i]
  action <- c(pos.mat[1,contract],diff(pos.mat[,contract]))
  draw.points <- which(action!=0)
  trend.following <- sum(sign(past.return[draw.points])==sign(action[draw.points]), na.rm=TRUE)
  n.action <- length(draw.points)
  trend.index[i] <- trend.following/n.action
}
thre.mat$trend.index <- trend.index
thre.mat$pnl <- round(thre.mat$pnl*10)
thre.mat$drawdown <- round(thre.mat$drawdown*10)
date.time <- get.date.time(data.date, data.time)
all.data <- all.data[1:length(date.time),]
recent.drawback <- apply(mm.pnl.mat,2, function(x) 1-tail(x,1)/max(x))
thre.mat$recent.drawback <- recent.drawback[thre.mat$contract]

target <- 4
par(mar=c(5,4,3,3)+0.3)
plot(date.time, mm.pnl.mat[,target]*10, type="l", xlab="time", ylab="pnl", main="rb strategy")
par(new=TRUE)
plot(date.time, all.data$price, type="l", xlab="", ylab="", bty="n", axes=FALSE)
action <- c(pos.mat[1,target],diff(pos.mat[,target]))
draw.points <- which(action!=0)
n.line <- length(draw.points)-1
i <- 1
while (i<=n.line) {
  open <- draw.points[i]
  close <- draw.points[i+1]
  color <- ifelse(action[open]>0,2,3)
  points(date.time[open:close], all.data$price[open:close], type="l", col=color)
  i <- i+1
  while (pos.mat[close,target]==0) {
    i <- i+1
    if (i>n.line) break
    close <- draw.points[i]
  }
}
## Figure 6-1
axis(side=4, at=pretty(range(all.data$price)))
mtext("price", side=4, line=2)
legend("bottom", legend=c("long","short"), lty=1, col=2:3)
legend("top", legend="pnl", lty=1, col=1)

## Table 6-1
thre.mat[1,c("sharp","pnl","prob","ratio","num","drawdown","ret.drawdown")]

## Table 6-2
thre.mat[,c("sharp","pnl","prob","ratio","num","drawdown","ret.drawdown")]

## Figure 6-2
plot(date.time, mm.pnl.mat[,6]*10, type="l", xlab="time", ylab="pnl", main="rb strategy")

## Figure 6-3
plot(date.time, mm.pnl.mat[,5]*10, type="l", xlab="time", ylab="pnl", main="rb strategy")

## Following with the book, may be duplicate but that's fine
for (i in 1:nrow(thre.mat)) { ## 遍历所有策略
  contract <- thre.mat$contract[i] ## 读取策略的合约代买
  action <- c(pos.mat[1,contract],diff(pos.mat[,contract])) ## 读取相应的买卖交易信号
  draw.points <- which(action!=0) ## 记录有交易动作的地方
  trend.following <- sum(sign(past.return[draw.points])==sign(action[draw.points])) ## 顺势交易统 ## 计
  n.action <- length(draw.points) ## 总共交易点
  trend.index[i] <- trend.following/n.action ## 顺势交易的比例
}
thre.mat$trend.index <- trend.index ## 顺势交易指标
thre.mat$trend.index[1]
## [1] 0.8088235

### Figure 6-4
target <- 10
par(mar=c(5,4,3,3)+0.3)
plot(date.time, mm.pnl.mat[,target]*10, type="l", xlab="time", ylab="pnl", main="rb strategy")
par(new=TRUE)
plot(date.time, all.data$price, type="l", xlab="", ylab="", bty="n", axes=FALSE)
action <- c(pos.mat[1,target],diff(pos.mat[,target]))
draw.points <- which(action!=0)
n.line <- length(draw.points)-1
i <- 1
while (i<=n.line) {
  open <- draw.points[i]
  close <- draw.points[i+1]
  color <- ifelse(action[open]>0,2,3)
  points(date.time[open:close], all.data$price[open:close], type="l", col=color)
  i <- i+1
  while (pos.mat[close,target]==0) {
    i <- i+1
    if (i>n.line) break
    close <- draw.points[i]
  }
}

## Table 6-3
thre.mat[target,c("sharp","pnl","prob","ratio","num","drawdown","ret.drawdown")]

period <- 32 ## 回看周期
past.return <- cum(all.data$wpr.log.ret, period) ## 过去的收益率
trend.index <- rep(0,nrow(thre.mat)) ## 顺势交易指标
for (i in 1:nrow(thre.mat)) { ## 遍历所有策略
  contract <- thre.mat$contract[i] ## 读取策略的编号
  action <- c(pos.mat[1,contract],diff(pos.mat[,contract])) ## 交易的动作
  draw.points <- which(action!=0) ## 有交易的地方
  trend.following <- sum(sign(past.return[draw.points])==sign(action[draw.points])) ## 顺势交易
  n.action <- length(draw.points) ## 总共交易次数
  trend.index[i] <- trend.following/n.action ## 顺势交易比例
}
thre.mat$trend.index <- trend.index ## 顺势交易指标
thre.mat$trend.index[c(1,7)]
## [1] 0.9558824 0.9177215


load(file="d:/liwei/book/data/thre.mat.22.RData")
## Table 6-4
thre.mat.22[,c("open","close","sharp","pnl","prob","ratio","num","drawdown")]

## Figure 6-5
plot(thre.mat.22$close[1:6], thre.mat$sharp[1:6], type="l", xlab="close",ylab="sharp")

load(file="d:/liwei/book/data/thre.mat.36.RData")
## Table 6-5
close.opt$thre[,c("open","close","sharp","pnl","prob","ratio","num","drawdown")]
## Figure 6-6
plot(close.opt$date.time, close.opt$mm.pnl.mat[,5]*10, type="l", xlab="date time", ylab="pnl",
     main="rb high threshold")

## Figure 6-7
plot(close.opt$thre$close[1:10], close.opt$thre$sharp[1:10], type="l", xlab="close",ylab="sharp")

thre.mat$avg.profit <- thre.mat$pnl/thre.mat$num ## 总利润除以交易次数
recent.drawback <- apply(mm.pnl.mat,2, function(x) 1-tail(x,1)/max(x))
thre.mat$recent.drawback <- recent.drawback[thre.mat$contract]

## Table 6-6
thre.mat[,c("sharp","pnl","prob","ratio","avg.profit", "num","recent.drawback", "drawdown")]
remain <- thre.mat$avg.profit>100 & thre.mat$recent.drawback<0.1
## Table 6-7
thre.mat[remain, c("sharp","pnl","prob","ratio","avg.profit", "num","recent.drawback", "drawdown")]
