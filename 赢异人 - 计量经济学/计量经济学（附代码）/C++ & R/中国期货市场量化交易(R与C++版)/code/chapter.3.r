#### 
## chapter 3.1
source("d:/liwei/book/code/helper.r")
product <- "rb"
all.contracts <- get.dates(product)
all.contracts
n.contract <- length(all.contracts)
contract <- all.contracts[n.contract-1]
CONTINUOUS.PATH <- "d:/liwei/book/binary"
setwd(CONTINUOUS.PATH)
load(contract)
## Figure 3-1
plot(data$date.time, data$price, type="l", xlab="date", ylab="price", main=contract)
points(data$date.time[data$continuous], data$price[data$continuous], col=2, type="l")

library(TTR)
lsp <- function(package, all.names = FALSE, pattern) {
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"), 
    all.names = all.names, 
    pattern = pattern
  )
}
lsp(TTR)
period <- 16 ## 研究周期
adx.5m <- function(high,low,close,period) {
  return (clean(ADX(cbind(high,low,close),n=period)[,3]/100-0.5))
}

fcum <- function(x, n, na.rm = TRUE, fill = 0) {
  if (na.rm) return(head(lag(cum(c(x, rep(fill, n)), n), -n), -n))
  else return(lag(cum(x, n), -n, fill = fill))
}

cor.vec <- as.numeric(sapply(all.contracts, function(contract){ ## 指标与收益的相关系数，每段
  ## 合约1个数字
  load(contract)
  x <- with(data, adx.5m(high, low, close, period))[data$continuous]
  y <- with(data, fcum(wpr.log.ret, period))[data$continuous]
  cor(x,y)
}))
cor.vec
mean(cor.vec)/sd(cor.vec)
## [1] -0.2655112
sum(cor.vec>0)/length(cor.vec)
## [1] 0.3333333
setwd(CONTINUOUS.PATH)
aroon.5m <- function(high,low,period) {
  return (clean(aroon(cbind(high,low),n=period)[,3])/100)
}
cor.vec <- as.numeric(sapply(all.contracts, function(contract){
  load(contract)
  x <- with(data, aroon.5m(high, low, period))[data$continuous]
  y <- with(data, fcum(wpr.log.ret, period))[data$continuous]
  cor(x,y)
}))
cor.vec
mean(cor.vec)/sd(cor.vec)
## 0.451995

size.imb <- function(bid.size,ask.size) {
  return (clean((bid.size-ask.size)/(bid.size+ask.size)))
}
cor.vec <- as.numeric(sapply(all.contracts, function(contract){
  load(contract)
  x <- with(data, size.imb(bid.qty, ask.qty))[data$continuous]
  y <- with(data, fcum(wpr.log.ret, period))[data$continuous]
  cor(x,y)
}))
cor.vec
mean(cor.vec)/sd(cor.vec)
## 0.1539977

bbands.5m <- function(high,low,close,period) { ## 5分钟布林带指标
  return (clean(BBands(cbind(high,low,close),period)[,4]))
}
cor.vec <- as.numeric(sapply(all.contracts, function(contract){
  load(contract)
  x <- with(data, bbands.5m(high, low, close, period))[data$continuous]
  y <- with(data, fcum(wpr.log.ret, period))[data$continuous]
  cor(x,y)
}))
cor.vec
mean(cor.vec)/sd(cor.vec)
## [1] 0.4645663

cci.5m <- function(high,low,close,period) {
  return (clean(CCI(cbind(high,low,close),period))*0.001)
}
cor.vec <- as.numeric(sapply(all.contracts, function(contract){
  load(contract)
  x <- with(data, cci.5m(high, low, close, period))[data$continuous]
  y <- with(data, fcum(wpr.log.ret, period))[data$continuous]
  cor(x,y)
}))
cor.vec
mean(cor.vec)/sd(cor.vec)
## [1] 0.4590126

clv.5m <- function(high,low,close) {
  return (clean(CLV(cbind(high,low,close))))
}
cor.vec <- as.numeric(sapply(all.contracts, function(contract){
  load(contract)
  x <- with(data, clv.5m(high, low, close))[data$continuous]
  y <- with(data, fcum(wpr.log.ret, period))[data$continuous]
  cor(x,y)
}))
cor.vec
mean(cor.vec)/sd(cor.vec)
## [1] 0.7873691

cmf.5m <- function(high,low,close,qty,period) {
  return (clean(CMF(cbind(high,low,close),qty,period)))
}
cor.vec <- as.numeric(sapply(all.contracts, function(contract){
  load(contract)
  x <- with(data, cmf.5m(high, low, close, qty, period))[data$continuous]
  y <- with(data, fcum(wpr.log.ret, period))[data$continuous]
  cor(x,y)
}))
cor.vec
mean(cor.vec)/sd(cor.vec)
## [1] 0.630666


dark.cloud <- function(open,close,ret,ratio=0.25) {
  one.quar <- open+(close-open)*ratio
  pre.one.quar <- c(one.quar[1], head(one.quar,-1))
  pre.open <- c(open[1], head(open,-1))
  pre.ret <- c(0,head(ret,-1))
  result <- ifelse((ret<0 & pre.ret>0 & close<pre.one.quar & close>pre.open),1,0)-0.5
  return (result)
}


piercing <- function(open,close,ret,ratio=0.25) { ## 穿刺指标
  one.quar <- close-(close-open)*ratio
  pre.one.quar <- c(one.quar[1], head(one.quar,-1))
  pre.open <- c(open[1], head(open,-1))
  pre.ret <- c(0,head(ret,-1))
  result<- (ret>0 & pre.ret<0 & close>pre.one.quar & close<pre.open)
  return (result)
}

morning.star <- function(open, close, body,thre) { ## 启明星指标
  move<- close-open
  pre.move <- c(0,head(move,-1))
  pre.close <- c(close[1], head(close,-1))
  jump<- open-pre.close 
  pre.body <- c(body[1],head(body,-1))
  ratio<- ifelse(pre.body==0,0,body/pre.body)
  result<- rep(0,length(open))
  chosen<- which(jump<0 & pre.move<0 & move>0 & ratio<thre)
  result[chosen] <- -1
  return(result)
}

###### chapter 3.2
log.ret <- c()
contract.len <- c()
mean.ret <- c()
sd.ret <- c()
for (contract in all.contracts) {
  load(contract)
  y <- with(data, fcum(wpr.log.ret, period))[data$continuous]
  print(Box.test(y)$p.value)
  log.ret <- c(log.ret, y)
  contract.len <- c(contract.len, sum(data$continuous))
  mean.ret <- c(mean.ret, mean(y))
  sd.ret <- c(sd.ret, sd(y))
}
mean(log.ret)
## [1] -0.00007017801
sd(log.ret)
## [1] 0.00713672
mean.ret
## Figure 3-2
plot(mean.ret, type="l", main="return")
sd.ret
## Figure 3-3
plot(sd.ret, type="l", main="standard deviation")
## Figure 3-4
plot(log.ret, type="l", main="return")

############ change

ret <- c()
contract.len <- c()
mean.ret <- c()
sd.ret <- c()
for (contract in all.contracts) {
  load(contract)
  change <- c(0, diff(data$wpr))
  y <- with(data, fcum(change, period))[data$continuous]
  ret <- c(ret, y)
  contract.len <- c(contract.len, sum(data$continuous))
  mean.ret <- c(mean.ret, mean(y))
  sd.ret <- c(sd.ret, sd(y))
}
mean(ret)
## [1] -0.2581286
sd(ret)
## [1] 20.08028
mean.ret
## Figure 3-5
plot(mean.ret, type="l", main="return")
sd.ret
## Figure 3-6
plot(sd.ret, type="l", main="standard deviation")

setwd(CONTINUOUS.PATH)
cor.vec <- as.numeric(sapply(all.contracts, function(contract){
  load(contract)
  x <- with(data, adx.5m(high, low, close, period))[data$continuous]
  change <- c(0, diff(data$wpr))
  y <- with(data, fcum(change, period))[data$continuous]
  cor(x,y)
}))
cor.vec
mean(cor.vec)/sd(cor.vec)
## [1] -0.2610164
sum(cor.vec>0)/length(cor.vec)
## [1] 0.3333333

cor.vec <- as.numeric(sapply(all.contracts, function(contract){
  load(contract)
  x <- with(data, clv.5m(high, low, close))[data$continuous]
  change <- c(0, diff(data$wpr))
  y <- with(data, fcum(change, period))[data$continuous]
  cor(x,y)
}))
cor.vec
mean(cor.vec)/sd(cor.vec)
## [1] 0.7870768
sum(cor.vec>0)/length(cor.vec)

library(wavelets)
all.ret <- c()
len <- c()
for (contract in all.contracts) {
  load(contract)
  x <- with(data, clv.5m(high, low, close))[data$continuous]
  change <- c(0, diff(data$wpr))
  y <- with(data, fcum(change, period))[data$continuous]
  all.ret <- c(all.ret, y)
  len <- c(len, sum(data$continuous))
}
ret.ts <- as.ts(all.ret)
## Figure 3-7
plot(ret.ts, type="l")


model<- dwt(ret.ts, filter="haar", n.levels=3, boundary="reflection")
imodel<- idwt(model, fast=TRUE)
length(imodel)
## [1] 74565
length(ret.ts)
cor(imodel, ret.ts)
## [1] 0.9070177

#### high frequency indicators

model<- dwt(ret.ts, filter="haar", n.levels=3, boundary="periodic")
imodel<- idwt(model, fast=TRUE)
length(imodel)
length(ret.ts)
cor(imodel, ret.ts)
## [1] 0.7274209


model<- dwt(ret.ts, filter="haar", n.levels=4, boundary="reflection")
imodel<- idwt(model, fast=TRUE)
length(imodel)
length(ret.ts)
cor(imodel, ret.ts)
## [1] 0.4866929


model<- dwt(ret.ts, filter="haar", n.levels=3, boundary="reflection")
imodel<- idwt(model, fast=TRUE)
cor.vec <- c()
all.x <- c()
start <- 1
end <- 0
i <- 1
for (contract in all.contracts) {
  load(contract)
  x <- with(data, clv.5m(high, low, close))[data$continuous]
  all.x <- c(all.x,x)
  end <- end+len[i]
  i <- i+1
  cor.vec <- c(cor.vec, cor(x,imodel[start:end]))
  start <- end+1
}
mean(cor.vec)/sd(cor.vec)
## [1] -2.070912
cor.vec
sum(cor.vec<0)/length(cor.vec)
## [1] 0.9333333

## chaptter 3.3 high freqeuncy indicators

range <- 1:40 ## 取1-40天的数据
product <- "rb" ## 品种是螺纹钢
#all.data <- prepare.tick.data(signal.list, y.str, product, all.dates[range]) ## 准备分笔数据
## we do not have prepare.tick.data here, so we just load the data instead
load("d:/liwei/book/data/hft.signal.RData")
dim(all.data)
## [1] 1609232      12

size.imb<- function(bid, ask, bid.size, ask.size) { ## 买卖不平衡指标
  return ((bid.size-ask.size)/(bid.size+ask.size)/(ask-bid)/(ask+bid)*2);
}

all.data$id <- ceiling(1:nrow(all.data)/100000) ## 分笔数据分组
library(plyr) ## 调用plyr包
sample.cor <- ddply(all.data, "id", summarise, corr=cor(size.dif, y)) ## 每组数据内部相关性
## Figure 3-8
plot(sample.cor$corr, type="l", main="size.imb")

trade.imb<- function(bid, ask, volume, turnover,period) { ## 成交量买卖不平衡
  get.trade <- active.trade(bid, ask, volume, turnover)
  buy.trade <- cum(get.trade$buy.trade,period)
  sell.trade <- cum(get.trade$sell.trade,period)
  buy.trade[1:period] <- 0
  sell.trade[1:period] <- 0
  volume[1] <- 0
  cum.volume <- cumsum(volume)/(1:length(volume))*period
  signal<- (buy.trade-sell.trade) %0/% cum.volume
  return (signal)
}

sample.cor <- ddply(all.data, "id", summarise, corr=cor(trade.imb.32, y))
## Figure 3-9
plot(sample.cor$corr, type="l", main="trade.imb.32", ylab="cor")

ewma.move<- function(bid, ask, bid.size, ask.size, period) { ## 标准化的指数加权平均因子
  wpr<- clean.x((bid*ask.size+ask*bid.size)/(bid.size+ask.size))
  signal<- clean.x((1-ewma(wpr, period)/wpr))
  first.negative <- which(signal<0)[1]
  signal[1:first.negative] <- 0
  return (signal)
}

sample.cor <- ddply(all.data, "id", summarise, corr=cor(ewma.move.32, y))
## Figure 3-10
plot(sample.cor$corr, type="l", main="ewma.move.32", ylab="cor")
## we don't have get.tick.lasso.cross, so we load tick.model.RData instead
#system.time(tick.model <- get.tick.lasso.cross(range=1:40, product, 
#                                               signal.list, y.str,chunk=chunk)) ## 用cross-validation的lasso模型进行建模
load(file="d:/liwei/book/data/tick.model.RData")
tick.model$best
names(tick.model)
## 调出模型
tick.model$coef <- tick.model$coef.mat[,66] ## 设置模型系数为最佳模型的系数
chosen <- which(tick.model$coef!=0) ## 过滤出非零系数
signals <- tick.model$signals[chosen]
coef <- tick.model$coef[chosen]
length(coef)
## [1] 11
all.dates <- get.dates(product)
r2 <- rep(0,n.dates)

## we don't have prepare.tick.data, so we load r2.66.RData instead
#for (i in 1:n.dates) {
#     date <- all.dates[i]
#     data <- prepare.tick.data(signals, y.str, product, date)
#     pred <- as.matrix(data[,1:(ncol(data)-1)]) %*% coef
#     r2[i] <- R2(pred, data$y)
#     save(pred, file=paste("d:/liwei/pred tick/", product, "/", date, ".RData", sep=""))
#}

load(file="d:/liwei/book/data/r2.66.RData")

train <- 1:40
test <- 41:150
mean(r2[train])
## [1] 0.0174052
median(r2[train])
## [1] 0.0164109
mean(r2[test])
## [1] 0.01037621
median(r2[test])
## [1] 0.01083499


load(file="d:/liwei/book/data/r2.45.RData")
train <- 1:40
test <- 41:150
mean(r2[train])
## [1] 0.01592312
median(r2[train])
## [1] 0.01533144
mean(r2[test])
## [1] 0.0119505
median(r2[test])
## [1]  0.01220418


load(file="d:/liwei/book/data/r2.35.RData")
train <- 1:40
test <- 41:150
mean(r2[train])
## [1] 0.01449562
median(r2[train])
## [1] 0.01440146
mean(r2[test])
## [1] 0.01441228
median(r2[test])
## [1]  0.01355247

