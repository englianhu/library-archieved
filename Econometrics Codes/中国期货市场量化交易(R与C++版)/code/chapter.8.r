
### chapter 8
source("d:/liwei/book/code/helper.r")
library(MTS)
product <- "rb"
load(file=paste("d:/liwei/book/data/",product,".new.RData",sep=""), verbose=TRUE)
end.day <- data.time=="15:00:00"
thre.mat
daily.ret <- diffM(mm.pnl.mat[end.day,])
chosen <- 4:10
data <- daily.ret[,chosen] ## 相关策略的每日收益数据
##colnames(data) <- strat.name[chosen] ## 给策略的列命名
thre.mat[chosen,] ## 相关策略的绩效统计
s_m = colMeans(data) ## 策略的收益率均值
s_cov = cov(data) ## 策略的收益率方差
m_grid = seq(min(s_m), max(s_m), length = 30) ## 目标均值网格
cov_grid=c() ## 方差取值
for (i in 1:length(m_grid)) { ## 遍历所有均值
  w=get.weight(s_m, s_cov, m_grid[i]) ## 计算投资组合权重
  cov_grid[i]=sqrt(t(w) %*% s_cov %*% w) ## 计算协方差矩阵
}

## Figure 8-1 may be different but shape is similar
plot(cov_grid, m_grid, type = "l") ## 画出有效前沿

which.min(cov_grid)
w=get.weight(s_m, s_cov, m_grid[13]) ## 得到

as.numeric(w) ## it may be very different from the book because it uses different data set
## the original data set is missing
## [1]  0.073935469  0.004435945  0.582951249  0.158493746 -0.056735730  0.171044231  0.065875090

library(quadprog)
n <- length(s_m)
sol <- solve.QP(Dmat=as.matrix(s_cov), ## 使用solve.QP求解
                dvec=rep(0,n), 
                Amat=t(as.matrix(rbind(as.numeric(s_m),rep(1,n)))), 
                bvec=c(m_grid[13],1), 
                meq=2)
sol$solution


cov_grid=c() ## 方差网格
good <- rep(TRUE, length(m_grid))
for (i in 2:(length(m_grid)-1)) { ## 遍历所有目标收益
  w=get.weight(s_m, s_cov, m_grid[i])
  sol <- solve.QP(Dmat=as.matrix(s_cov), 
                  dvec=rep(0,n), 
                  Amat=t(rbind(as.numeric(s_m),rep(1,n),diag(n))), 
                  bvec=c(m_grid[i],1,rep(0,n)),  ## 加入非负约束
                  meq=2)
  w <- sol$solution
  cov_grid[i]=sqrt(t(w) %*% s_cov %*% w) ## 计算方差
  if (sum(w<0.001)>0) good[i] <- FALSE
}

## Figure 8-3 the shape is similar but detail is different
plot(cov_grid[2:(length(m_grid)-1)], m_grid[2:(length(m_grid)-1)], type="l",
     xlab="covariance", ylab="mean") ## 画出有效前沿


best <- which.min(cov_grid) ## 找出方差最小的解
sol <- solve.QP(Dmat=as.matrix(s_cov), 
                dvec=rep(0,n), 
                Amat=t(rbind(as.numeric(s_m),rep(1,n),diag(n))), 
                bvec=c(m_grid[best],1,rep(0,n)), 
                meq=2) ## 求出最佳权重
w.best <- sol$solution ## 最佳权重
w.best
total.qty <- 100 ## 默认100手
weight <- floor(w.best*total.qty) ## 向下取整
weight
## [1]  5  0 58 13  0 19  3 it's different because data is different
pnl <- as.matrix(data) %*% weight/sum(weight) ## 计算资金曲线
## Figure 8-4, shape is similar but plot is different
plot(cumsum(pnl), type="l", ylab="pnl", main="optimal portfolio") ## 画出资金曲线

cov_grid=rep(NA,30) ## 投资组合的方差
good <- rep(TRUE, length(m_grid)) ## 满足条件的模型
for (i in 2:(length(m_grid)-1)) { ## 遍历所有期望收益
  w=get.weight(s_m, s_cov, m_grid[i]) ## 计算当前期望收益下的投资组合权重
  sol <- NULL
  try(sol <- solve.QP(Dmat=as.matrix(s_cov), 
                      dvec=rep(0,n), 
                      Amat=t(rbind(as.numeric(s_m),rep(1,n),diag(n), -diag(n))), 
                      bvec=c(m_grid[i],1,rep(0,n), rep(-0.25,n)),  ## 加入不超过25%的限制
                      meq=2), silent=TRUE)
  if (is.null(sol)) next
  w <- sol$solution
  cov_grid[i]=sqrt(t(w) %*% s_cov %*% w) ## 计算单个投资组合的方差
}
best <- which.min(cov_grid) ## 找到最优解
sol <- solve.QP(Dmat=as.matrix(s_cov), 
                dvec=rep(0,n), 
                Amat=t(rbind(as.numeric(s_m),rep(1,n),diag(n), -diag(n))), 
                bvec=c(m_grid[best],1,rep(0,n), rep(-0.25,n)),  ## 加入不超过25%的限制
                meq=2)

w.best <- sol$solution ## 找到最优解
w.best
total.qty <- 100 ## 交易100手
weight <- round(w.best*total.qty) ## 最佳手数配比
weight
## [1] 16  7 25 18  0 23 11 result is different

pnl <- as.matrix(data) %*% weight/sum(weight) ## 计算资金曲线
## Figure 8-5, conclusion is the same but plot is different
plot(cumsum(pnl), type="l", ylab="pnl", main="optimal portfolio")  ##

weight <- rep(1,n)
pnl <- as.matrix(data) %*% weight/sum(weight) ## 等手数的资金曲线
## Figure 8-6, plot is different
plot(cumsum(pnl), type="l", ylab="pnl", main="equal weight")  ## 画出等权重的资金曲线

weight <- round(w.best*total.qty) ## 最佳手数配比
weight
## [1] 16  7 25 18  0 23 11 result is different
pnl.opt <- as.matrix(data) %*% weight/sum(weight) ## 计算资金曲线

## Figure 8-7
plot(cumsum(pnl), type="l", ylab="pnl", main="equal weight")  ## 画出等权重的资金曲线
points(cumsum(pnl.opt), type="l", col=2)
legend("topleft", legend=c("equal","optimal"), col=1:2, lty=1)


## equal capital
chosen.product <- c("rb", "hc", "ru", "cu", "zn", "bu", "ag", "au")
price.list <- c(3695, 3978, 13495, 31580, 26070, 2428, 3874, 278.90)
n.product <- length(chosen.product)
contract.value <- rep(0,n.product)
for (i in 1:n.product) 
  contract.value[i] <- price.list[i]*product.info[[chosen.product[i]]]$multiplier
capital <- 1e6
weight <- round(capital/contract.value)
weight
total.days <- 2000
for (product in chosen.product) {
  load(file=paste("d:/liwei/filtered opt/",product,".new.RData",sep=""))
  end.day <- which(data.time=="15:00:00")
  if (length(end.day)<total.days) total.days <- length(end.day)
}
total.days

all.pnl <- matrix(0, nrow=total.days, ncol=n.product)
colnames(all.pnl) <- chosen.product
all.pnl <- as.data.frame(all.pnl)
for (product in chosen.product) {
  load(file=paste("d:/liwei/filtered opt/",product,".new.RData",sep=""))
  recent.drawdown <- apply(mm.pnl.mat, 2, function(x)  1-clean(tail(x,1)/max(x)))
  thre.mat$recent.drawdown <- recent.drawdown[thre.mat$contract]
  chosen <- thre.mat$contract[which.min(thre.mat$recent.drawdown)]
  end.day <- which(data.time=="15:00:00")
  pnl <- mm.pnl.mat[end.day,chosen]
  pnl <- tail(pnl, total.days)
  pnl <- pnl-pnl[1]
  all.pnl[,product] <- pnl*product.info[[product]]$multiplier
}
portfolio <- as.matrix(all.pnl) %*%　weight

## Figure 8-8, this one is the same as that in the book
plot(portfolio, type="l")

## Table 8-1
as.data.frame(get.each.pnl.stat(diff(portfolio)))
tail(portfolio,1)/max(cummax(portfolio)-portfolio)

library(MTS)
library(quadprog)
data <- rbind(all.pnl[1,], diffM(all.pnl))
save(data, file="d:/liwei/book/data/all.product.pnl.RData")
s_m = colMeans(data)
s_cov = cov(data)
m_grid = seq(min(s_m), max(s_m), length = 30)
cov_grid=rep(NA,30)
good <- rep(TRUE, length(m_grid))
n <- ncol(data)
upper.bound <- 0.15
lower.bound <- 0.02
for (i in 2:(length(m_grid)-1)) {
  sol <- NULL
  try(sol <- solve.QP(Dmat=as.matrix(s_cov), 
                      dvec=rep(0,n), 
                      Amat=t(rbind(as.numeric(s_m),rep(1,n),diag(n), diag(n), -diag(n))), 
                      bvec=c(m_grid[i],1,rep(0,n), rep(lower.bound,n), rep(-upper.bound,n)), 
                      #Amat=t(rbind(as.numeric(s_m),rep(1,n),diag(n))), 
                      #bvec=c(m_grid[i],1,rep(0,n)), 
                      meq=2),silent = TRUE)
  if (is.null(sol)) next
  w <- sol$solution
  cov_grid[i]=sqrt(t(w) %*% s_cov %*% w)
}

plot(cov_grid[2:(length(m_grid)-1)], m_grid[2:(length(m_grid)-1)], type="l",
     xlab="covariance", ylab="mean")
points(cov_grid[good], m_grid[good], col=2, type="l")
best <- which.min(cov_grid)
best
sol <- solve.QP(Dmat=as.matrix(s_cov), 
                dvec=rep(0,n), 
                Amat=t(rbind(as.numeric(s_m),rep(1,n),diag(n), diag(n), -diag(n))), 
                bvec=c(m_grid[best],1,rep(0,n), rep(lower.bound,n), rep(-upper.bound,n)), 
                meq=2)

w.best <- sol$solution
w.best
## [1] 0.15000000 0.15000000 0.15000000 0.05222534 0.15000000 0.15000000 0.13613205 0.06164261
multiplier <- rep(0,n.product)
for (i in 1:n.product)
  multiplier[i] <- product.info[[chosen.product[i]]]$multiplier
mv.weight <-round(w.best*capital/contract.value*n.product)
mv.weight
## [1] 32 30  9  3  9 49 19  2
portfolio.mv <- as.matrix(all.pnl) %*%　mv.weight

## Figure 8-10
plot(portfolio.mv, type="l")
points(portfolio, type="l", col=2)
legend("topleft", legend=c("mean-variance","equal capital"), lty=1, col=1:2)

## Table 8-2
as.data.frame(get.each.pnl.stat(diff(portfolio.mv)))
max(cummax(portfolio.mv)-portfolio.mv)
tail(portfolio.mv,1)/max(cummax(portfolio.mv)-portfolio.mv)


