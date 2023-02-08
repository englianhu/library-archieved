## chapter 7
source("d:/liwei/book/code/helper.R")

get.y.end <- function(ret, qty, qty.chunk) { ## 按等成交量划分K线
  len <- length(qty) ## 收益数据的长度
  y.end <- rep(0,len) ## 分割点的位置初始化
  y.equal.volume <- rep(0,len) ## 等成交量数据
  for (tick in 1:(len-1)) { ## 遍历全部行情
    total <- 0 ## 成交量累加
    i <- tick
    while (i<len & total<qty.chunk) { ## 找到划分点
      i <- i+1
      total <- total+qty[i]
    }
    y.end[tick] <- i ## 赋值
    y.equal.volume[tick] <- sum(ret[(tick+1):i])
  }
  y.end[len] <- len
  return (y.end) ## 返回分割点
}

PATH <- "d:/liwei/book/data"
get.data <- function(s,d) {
  load(paste(PATH,"/", s, "/", d,sep=""))
  return (data)
}
product <- "rb"
all.dates <- list.files("d:/liwei/book/data/rb")
data <-get.data(product, all.dates[1])

ys <- fcum(data$ret,32)
## Figure 7-1 may be different because it uses a different day
plot(ys, type="l")

debug(get.y.end)


system.time(for (date in all.dates[1:100]) {
  data <- get.data(product, date)
  y.slow <- get.y.end(data$ret, data$qty, 4000)
})


## 用户   系统   流逝 
## 785.30   0.31 797.07 

library(Rcpp)
sourceCpp("d:/liwei/book/code/getVolumeSplit.cpp")
system.time(for (date in all.dates[1:100]) { ## 处理前100天
     data <- get.data(product, date) ## 读取数据
     y.fast <- getVolumeSplit(data$qty, 4000) ## 等成交量划分K线
})
## 用户  系统  流逝 
## 37.56  0.22 38.73 

all.equal(y.slow,y.fast)
##[1] TRUE

cl <- makeCluster(20) ## 使用20个核
## Lack of get.daily.tick.stat, could not be run
## just give the format of using parSapply
#system.time(result <- parSapply(cl, all.dates, get.daily.tick.stat, product, thre=thre, scratch=-1,spread=product.info[[product]]$spread,
#                                slippage=0,strat.symbol=".add"))
stopCluster(cl)

library(gpuR) ## 调用gpu

## cannot be run, just give an idea how to use gpuR
## gpu.coef <- gpuMatrix(matrix(coef, ncol=1),type="double") ## 生成gpu系数矩阵
## gpu.data <- gpuMatrix(as.matrix(data[,1:(ncol(data)-1)]), type="double") ## 生成gpu数据矩阵
## pred <- (gpu.data %*% gpu.coef)[] ## 用gpu计算预测值


library(gpuR) ## 调用gpuR的库
ORDER = 1024 ## 规模大小
A = matrix(rnorm(ORDER^2), nrow=ORDER) ## 构建矩阵A
B = matrix(rnorm(ORDER^2), nrow=ORDER) ## 构建矩阵B
## some device supports double while some supports only float
gpuA = gpuMatrix(A, type="float") ## 矩阵A转成gpu矩阵
gpuB = gpuMatrix(B, type="float") ## 矩阵B转成gpu矩阵
system.time(for(i in 1:100) C <- A %*% B) ## 计算矩阵相乘

## 用户   系统   流逝 
## 103.95   0.04 105.58 
system.time(for(i in 1:100) gpuC = gpuA %*% gpuB) ## 重复100次计算矩阵乘法
## 用户系统流逝
## 11.31  2.26 26.43 
all.equal(C,gpuC[])
## [1] TRUE when use double
## [1] "Mean relative difference: 0.0000005057388" when use float

library(Matrix) ## 调用矩阵包
m1 <- matrix(0, nrow = 1000, ncol = 1000) ## 构建普通矩阵
m2 <- Matrix(0, nrow = 1000, ncol = 1000, sparse = TRUE) ## 构建稀疏矩阵
object.size(m1) ## 普通矩阵的大小
## 8000200 bytes
object.size(m2) ## 稀疏矩阵的大小
## 5632 bytes

library(Matrix) ## 调用矩阵包
library(glmnet) ## 调用glmnet
n <- 100000 ## 样本数
p <- 300 ## 因子数
x <- matrix(rnorm(n * p), n, p) ## 构建矩阵
iz <- sample(1:(n * p),
                             size = n * p * 0.85,
                             replace = FALSE) ## 随机抽取样本
x[iz] <- 0
object.size(x) ## 原矩阵大小
## 240000200 bytes
sx <- Matrix(x, sparse = TRUE) ## 构建稀疏矩阵
object.size(sx)## 稀疏矩阵大小
## 54002624 bytes
beta <- rnorm(p)
y <- x %*% beta + rnorm(n) ## 计算y值
glmnet.fit <- glmnet(x, y) ## 原矩阵拟合
glmnet.sparse <- glmnet(sx, y)##  稀疏矩阵拟合
all.equal(glmnet.fit$beta, glmnet.sparse$beta) ## 对比两个结果
## [1] TRUE
