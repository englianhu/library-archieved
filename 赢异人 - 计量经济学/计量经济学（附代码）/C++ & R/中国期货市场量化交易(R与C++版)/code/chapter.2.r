## When there is encoding error for Chinese characters, you can use File->Reopen with Encoding
## and choose UTF-8.

### chapter 2
load(file="d:/liwei/book/data/rb1710_20170721.RData")
## Figure 2-1
head(data)

## Figure 2-2
start <- which(data$时间>="2017-07-21")[1]
data[(start-3):(start+10),]


setwd("D:/liwei/book/data/20170727/sc") ## 设置工作路径
file.list <- list.files() ## 读取文件列表
system.time(for (file in file.list) { ## 测试运行时间，并且使用循环
  data <- read.csv(file, header=TRUE, stringsAsFactors = FALSE)}) ## 读取csv数据
## 用户  系统  流逝 
## 27.68  0.27 28.24 
length(file.list) ## 显示文件长度
##[1] 161 ## 显示结果

library(data.table) ## 调用data.table程序包
system.time(for (file in file.list) ## 测试运行时间
  fast.data <- fread(file, header=TRUE, stringsAsFactors = FALSE)) ## 用fread高速读取
## 用户系统流逝 ## 运行时间
##　4.96 0.08 5.11 

data[101:110,c(3:6,13:16)] ## 显示部分样本


for (file in file.list) { ## 循环逐个处理文件
  data <- fread(file, header=TRUE, stringsAsFactors = FALSE) ## 高速读取
  save(data, file=paste("d:/liwei/book/data/RData/", substr(file, 1, nchar(file)-3), "RData", sep="")) ## 保存二进制格式的文件
}

file.list <- list.files("d:/liwei/book/data/RData/", full.names = TRUE)
system.time(for (file in file.list)
     load(file)) ## 直接调用二进制文件
## 用户系统流逝
##　5.74 0.09 5.98 

address <- "ftp://account:password@down.licai668.cn/" ## 下载的网址，其中account和password
## 需要自己购买填写
MAIN.CONTRACT.PATH <- "d:/liwei/main contract" ## 主力合约所在的路径
exchanges <- c("dc","sc","zc") ## 三个商品交易所列表

## 以下程序需要读者自己购买专门的帐号和密码后才能运行，所以在这里仅仅作为抛砖引玉
download.exchange.data <- function(commodity.latest.date, exchange) { ## 函数头
  ## 需要最新日期和交易所的代码作为参数
  setwd("d:/QQMiniDL") ## 设置工作目录
  library(RCurl) ## 调用RCurl包，可以进行网络传输
  file.dest <- paste(address, exchange,"/",sep="") ## 目标文件地址
  all.files <- getURL(file.dest) ## 获取目标文件列表
  names <- str_split(all.files, pattern=" ")[[1]] ## 分割文件名
  chosen<- names[which(!is.na(str_match(names, paste("^", exchange, "_.*", sep=""))))]
  ## 选择符合命名规则的文件
  date.list <- str_sub(chosen, 4,11) ## 读取文件日期部分
  new.date <- date.list>commodity.latest.date ## 选取需要更新的日期
  file.list <- str_sub(chosen,1,15)[new.date] ## 读取需要更新的文件名
  dire.list <- str_sub(chosen,1,11)[new.date] ## 读取需要更新的文件目录
  date.list <- str_sub(chosen, 4,11)[new.date] ## 读取需要更新的日期列表
  month <- str_sub(date.list,1,6)[1] ## 需要更新的月份
  dir.create(paste("d:/liwei/tick/f_c",month,"d",sep=""),showWarnings = FALSE) ## 创建文件夹
  dire.path <- paste("d:/liwei/tick/f_c",month,"d/",exchange,sep="") ## 下载的文件夹
  target.path <- paste("d:\\liwei\\tick\\f_c",month,"d\\",exchange,sep="")## 目标文件夹
  dir.create(dire.path, showWarnings = FALSE) ##创建目标文件夹
  for (j in 1:length(file.list)) { ## 逐个文件处理
    file <- file.list[j] ## 读取文件名
    file.url <- paste(file.dest, file, sep="") ## 文件下载地址
    download.file(url=file.url, cacheOK=FALSE,mode="wb",quiet=FALSE,destfile=file) ##下载文件
    command<- paste("unrar e -y d:\\QQMiniDL\\", file, " ",target.path, "\\",date.list[j], sep="")
    ##　解压命令
    dir.create(paste(dire.path,"/",date.list[j],sep=""), showWarnings = FALSE)
    ## 创建目标目录
    system(command) ## 在操作系统中下执行命令
  }
  return (date.list)  ## 返回更新的日期列表
}

get.time.split <- function(data.time, split.time) { ## 切割5分钟K线
  j <- 1 ## 跟踪切割位置
  total.bar <- length(data.time) ## 总的行情数目
  n.bar <- length(split.time) ## 总的K线数目
  chosen.line <- rep(0,n.bar) ## 切割位置
  for (i in 1:n.bar) {
    while (j<=total.bar && data.time[j]<=split.time[i]) ## 寻找下一个切割位置
      j <- j+1
    chosen.line[i] <- j-1 ## 设置切割位置
  }
  return(chosen.line)
}


load("d:/liwei/book/data/RData/rb1710_20170727.RData")
data$time <- substr(data$时间,12,23)

night.1 <- seq(from=21*3600+5*60, to=23*3600, by=5*60)
night.2 <- seq(from=23*3600+5*60, to=23*3600+30*60, by=5*60)
night.3 <- seq(from=23*3600+35*60, to=24*3600, by=5*60)
night.4 <- seq(from=5*60, to=1*3600, by=5*60)
night.5 <- seq(from=1*3600+5*60, to=2*3600+30*60, by=5*60)
day.1 <- seq(from=9*3600+5*60,to=10*3600+15*60,by=5*60)
day.2 <- seq(from=10*3600+35*60,to=11*3600+30*60,by=5*60)
day.3 <- seq(from=13*3600+35*60,to=15*3600,by=5*60)
day <- c(day.1,day.2,day.3)
get.split.time <- function(day) {
  hour <- floor(day/3600)
  hour.str <- ifelse(hour<10, paste("0",hour,sep=""),hour)
  minute <- (day%%3600)/60
  minute.str <- ifelse(minute<10, paste("0",minute,sep=""),minute)
  split.time <- paste(hour.str,":",minute.str,":00.000",sep="")
  return (split.time)
}
night.1.split <- get.split.time(night.1)
night.2.split <- get.split.time(night.2)
night.3.split <- get.split.time(night.3)
night.4.split <- get.split.time(night.4)
night.5.split <- get.split.time(night.5)


##　这里使用的变量名与之前重复是失误导致，不会引起错误
night.1 <- which(data$time>"20:59" & data$time<"23:00:01") ## 夜盘第一段时间
night.2 <- which(data$time>"23:00:01" & data$time<"23:30:01") ## 夜盘第二段时间
night.3 <- which(data$time>"23:30:01") ## 夜盘第三段时间
night.4 <- which(data$time<"01:00:01") ## 夜盘第四段时间
night.5 <- which(data$time>"01:00:01" & data$time<"02:30:01") ## 夜盘第5段时间
day.time <- which(data$time>"08:59:00" & data$time<"15:00:01") ## 白天时段


system.time(for (i in 1:1000)
  bb <- get.time.split(data$time[night.1], night.1.split)+night.1[1]-1)
## 用户   系统   流逝 
## 35.66  0.03 35.92 

library(Rcpp) ## 调用Rcpp包
sourceCpp("d:/liwei/book/code/getTimeSplit.cpp") ## 编译.cpp文件

system.time(for (i in 1:1000) ## 循环1000次
   aa <- getTimeSplit(data$time[night.1], night.1.split)+night.1[1]-1)
## 用户 系统 流逝 
## 1.12 0.00 1.12

aa
bb
all.equal(aa,bb)
## [1] TRUE


parallel.process.5m.data <- function(product, prefix="20170727") { ## 并行处理5分钟数据
  cat(product,"\n") ## 输出品种名称
  path <- "d:/liwei/book/data" ## 分笔数据的路径
  setwd(path) ## 设置路径
  all.dire <- list.dirs(full.name=FALSE,recursive=FALSE) ## 查找文件夹
  dire.list <- all.dire[grep(prefix, all.dire)] ## 满足前缀条件的文件夹
  library(Rcpp) ## 调用Rcpp程序包
  library(inline) ## 调用inline程序包
  source("d:/liwei/book/code/helper.r") ## 调用相关辅助函数
  sourceCpp("d:/liwei/rcode/getTimeSplit.cpp") ## 编译.cpp函数
  library(data.table) ## 调用data.table
  chosen.month <- paste(contract.month[[product]],collapse="|") ##选择品种活跃月份
  
  if (!grepl("^[[:upper:]]+$", product)) { ## Shanghai or Dalian上海或大连交易所
    pattern <- paste(".*",product,"[[:digit:]]{2}(?:",chosen.month,")",sep="")
  } else { ## Zhengzhou郑州交易所
    pattern <- paste(".*",product,"[[:digit:]]{1}(?:",chosen.month,")",sep="")
  }
  for (dire in dire.list) {
    cat(dire,"\n")
    parse.dire.fast(dire,pattern,product) ## 处理一个文件夹的问价
  }
}

library(parallel) ## 调用并行的程序包
product.list <- c("ag", "au","bu","cu","hc","ni","rb","ru","zn") ## 活跃商品列表
n.core <- 4 ## 核对数目
system.time({
  cl <- makeCluster(n.core) ## 建立集群
  results <- parLapply(cl,product.list,parallel.process.5m.data) ## 并行处理
  stopCluster(cl)
})
用户系统流逝
## 0.04  0.00 16.05 ## 只是上期品种一天的量


product <- "rb"

all.contracts <- get.dates(product)
all.contracts
total <- 0
good <- 0
setwd("d:/liwei/book/binary/")
for (contract in all.contracts) {
  load(contract)
  total <- total+sum(data$continuous)
  good <- good+sum(data$continuous & data$good)
}
good/total
## [1] 0.9960169
good
## [1] 74268
total
##　[1] 74565

