### chapter 12
product <- "rb"

date.spread.combine <- function(data.now, data.new, col.names) { ## 生成价差合约
  ## data.now是近月合约的数据，data.new是远月合约，col.names是列的名称
  clean <- function(x) { ## 清理异常值
    x[is.na(x)] <- 0
    x[x==Inf] <- 0
    x[x==-Inf] <- 0
    return (x)
  }
  n.now <- nrow(data.now) ## 近月合约样本数
  n.new <- nrow(data.new) ## 远月合约样本数
  i.now <- 1 ## 近月合约下标
  i.new <- 1 ## 远月合约下标
  pre.now <- 1 ## 近月合约前一个行情
  pre.new <-1 ## 远月合约前一个行情
  data.time.combine <- rep(data.now$date.time[1], n.now+n.new) ## 合并后的数据
  data.now.index <- rep(0, n.now+n.new) ## 近月合约数据位置标记
  data.new.index <- rep(0, n.now+n.new) ## 远月合约数据位置标记
  i.combine <- 1 ## 合并后数据的位置下标
  while (i.now<n.now && i.new<n.new) { ## 数据还未处理完
    if (data.now$date.time[i.now]<data.new$date.time[i.new]) { ## 近月时间更慢
      data.time.combine[i.combine] <- data.now$date.time[i.now] ## 当前时间为近月时间 
      data.now.index[i.combine] <- i.now ## 近月用当前的行情
      data.new.index[i.combine] <- pre.new ## 远月用旧的行情
      i.combine <- i.combine+1 ## 合并行情指标加1
      pre.now <- i.now ## 近月前一个行情更新
      i.now <- i.now+1 ## 近月合约下标累加
    } else if (data.now$date.time[i.now]>data.new$date.time[i.new]) { ## 远月合约慢
      data.time.combine[i.combine] <- data.new$date.time[i.new] ## 当前时间为远月时间
      data.now.index[i.combine] <- pre.now ## 近月用旧的行情
      data.new.index[i.combine] <- i.new ## 远月用新的行情
      i.combine <- i.combine+1 ## 合并行情指标加1
      pre.new <- i.new ## 远月前一个行情更新
      i.new <- i.new+1 ## 远月合约下标累加
    } else { ## 近月和远月的时间一样
      data.time.combine[i.combine] <- data.now$date.time[i.now] ## 当且时间为近月时间
      data.now.index[i.combine] <- i.now ## 近月用新的行情
      data.new.index[i.combine] <- i.new ## 远月用新的行情
      pre.now <- i.now ## 近月前一个行情更新
      pre.new <- i.new ## 远月前一个行情更新
      i.combine <- i.combine+1 ## 合并行情指标加1
      i.now <- i.now+1 ## 近月下标累加
      i.new <- i.new+1 ## 远月下标累加
    }
  }
  i.combine <- i.combine-1 ## 行情总数最新的没有设置，因此要减一
  data.time.combine <- data.time.combine[1:i.combine] ## 取有效的合并行情
  data.now.index <- data.now.index[1:i.combine] ## 近月的行情下标
  data.new.index <- data.new.index[1:i.combine] ## 远月的行情下标
  data.spread <- matrix(0, nrow=i.combine, ncol=length(col.names)) ## 构建价差合约数据框
  colnames(data.spread) <- col.names ## 给数据框命名
  data.spread <- as.data.frame(data.spread) ## 转成数据框格式
  data.spread$date.time <- data.time.combine ## 设置日期时间
  data.spread$price <- data.now$price[data.now.index]-data.new$price[data.new.index] 
  ## 价差的最新价直接相减
  data.spread$cum.open.int <- data.now$cum.open.int[data.now.index]+data.new$cum.open.int[data.new.index]
  ## 累计持仓量为两个合约持仓量相加
  data.spread$open.int <- data.now$open.int[data.now.index]+data.new$open.int[data.new.index]
  ## 持仓量增量也是直接相加
  data.spread$turnover <- data.now$turnover[data.now.index]+data.new$turnover[data.new.index]
  ## 成交额为两个合约的成交额相加
  data.spread$qty <- data.now$qty[data.now.index]+data.new$qty[data.new.index]
  ## 成交量为两个合约的成交量相加
  data.spread$bid <- data.now$bid[data.now.index]-data.new$ask[data.new.index]
  ## 买一价位近月的买一价减去远月的卖一价
  data.spread$ask <- data.now$ask[data.now.index]-data.new$bid[data.new.index]
  ## 卖一价为近月的卖一价减去远月的买一价
  data.spread$bid.qty <- data.now$bid.qty[data.now.index]
  ## 买一挂单量为近月买一挂单量
  data.spread$ask.qty <- data.now$ask.qty[data.now.index]
  ## 卖一挂单量为近月卖一挂单量
  data.now$wpr <- clean(with(data.now, (bid*ask.qty+ask*bid.qty)/(bid.qty+ask.qty)))
  ## 计算近月的加权平均价
  data.new$wpr <- clean(with(data.new, (bid*ask.qty+ask*bid.qty)/(bid.qty+ask.qty)))
  ## 计算远月的加权平均价
  data.spread$open.symbol <- data.now$wpr[data.now.index]
  ## 找一列设置近月的加权平均价
  data.spread$close.symbol <- data.new$wpr[data.new.index]
  ## 找一列设置远月的加权平均价
  col.names[col.names=="open.symbol"] <- "now.wpr" ## 更改近月加权平均价列名
  col.names[col.names=="close.symbol"] <- "new.wpr" ## 更改远月加权平均价列名
  colnames(data.spread) <- col.names ## 设置新的数据框列名
  return(data.spread) ## 返回新数据框
}


col.names <- c("market","contract","date.time","price","cum.open.int","open.int","turnover",
               "qty","open.symbol","close.symbol","type","dire","bid","ask","bid.qty","ask.qty")
## 提取的列名
exchange <- product.info[[product]]$exchange ## 所在交易所的名字
library(data.table)
chosen.month <- paste(contract.month[[product]],collapse="|")
if (!grepl("^[[:upper:]]+$", product)) { ## Shanghai or Dalian
  pattern <- paste(".*",product,"[[:digit:]]{2}(?:",chosen.month,")",sep="")
} else { ## Zhengzhou
  pattern <- paste(".*",product,"[[:digit:]]{1}(?:",chosen.month,")",sep="")
}
dire <- "d:/liwei/book/data/sc"
file.list <- list.files(dire, recursive=TRUE,pattern=pattern)
file.list <- file.list[grep(paste("/",product,sep=""), file.list)]
file.list
start.prod <- as.numeric(regexpr(paste("/",product,sep=""),file.list[1]))
check.dup <- substr(file.list, start.prod+nchar(product)+1,start.prod+nchar(product)+1)
file.list <- file.list[check.dup>="0" & check.dup<="9"]
file.list
len <- nchar(file.list[1])
date.list <- substr(file.list,len-11,len-4)
date.list
count.file <- table(date.list)
date.list <- date.list[date.list>=names(which(count.file==3))[1]]
if (length(date.list)/length(table(date.list))!=3) {
  stop("error length of date.list\n")
}
len.date <- length(table(date.list))
len.contract <- length(date.list)
i <- 1
col.names <- c("market","contract","date.time","price","cum.open.int","open.int","turnover",
               "qty","open.symbol","close.symbol","type","dire","bid","ask","bid.qty","ask.qty")
exchange <- product.info[[product]]$exchange
file.list


cat(i, "\n")
data.old <- fread(paste(dire,file.list[i],sep="/"),stringsAsFactors=FALSE, col.names = col.names)  
data.now <- fread(paste(dire,file.list[i+1],sep="/"),stringsAsFactors=FALSE, col.names = col.names)  
data.new <- fread(paste(dire,file.list[i+2],sep="/"),stringsAsFactors=FALSE, col.names = col.names)  
start <- 1
old.time <- substr(data.old$date.time, 12, 19)
while (old.time[start]!="20:59:00" && old.time[start]!="08:59:00" && data.old$bid[start]==0 && data.old$ask[start]==0) start <- start+1
data.old <- data.old[start:nrow(data.old),]
start <- 1
now.time <- substr(data.now$date.time, 12, 19)
while (now.time[start]!="20:59:00" && now.time[start]!="08:59:00" && data.now$bid[start]==0 && data.now$ask[start]==0) start <- start+1
data.now <- data.now[start:nrow(data.now),]
start <- 1
new.time <- substr(data.new$date.time, 12, 19)
while (new.time[start]!="20:59:00" && new.time[start]!="08:59:00" && data.new$bid[start]==0 && data.new$ask[start]==0) start <- start+1
data.new <- data.new[start:nrow(data.new),]


old.contract <- data.old$contract[1]
now.contract <- data.now$contract[1]
new.contract <- data.new$contract[1]
old.spread.name <- paste(old.contract, now.contract, sep="-")
old.spread.name
## [1] "rb1705-rb1710"
new.spread.name <- paste(now.contract, new.contract, sep="-")
new.spread.name
## [1] "rb1710-rb1801"


spread <- product.info[[product]]$spread
data.old$ask[data.old$ask==0 & data.old$bid>0] <- data.old$bid[data.old$ask==0 & data.old$bid>0]+spread
data.old$bid[data.old$bid==0 & data.old$ask>0] <- data.old$ask[data.old$bid==0 & data.old$ask>0]-spread
data.now$ask[data.now$ask==0 & data.now$bid>0] <- data.now$bid[data.now$ask==0 & data.now$bid>0]+spread
data.now$bid[data.now$bid==0 & data.now$ask>0] <- data.now$ask[data.now$bid==0 & data.now$ask>0]-spread
data.new$ask[data.new$ask==0 & data.new$bid>0] <- data.new$bid[data.new$ask==0 & data.new$bid>0]+spread
data.new$bid[data.new$bid==0 & data.new$ask>0] <- data.new$ask[data.new$bid==0 & data.new$ask>0]-spread



old.data <- date.spread.combine(data.old, data.now, col.names)
sum(old.data$bid>old.data$ask)
## [1] 0
mean(old.data$ask-old.data$bid)
## [1] 2.360243

## Figure 12-1
plot(old.data$bid, type="l", main=old.spread.name, ylab="price")
points(old.data$ask, type="l", col=2)
nrow(data.old)

## Figure 12-2
plot(old.data$bid[601:nrow(old.data)], type="l", main=old.spread.name, ylab="price")

spread <- 1
get.cross.date.data <- function(product, dire, path="d:/liwei/book/data") { ## 处理一个文件夹的跨期合约数据
  setwd(path) ## 设置路径
  source("d:/liwei/book/code/helper.r") ## 调用相关辅助函数
  library(data.table) ## 调用data.table
  chosen.month <- paste(contract.month[[product]],collapse="|") ## 品种活跃的月份
  if (!grepl("^[[:upper:]]+$", product)) { ## Shanghai or Dalian ## 正则表达式的公式
    pattern <- paste(".*",product,"[[:digit:]]{2}(?:",chosen.month,")",sep="")
  } else { ## Zhengzhou
    pattern <- paste(".*",product,"[[:digit:]]{1}(?:",chosen.month,")",sep="")
  }
  file.list <- list.files(dire, recursive=TRUE,pattern=pattern) ##　列出所有文件
  file.list <- file.list[grep(paste("/",product,sep=""), file.list)]　## 提取有关的文件
  file.list
  start.prod <- as.numeric(regexpr(paste("/",product,sep=""),file.list[1]))
  check.dup <- substr(file.list, start.prod+nchar(product)+1,start.prod+nchar(product)+1)
  file.list <- file.list[check.dup>="0" & check.dup<="9"] ## 过滤掉无关的文件
  len <- nchar(file.list[1])
  date.list <- substr(file.list,len-11,len-4)
  count.file <- table(date.list)
  date.list <- date.list[date.list>=names(which(count.file==3))[1]] ## 提取相关的日期
  if (length(date.list)/length(table(date.list))!=3) { ## 出现异常则停止检查
    stop("error length of date.list\n")
  }
  len.date <- length(table(date.list))
  len.contract <- length(date.list)
  i <- 1
  col.names <- c("market","contract","date.time","price","cum.open.int","open.int","turnover",
                 "qty","open.symbol","close.symbol","type","dire","bid","ask","bid.qty","ask.qty")
  ## 列名
  exchange <- product.info[[product]]$exchange ## 交易所名
  spread <- product.info[[product]]$spread ## 品种合约价差
  while (i<len.contract) {
    cat(i, "\n")
    data.old <- fread(paste(dire,file.list[i],sep="/"),stringsAsFactors=FALSE, col.names = col.names)  
    data.now <- fread(paste(dire,file.list[i+1],sep="/"),stringsAsFactors=FALSE, col.names = col.names)  
    data.new <- fread(paste(dire,file.list[i+2],sep="/"),stringsAsFactors=FALSE, col.names = col.names)  
    start <- 1
    old.time <- substr(data.old$date.time, 12, 19)
    while (old.time[start]!="20:59:00" && old.time[start]!="08:59:00" && data.old$bid[start]==0 && data.old$ask[start]==0) start <- start+1
    data.old <- data.old[start:nrow(data.old),]
    start <- 1
    now.time <- substr(data.now$date.time, 12, 19)
    while (now.time[start]!="20:59:00" && now.time[start]!="08:59:00" && data.now$bid[start]==0 && data.now$ask[start]==0) start <- start+1
    data.now <- data.now[start:nrow(data.now),]
    start <- 1
    new.time <- substr(data.new$date.time, 12, 19)
    while (new.time[start]!="20:59:00" && new.time[start]!="08:59:00" && data.new$bid[start]==0 && data.new$ask[start]==0) start <- start+1
    data.new <- data.new[start:nrow(data.new),]
    old.contract <- data.old$contract[1]
    now.contract <- data.now$contract[1]
    new.contract <- data.new$contract[1]
    old.spread.name <- paste(old.contract, now.contract, sep="-") ## 第一个价差合约名
    new.spread.name <- paste(now.contract, new.contract, sep="-") ## 第二个价差合约名
    if (sum(data.old$ask==0 & data.old$bid>0)>0) data.old$ask[data.old$ask==0 & data.old$bid>0] <- data.old$bid[data.old$ask==0 & data.old$bid>0]+spread
    if (sum(data.old$bid==0 & data.old$ask>0)>0) data.old$bid[data.old$bid==0 & data.old$ask>0] <- data.old$ask[data.old$bid==0 & data.old$ask>0]-spread
    if (sum(data.now$ask==0 & data.now$bid>0)>0) data.now$ask[data.now$ask==0 & data.now$bid>0] <- data.now$bid[data.now$ask==0 & data.now$bid>0]+spread
    if (sum(data.now$bid==0 & data.now$ask>0)>0) data.now$bid[data.now$bid==0 & data.now$ask>0] <- data.now$ask[data.now$bid==0 & data.now$ask>0]-spread
    if (sum(data.new$ask==0 & data.new$bid>0)>0) data.new$ask[data.new$ask==0 & data.new$bid>0] <- data.new$bid[data.new$ask==0 & data.new$bid>0]+spread
    if (sum(data.new$bid==0 & data.new$ask>0)>0) data.new$bid[data.new$bid==0 & data.new$ask>0] <- data.new$ask[data.new$bid==0 & data.new$ask>0]-spread
    old.data <- date.spread.combine(data.old, data.now, col.names)
    len.file.name <- nchar(file.list[i])
    old.file.name <- paste("d:/liwei/book/data/cross date/", exchange, "/", date.list[i], "/",old.spread.name, substr(file.list[i], len.file.name-12, len.file.name), sep="")
    cross.dire <- paste("d:/liwei/book/data/cross date/",  exchange, "/", date.list[i], sep="")
    old.data$contract <- old.spread.name
    dir.create(cross.dire, showWarnings = FALSE)
    write.csv(old.data, file=old.file.name, row.names = FALSE, quote=FALSE) 
    ## 把第一个价差合约写进文件
    new.file.name <- paste("d:/liwei/book/data/cross date/", exchange, "/", date.list[i], "/", new.spread.name,  substr(file.list[i], len.file.name-12, len.file.name), sep="")
    new.data <- date.spread.combine(data.now, data.new, col.names)  
    new.data$contract <- new.spread.name
    write.csv(new.data, file=new.file.name, row.names = FALSE, quote=FALSE)
    ## 把第二个价差合约写进文件
    i <- i+3
  }  
}

parallel.process.5m.arb <- function(product, path="d:/liwei/book/data/cross date", prefix="", data.type="cross date") { ## 并行生成跨期5分钟数据
  cat(product,"\n") 
  setwd(path) ## 设置路径
  all.dire <- list.dirs(full.name=FALSE,recursive=FALSE) ## 全部子目录
  dire.list <- all.dire[grep(prefix, all.dire)] ## 提取满足条件的目录
  library(Rcpp) ## 调用Rcpp
  library(inline) ## 调用inline
  source("d:/liwei/book/code/helper.r") ## 读取帮助文件
  library(split5m) ## 调用分割5分钟数据的C++程序包
  library(data.table) ## 调用data.table
  chosen.month <- paste(contract.month[[product]],collapse="|") ## 活跃合约的月份
  if (!grepl("^[[:upper:]]+$", product)) { ## 上海大连的日期是四位
    pattern <- paste(".*",product,"[[:digit:]]{2}(?:",chosen.month,")",sep="")
  } else { ## 郑州的日期是三位
    pattern <- paste(".*",product,"[[:digit:]]{1}(?:",chosen.month,")",sep="")
  }
  for (dire in dire.list) { ## 逐个文件夹处理
    cat(dire,"\n")
    parse.dire.fast(dire,pattern,product, "d:/liwei/book/data/binary/arb5m/", data.type) ## 快速生成5分钟数据
  }
}


get.data.5m.fast <- function(dire, file, data.type="futures") {
  #data <- read.csv(paste(dire,file,sep="/"),stringsAsFactors=FALSE)
  data <- fread(paste(dire,file,sep="/"),stringsAsFactors=FALSE)
  data <- as.data.frame(data)
  if (nrow(data)<100) return (NA)
  trade.date <- substr(file, nchar(file)-11, nchar(file)-4)
  if (data.type=="futures") {
    colnames(data) <- c("market","contract","date.time","price","cum.open.int","open.int","turnover",
                        "qty","open.symbol","close.symbol","type","dire","bid","ask","bid.qty","ask.qty")
  }
  #if (sum(data$bid>0)<200 & sum(data$ask>0)<200) return(0)
  data$date <- paste(substr(data$date.time,1,4),substr(data$date.time,6,7),substr(data$date.time,9,10),sep="")
  data$time <- substr(data$date.time,12,23)
  total.bar <- nrow(data)
  pre.time <- c(data$time[1], head(data$time,-1))
  night.1 <- which(data$time>"20:59" & data$time<"23:00:01")
  night.2 <- which(data$time>"23:00:01" & data$time<"23:30:01")
  night.3 <- which(data$time>"23:30:01")
  night.4 <- which(data$time<"01:00:01")
  night.5 <- which(data$time>"01:00:01" & data$time<"02:30:01")
  day.time <- which(data$time>"08:59:00" & data$time<"15:00:01")
  chosen.line <- rep(0,120)
  split.time <- rep("",120)
  cur.i <- 0
  if (length(night.1)>0) {
    chosen.line[(cur.i+1):(cur.i+24)] <- getTimeSplit(data$time[night.1], night.1.split)+night.1[1]-1
    chosen.line[cur.i+24] <- tail(night.1,1)
    split.time[(cur.i+1):(cur.i+24)] <- night.1.split
    cur.i <- cur.i+24
  }
  if (length(night.2)>0) {
    chosen.line[(cur.i+1):(cur.i+6)] <- getTimeSplit(data$time[night.2], night.2.split)+night.2[1]-1
    chosen.line[cur.i+6] <- tail(night.2,1)
    split.time[(cur.i+1):(cur.i+6)] <- night.2.split
    cur.i <- cur.i+6
  }
  if (length(night.3)>0) {
    chosen.line[(cur.i+1):(cur.i+6)] <- getTimeSplit(data$time[night.3], night.3.split)+night.3[1]-1
    chosen.line[cur.i+6] <- tail(night.3,1)
    split.time[(cur.i+1):(cur.i+6)] <- night.3.split
    cur.i <- cur.i+6
  }
  if (length(night.4)>0) {
    chosen.line[(cur.i+1):(cur.i+12)] <- getTimeSplit(data$time[night.4], night.4.split)+night.4[1]-1
    chosen.line[cur.i+12] <- tail(night.4,1)
    split.time[(cur.i+1):(cur.i+12)] <- night.4.split
    cur.i <- cur.i+12
  }
  if (length(night.5)>0) {
    chosen.line[(cur.i+1):(cur.i+18)] <- getTimeSplit(data$time[night.5], night.5.split)+night.5[1]-1
    chosen.line[cur.i+18] <- tail(night.5,1)
    split.time[(cur.i+1):(cur.i+18)] <- night.5.split
    cur.i <- cur.i+18
  }
  if (length(day.time)>0) {
    chosen.line[(cur.i+1):(cur.i+45)] <- getTimeSplit(data$time[day.time], day.split)+day.time[1]-1
    chosen.line[cur.i+45] <- tail(day.time,1)
    split.time[(cur.i+1):(cur.i+45)] <- day.split
    cur.i <- cur.i+45
  }
  chosen.line <- chosen.line[1:cur.i]
  chosen.line[chosen.line==0] <- 1
  if (data.type=="cross date") 
    selected.column <- c("contract", "date", "price",  "bid", "ask", "bid.qty", "ask.qty", "now.wpr", "new.wpr")
  data.5m <- data[chosen.line,selected.column]
  data.5m$time <- substr(split.time[1:cur.i],1,8)
  data.5m$trade.date <- trade.date
  start <- 1
  while (data$bid[start]==0 & data$ask[start]==0) start <- start+1
  for (i in 1:cur.i) {
    range <- start:chosen.line[i]
    data.5m$open.int[i] <- sum(data$open.int[range])
    data.5m$qty[i] <- sum(data$qty[range])
    data.5m$open[i] <- data$price[start]
    data.5m$high[i] <- max(data$price[range])
    data.5m$low[i] <- min(data$price[range])
    data.5m$close[i] <- data$price[chosen.line[i]]
    start <- chosen.line[i]+1
  }
  return(data.5m)
}

combine.arb <- function(product,dire="d:/liwei/book/data/binary/arb5m/",start.date="20120101",
                        end.date="20171101") { ## 合并期货跨期价差数据
  day.bar <- 120 ## K线数目
  setwd(dire) ## 设置路径
  product.contract <- dir(pattern=product,, recursive=FALSE) ## 读取文件
  product.contract <- product.contract[grep(paste("^",product,"[[:digit:]]{3,4}",sep=""), product.contract)] ## 提取符合要求的文件
  if (length(product.contract)==0) return (c()); ## 文件为空就返回不做
  col.names <- c("contract","date","trade.date", "time","price","open.int",
                 "qty","bid","ask","bid.qty","ask.qty","now.wpr","new.wpr", "open","high","low","close") ## 数据的列名
  n.col <- length(col.names) ## 列的长度
  update.contract <- c() ## 更新的合约
  for (contract in product.contract) { ## 遍历所有合约
    file.list <- list.files(paste(dire,contract,sep="")) ## 找出相关文件
    if (sum(file.list>=start.date & file.list<end.date)==0) next
    file.list <- file.list[file.list>=start.date & file.list<end.date] ## 在目标日期内
    n.files <- length(file.list) ## 相关文件数量
    cat(contract, n.files,"\n")
    all.data <- matrix(0, nrow=day.bar*n.files, ncol=n.col) ## 整合的数据
    colnames(all.data) <- col.names ## 列名
    all.data <- as.data.frame(all.data) 
    index <- 0
    setwd(paste(dire,contract,sep="")) ## 进入路径
    for (file in file.list) { ## 遍历所有文件
      load(file) ## 调出文件
      all.data[(index+1):(index+nrow(data)),col.names] <- data[,col.names] # 写入数据
      index <- index+nrow(data)
      rm(data)
    }
    all.data <- all.data[1:index,]
    data <- all.data
    save(data,file=paste("d:/liwei/book/data/cross date binary/",contract,".RData",sep=""))
    ## 保存至跨期数据文件夹
  }
}

dire
undebug(get.cross.date.data)
get.cross.date.data("rb",dire)
get.cross.date.data("hc",dire)

debug(parallel.process.5m.arb)

parallel.process.5m.arb("rb")

undebug(combine.arb)
combine.arb("rb")

load("d:/liwei/book/data/cross date binary/rb1601-rb1605.RData")
## Figure 12-3
plot(data$price, type="l", ylab="price")

get.continuous.cross.date <- function(product) { ## 找价差合约的主力合约
  setwd("d:/liwei/book/data/cross date binary") ## 设置当前路径
  contract.list <- list.files(pattern=paste("^",product,"[[:digit:]]{3,4}.*RData",sep="")) 
  ## 提取符合条件的所有合约
  if (length(contract.list)==0) return(1) ## 不存在这样的合约，直接返回
  daily.volume <- list() ## 每天的成交量
  for (contract in contract.list) { ## 遍历所有合约
    load(contract)  ## 读取合约数据
    if (is.null(data$trade.date)) next ## 不存在日期，下一个
    agg.qty <- aggregate(qty~trade.date, data=data, FUN =sum) ## 按交易日统计成交量
    for (i in 1:nrow(agg.qty)) { 
      if (length(daily.volume[[agg.qty$trade.date[i]]])==0) { ## 新的日期
        daily.volume[[agg.qty$trade.date[i]]] <- list(agg.qty$qty[i],contract)
      } else if (agg.qty$qty[i]>daily.volume[[agg.qty$trade.date[i]]][[1]]) { ## 找最大的成交量
        daily.volume[[agg.qty$trade.date[i]]] <- list(agg.qty$qty[i],contract)
      }
    }
  }
  all.contract=as.character(lapply(daily.volume, function(x) return(x[[2]]))) 
  ## 每天最大成交量的合约
  all.dates=as.numeric(names(daily.volume)) ## 所有日期
  pre.contract <- c(all.contract[1], head(all.contract,-1)) ## 前一天的合约
  if (sum(all.contract<pre.contract)>0) { ## 出现混乱的合约（一般是最后一个）
    cat(contract, "messy contract\n")
  }
  continuous.index <- c(1,which(all.contract>pre.contract),length(all.dates)+1) 
  ## 换新合约的下标
  if (max(table(all.contract[continuous.index]))>1) { ## 发现重复，即换新的合约后换回来
    i <- 2
    while (i<length(continuous.index)-1) {
      if (all.contract[continuous.index[i]]==all.contract[continuous.index[i-1]]) {
        aa <- continuous.index[-i]
        continuous.index <- aa
      } else i <- i+1
    }
  } ## 剔除这种换新的之后换回来的情况
  cur.contract <- c()
  exist <- rep(TRUE,length(continuous.index))
  for (i in 1:length(continuous.index)) {
    if (is.element(all.contract[continuous.index[i]],cur.contract)) {
      exist[i] <- FALSE
    } else  cur.contract <- c(cur.contract,
                              all.contract[continuous.index[i]])
  }
  continuous.index <- continuous.index[exist]
  for (i in 1:(length(continuous.index)-1)) {
    index <- continuous.index[i]
    load(all.contract[index])
    data$continuous <- rep(FALSE,nrow(data))
    range <- data$trade.date>=all.dates[index] & data$trade.date<=all.dates[continuous.index[i+1]-1]
    data$continuous[range] <- TRUE ## 设置连续合约的标志
    save(data, file=all.contract[index])
  }
}

## Figure 12-4
plot(data$price, type="l", ylab="price")
points(which(data$continuous), data$price[data$continuous], type="l", col=2)

get.good.arb <- function(product) { ## 查找所有价差合约的主力合约
  SYMBOL.PATH <- "d:/liwei/book/data/" ## 路径
  setwd(paste(SYMBOL.PATH,"cross date binary",sep="")) ## 设置路径
  all.list <- list.files(pattern=paste("^",product,"[[:digit:]]{1}.*RData",sep="")) 
  ## 找出该品种的所有合约
  good.contract <- rep(TRUE,length(all.list))
  for (i in 1:length(all.list)) { ## 遍历所有合约
    load(all.list[i]) ## 调用合约
    if (is.null(data$continuous)) good.contract[i] <- FALSE ## 不包含连续合约就去掉
  }
  contract.list <- all.list[good.contract] ## 保留好的合约
  contract.list <- contract.list[-1] ## 第一个不要，因为没有开头的训练数据
  save(contract.list,file=paste(SYMBOL.PATH,"model set/",product,".arb.list.RData",sep=""))
  ## 保存好结果
}

get.good.arb("rb")

clean.arb <- function(contract, extreme=500) { ## 清洗套利合约数据
  library(stringr) ## 调用stringr
  SYMBOL.PATH <- "d:/liwei/" ## 路径
  setwd(paste(SYMBOL.PATH,"cross date binary",sep="")) ## 设置当前路径
  clean <-  function(x) { ## 清理函数
    x[is.na(x)] <- 0
    x[x==Inf] <- 0
    x[x==-Inf] <- 0
    return (x)
  }
  load(contract) ## 调取合约
  n.bar <- nrow(data) ## 行情数目
  data$good <- rep(TRUE,n.bar) ## 设置好的行情标记
  data$wpr <- data$now.wpr-data$new.wpr ## 加权平均价，为新旧两合约加权平均价之差
  outlier <- data$now.wpr==0 | data$new.wpr==0 ## 特殊情况处理
  data$wpr[outlier] <- data$price[outlier]
  outlier <- abs(data$wpr)>extreme ## 绝对值大小超过极端值每个合约单独处理
  data$wpr[outlier] <- NA
  clean.wpr <- na.locf(data$wpr,na.rm=FALSE) ## 沿用前值
  if (is.na(data$wpr[1])) { ## 如果开始值缺失则停止
    stop(contract)
  }
  data$wpr <- clean.wpr ## 加权平均价设置为清洗过的价格
  data$wpr.ret <- c(0,diff(data$wpr)) ## 计算价格收益率
  na.price <- is.na(data$open) | is.na(data$high) | is.na(data$low) ## 查找缺失值
  data$open[na.price] <- data$close[na.price] ## 补充K线缺失值
  data$high[na.price] <- data$close[na.price]
  data$low[na.price] <- data$close[na.price]
  na.qty <- is.na(data$qty)
  data$qty[na.qty] <- 0
  data$open.int[na.qty] <- 0
  data$price[data$price==0 & data$bid>0] <- data$bid[data$price==0 & data$bid>0]
  data$price[data$price==0 & data$ask>0] <- data$ask[data$price==0 & data$ask>0]
  data$close[data$close==0] <- data$price[data$close==0]
  data$bid[data$bid==0] <- data$price[data$bid==0]
  data$ask[data$ask==0] <- data$price[data$ask==0]
  data$wpr[data$wpr==0] <- data$close[data$wpr==0]
  data$high[data$high==0] <- data$close[data$high==0]
  data$low[data$low==0] <- data$close[data$low==0]
  dates <- str_c(substr(data$date,1,4),substr(data$date,5,6),substr(data$date,7,8),
                 sep="/")
  data$date.time <- as.POSIXlt(paste(dates,data$time)) ## 设置时间
  save(data, file=contract) ## 保存数据
}


get.signal <- function(product, signal, dates, dire="tmp") { ## 获取因子的函数
  signal.list <- paste(SYMBOL.PATH, "/", dire, "/",signal,"/",dates, sep="")
  ## 期货银子默认在tmp下面，跨期因子则在其他文件夹下面
  value <- rep(0,200*length(dates))
  cur <- 0
  for (file in signal.list) { ## 遍历所有文件
    load(file) ## 调取数据
    value[(cur+1):(cur+length(S))] <- S ## 存储因子
    cur <- cur+length(S)
  }
  return (value[1:cur])
}

setwd("d:/liwei/book/data/cross date binary/")
SYMBOL.PATH <- "d:/liwei/book/data"
ys <- get.signal("rb", "fcum.32","rb1601-rb1605.RData", "cross tmp") ## 计算因变量
signal.rsi <- get.signal("rb", "rsi.32", "rb1601-rb1605.RData", "cross tmp") ## 提取rsi.32这个银子

## Figure 12-5 may be different because of different data
plot(ys, type="l", ylab="y")

## Figure 12-6 may be different because of different data
plot(signal.rsi, type="l", ylab="rsi")
cor(ys, signal.rsi)
## [1] -0.03649115



strat.product <- "au"
product <- "ag"
load(file="d:/liwei/book/data/au.ag.RData")

dim(product.data)
## [1] 121362     20
dim(strat.data)
##　[1] 121362     20

dim(product.all.data)
## [1] 121362     15
dim(strat.all.data)
## [1] 121362     15

## Figure 12-10
plot(as.Date(product.all.data$date.time), product.all.data$price, type="l", main=product, ylab="price", xlab="date")

## Figure 12-11
plot(as.Date(strat.all.data$date.time), strat.all.data$price, type="l", main=strat.product, ylab="price", xlab="date")
         
product.all.data$wpr <- with(product.all.data, (bid*ask.qty+ask*bid.qty)/(bid.qty+ask.qty))
strat.all.data$wpr <- with(strat.all.data, (bid*ask.qty+ask*bid.qty)/(bid.qty+ask.qty))
product.all.data$notional <- product.all.data$wpr*product.info[[product]]$multiplier ## 白银合约价值
strat.all.data$notional <- strat.all.data$wpr*product.info[[strat.product]]$multiplier ## 黄金合约价值
product.all.data$log.notional <- log(product.all.data$notional) ## 白银合约价值对数
strat.all.data$log.notional <- log(strat.all.data$notional) ## 黄金合约价值对数

fit.1 <- lm(strat.all.data$log.notional~product.all.data$log.notional) ## 回归模型
summary(fit.1)

fit.2 <- lm(diff(strat.all.data$log.notional)~diff(product.all.data$log.notional))
summary(fit.2)

fit.3 <- lm(diff(strat.all.data$log.notional)~diff(product.all.data$log.notional)+0)
summary(fit.3)

Box.test(fit.1$residuals)

## Figure 12-13
plot(fit.2$residuals, main="model 2 residual", type="l")
Box.test(fit.2$residuals)

fit.1.adjust <- arima(strat.all.data$log.notional, order=c(1,0,1), xreg=product.all.data$log.notional) ## 用ARIMA建模
Box.test(fit.1.adjust$residuals)

## Figure 12-14
plot(fit.1.adjust$residuals, main="model 1 adjust residual")

fit.2.adjust <- arima(diff(strat.all.data$log.notional), order=c(1,0,1), xreg=diff(product.all.data$log.notional))
Box.test(fit.2.adjust$residuals)

## Figure 12-15
plot(fit.2.adjust$residuals, main="model 2 adjust residual")

fit.3.adjust <- arima(diff(strat.all.data$log.notional), order=c(1,0,1), include.mean=FALSE, xreg=diff(product.all.data$log.notional))
Box.test(fit.3.adjust$residuals)

## Figure 12-16
plot(fit.3.adjust$residuals, main="model 3 adjust residual")


cross.bid <- log(product.all.data$bid)-log(strat.all.data$ask) ## 跨品种买家
cross.ask <- log(product.all.data$ask)-log(strat.all.data$bid) ## 跨品种卖家
cross.wpr <- log(product.all.data$wpr)-log(strat.all.data$wpr) ## 加权平均价

## Figure 12-17
plot(cross.wpr, type="l")
