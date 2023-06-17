contract.month <- list()
contract.month[["a"]] <- c("01","05","09")
contract.month[["ag"]] <- c("06","12")
contract.month[["al"]] <- c("01","02","03","04","05","06","07","08","09","10","11","12")
contract.month[["au"]] <- c("06","12")
contract.month[["bu"]] <- c("03", "06", "09", "12")
contract.month[["c"]] <- c("01","05","09")
contract.month[["CF"]] <- c("01","05","09")
contract.month[["cs"]] <- c("01","05","09")
contract.month[["cu"]] <- c("01","02","03","04","05","06","07","08","09","10","11","12")
contract.month[["FG"]] <- c("01","05","09")
contract.month[["hc"]] <- c("01","05","10")
contract.month[["i"]] <- c("01","05","09")
contract.month[["j"]] <- c("01","05","09")
contract.month[["jd"]] <- c("01","05","09")
contract.month[["jm"]] <- c("01","05","09")
contract.month[["l"]] <- c("01","05","09")
contract.month[["m"]] <- c("01","05","09")
contract.month[["MA"]] <- c("01","05","09")
contract.month[["ME"]] <- c("01","05","09")
contract.month[["ni"]] <- c("01","05","09")
contract.month[["OI"]] <- c("01","05","09")
contract.month[["p"]] <- c("01","05","09")
contract.month[["pb"]] <- c("01","02","03","04","05","06","07","08","09","10","11","12")
contract.month[["pp"]] <- c("01","05","09")
contract.month[["rb"]] <- c("01","05","10")
contract.month[["RM"]] <- c("01","05","09")
contract.month[["ru"]] <- c("01","05","09")
contract.month[["SM"]] <- c("01","05","09")
contract.month[["sn"]] <- c("01","05","09")
contract.month[["SF"]] <- c("01","05","09")
contract.month[["SR"]] <- c("01","05","09")
contract.month[["TA"]] <- c("01","05","09")
contract.month[["TC"]] <- c("01","05","09")
contract.month[["v"]] <- c("01","05","09")
contract.month[["WH"]] <- c("01","05","09")
contract.month[["y"]] <- c("01","05","09")
contract.month[["ZC"]] <- c("01","05","09")
contract.month[["zn"]] <- c("01","02","03","04","05","06","07","08","09","10","11","12")

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
day.split <- get.split.time(day)
n.bars <- length(day)


col.names <- c("contract","date","time","price","open.int",
               "qty","bid","ask","bid.qty","ask.qty","open","high","low","close")

selected.column <- c("contract","date","price","bid","ask","bid.qty","ask.qty")

parse.dire.fast  <- function(dire,pattern,product,object.dire="d:/liwei/book/binary/") {
  ## 处理文件夹的程序
  file.list <- list.files(dire, recursive=TRUE,pattern=pattern) ##查找所有满足条件的文件
  file.list <- file.list[grep(paste("/",product,sep=""), file.list)] ## 过滤掉一些错误的文件
  start.prod <- as.numeric(regexpr(paste("/",product,sep=""),file.list[1])) ## 查找到期日位�?
  check.dup <- substr(file.list, start.prod+nchar(product)+1,start.prod+nchar(product)+1) ##提取到期�?
  file.list <- file.list[check.dup>="0" & check.dup<="9"] ## 继续过滤文件
  if (length(file.list)==0) return(1) ## 如果没有满足要求的文件则返回
  for (i in 1:length(file.list)) { ## 逐个处理文件
    file <- file.list[i] ## 提取文件
    cat(file,"\n")
    if (file.info(paste(dire,file,sep="/"))$size==0) next ## 文件错误则下一�?
    data <- get.data.5m.fast(dire,file) ## 快速整�?5分钟数据
    if (length(data)==1) next ## 数据错误则下一�?
    contract <- data$contract[1] ## 提取合约
    dir.create(paste(object.dire,contract,sep=""),showWarnings=FALSE) ## 创建文件�?
    new.file <- gsub("_","/",paste(substr(file,10,nchar(file)-3),"RData",sep="")) ##  新文件名
    heaven <- as.numeric(gregexpr("/",new.file)[[1]]) ## 特殊情况处理
    if (length(heaven)>1) new.file <- substr(new.file, heaven[1]+1,nchar(new.file))
    save(data,file=paste(object.dire,new.file,sep="")) ## 保存新文�?
  }
}

get.data.5m.fast<- function(dire,file) { ## 快速处�?5分钟数据
  data <- fread(paste(dire,file,sep="/"),stringsAsFactors=FALSE) ## 快速读取文�?
  data <- as.data.frame(data) ## 整理成数据框
  trade.date <- substr(file, nchar(file)-11, nchar(file)-4) ## 提取交易�?
  colnames(data) <- c("market","contract", "date.time","price","cum.open.int","open.int","turnover",
                      "qty","open.symbol","close.symbol","type","dire","bid","ask","bid.qty","ask.qty") ## 列的名称
  data$date<- paste(substr(data$date.time,1,4),substr(data$date.time,6,7),substr(data$date.time,9,10),sep="")
  ## 提取日期
  data$time <- substr(data$date.time,12,23) ##提取时间
  total.bar <- nrow(data) ## 总的行情�?
  pre.time <- c(data$time[1], head(data$time,-1)) ## 前一个时�?
  night.1 <- which(data$time>"20:59" & data$time<"23:00:01") ## 夜盘第一段数�?
  night.2 <- which(data$time>"23:00:01" & data$time<"23:30:01") ## 夜盘第二段数�?
  night.3 <- which(data$time>"23:30:01") ## 夜盘第三段数�?
  night.4 <- which(data$time<"01:00:01") ## 夜盘第四段数�?
  night.5 <- which(data$time>"01:00:01" & data$time<"02:30:01") ## 夜盘第五段数�?
  day.time <- which(data$time>"08:59:00" & data$time<"15:00:01") ## 白天数据
  chosen.line <- rep(0,120) ## 分割位置
  split.time <- rep("",120) ## 分割时间�?
  cur.i <- 0
  if (length(night.1)>0) { ## 处理夜盘第一段数�?
    chosen.line[(cur.i+1):(cur.i+24)] <- getTimeSplit(data$time[night.1], night.1.split)+night.1[1]-1
    chosen.line[cur.i+24] <- tail(night.1,1)
    split.time[(cur.i+1):(cur.i+24)] <- night.1.split
    cur.i <- cur.i+24
  }
  if (length(night.2)>0) { ## 处理夜盘第二段数�?
    chosen.line[(cur.i+1):(cur.i+6)] <- getTimeSplit(data$time[night.2], night.2.split)+night.2[1]-1
    chosen.line[cur.i+6] <- tail(night.2,1)
    split.time[(cur.i+1):(cur.i+6)] <- night.2.split
    cur.i <- cur.i+6
  }
  if (length(night.3)>0) { ## 处理夜盘第三段数�?
    chosen.line[(cur.i+1):(cur.i+6)] <- getTimeSplit(data$time[night.3], night.3.split)+night.3[1]-1
    chosen.line[cur.i+6] <- tail(night.3,1)
    split.time[(cur.i+1):(cur.i+6)] <- night.3.split
    cur.i <- cur.i+6
  }
  if (length(night.4)>0) { ## 处理夜盘第四段数�?
    chosen.line[(cur.i+1):(cur.i+12)] <- getTimeSplit(data$time[night.4], night.4.split)+night.4[1]-1
    chosen.line[cur.i+12] <- tail(night.4,1)
    split.time[(cur.i+1):(cur.i+12)] <- night.4.split
    cur.i <- cur.i+12
  }
  if (length(night.5)>0) { ## 处理夜盘第五段数�?
    chosen.line[(cur.i+1):(cur.i+18)] <- getTimeSplit(data$time[night.5], night.5.split)+night.5[1]-1
    chosen.line[cur.i+18] <- tail(night.5,1)
    split.time[(cur.i+1):(cur.i+18)] <- night.5.split
    cur.i <- cur.i+18
  }
  if (length(day.time)>0) {## 处理白天数据
    chosen.line[(cur.i+1):(cur.i+45)] <- getTimeSplit(data$time[day.time], day.split)+day.time[1]-1
    chosen.line[cur.i+45] <- tail(day.time,1)
    split.time[(cur.i+1):(cur.i+45)] <- day.split
    cur.i <- cur.i+45
  }
  chosen.line <- chosen.line[1:cur.i] ## 提取有效的部�?
  chosen.line[chosen.line==0] <- 1
  data.5m <- data[chosen.line,selected.column] ##提取有意义的行和�?
  data.5m$time <- substr(split.time[1:cur.i],1,8) ## 设置时间
  data.5m$trade.date <- trade.date ## 设置交易日期
  start<- 1
  while (data$bid[start]==0 & data$ask[start]==0) start <- start+1 ## 过滤没意义的行情
  for (i in 1:cur.i) { ## 逐段计算K线数�?
    range <- start:chosen.line[i] ## K线对应的行情范围
    data.5m$open.int[i] <- sum(data$open.int[range]) ## 持仓�?
    data.5m$qty[i] <- sum(data$qty[range]) ## 成交�?
    data.5m$open[i] <- data$price[start] ## 开盘价
    data.5m$high[i] <- max(data$price[range]) ## 最高价
    data.5m$low[i] <- min(data$price[range]) ## 最低价
    data.5m$close[i] <- data$price[chosen.line[i]] ## 收盘�?
    start <- chosen.line[i]+1 ## K线开始位�?
  }
  return(data.5m) ## 返回5分钟K�?
}


TASK.PATH <<- "d:/liwei/book/model set/"
TEMP.PATH <- "d:/liwei/tmp/"
SYMBOL.PATH <- "d:/liwei"


get.dates <- function(s) {
  load(paste(TASK.PATH,s,".list.RData",sep=""))
  return(contract.list)
}

clean <- function(x) {
  x[is.na(x)] <- 0
  x[x==Inf] <- 0
  x[x==-Inf] <- 0
  return (x)
}



lag <- function(x, n, fill = FALSE) {
  if (n == 0)
    return(x)
  else if (n > 0)
    return(c(rep(ifelse(fill, x[1], NA), n), head(x, -n)))
  else
    return(c(x[-(1:-n)], rep(ifelse(fill, tail(x, 1), NA), -n)))
}


cum <- function(x, n) {
  len <- length(x)
  na <- which(is.na(x))
  x[na] <- 0
  sum.x <- cumsum(x)
  res <- (sum.x - c(rep(0, n - 1), 0, head(sum.x, -n)))
  for (i in seq_len(n) - 1) {
    res[na + i] <- 0
  }
  #na <- which(is.na(res))
  #res[na] <- 0
  #res[1:n] <- x[1:n]*n
  res[1:n] <- sum.x[1:n]
  return(res[seq_len(len)])
  ## breaks with NA's: return(cumsum(x) - c(rep(NA, n - 1), 0, head(cumsum(x), -n)))
}


fcum <- function(x, n, na.rm = TRUE, fill = 0) {
  if (na.rm) return(head(lag(cum(c(x, rep(fill, n)), n), -n), -n))
  else return(lag(cum(x, n), -n, fill = fill))
}

product.info <- list()
product.info[["rb"]] <- list(spread=1, slippage=1, tranct=1e-4, tranct.ratio=TRUE,
                             min.period=16, max.period=16, train.len=4, t.thre=2,
                             pred.period=16, criterion="sharp",
                             open.thre.list=seq(from=1e-3, to=4e-3, by=2e-4),
                             better.open.thre.list=seq(from=1e-3, to=4e-3, by=2e-4),
                             best=19,int.factor=1,
                             train.range=c(5,6,9), valid.range=1:13,test.start=1,
                             open.thre=0.0026, close.thre=-0.0014,
                             #better=TRUE, ## original
                             better=FALSE,
                             multiplier=10,opti.range=1:13,night="23:00:00",n.train=3,target=4,
                             exchange="sc", better.good.target=c(4,6,10,12,14,15))

product.info[["hc"]] <- list(spread=1, slippage=1, tranct=1.2e-4, tranct.ratio=TRUE,
                             min.period=16, 
                             max.period=32,  #original
                             #max.period=16,
                             train.len=4, t.thre=2,
                             pred.period=16, criterion="sharp",multiplier=10,
                             open.thre.list=seq(from=2e-3, to=4e-3, by=2e-4),
                             better.open.thre.list=seq(from=2e-3, to=4e-3, by=2e-4),
                             int.factor=1,
                             train.range=2:3, valid.range=6,test.start=1,opti.range=1:6,
                             better=TRUE, # original
                             #better=FALSE,
                             best=19, open.thre=0.0032,close.thre=0.002,
                             night="23:00:00",n.train=3, target=5,
                             exchange="sc",
                             better.good.target=6, total.qty=35, n.strat=8)

product.info[["ru"]] <- list(spread=5, slippage=2, tranct=2e-4, tranct.ratio=TRUE,
                             min.period=16, max.period=16, train.len=4, t.thre=2,
                             pred.period=16, criterion="sharp",
                             open.thre.list=seq(from=2e-3, to=4e-3, by=2e-4),
                             train.range=c(1,10,13), valid.range=1:14, test.start=1,
                             best=37, spread.limit=TRUE,multiplier=10,
                             opti.range=1:14, test.start=1,int.factor=1,
                             better=FALSE,open.thre=0.0016,close.thre=-0.001,night="23:00:00",
                             n.train=3,target=5, exchange="sc") ## original is 8

product.info[["cu"]] <- list(spread=10, slippage=1, tranct=1e-4, tranct.ratio=TRUE,
                             min.period=16, max.period=32, train.len=4, t.thre=2,
                             pred.period=16, criterion="sharp",int.factor=1,
                             open.thre.list=seq(from=1e-3, to=3e-3, by=2e-4),
                             better.open.thre.list=seq(from=1e-3, to=3e-3, by=2e-4),
                             better=TRUE,best=13,open.thre=0.0014,close.thre=-0.001,
                             train.range=25:30, valid.range=24:56, opti.range=24:54,
                             test.start=24, multiplier=5,night="01:00:00",
                             n.train=12,target=21,
                             exchange="sc",better.good.target=31)


product.info[["zn"]] <- list(spread=5, slippage=1, tranct=0.66, tranct.ratio=FALSE,
                             min.period=16, 
                             ##max.period=16,  ## origianl
                             max.period=32,
                             train.len=4, t.thre=2,
                             pred.period=16, criterion="sharp",multiplier=5,int.factor=1,
                             open.thre.list=seq(from=1e-3, to=3e-3, by=2e-4),
                             better.open.thre.list=seq(from=1e-3, to=3e-3, by=2e-4),
                             train.range=24:29, valid.range=30:35,test.start=24,
                             better=TRUE, best=26, open.thre=0.003, close.thre=0.0022,
                             opti.range=30:54,night="01:00:00",n.train=12, target=48,
                             exchange="sc")


product.info[["bu"]] <- list(spread=2, slippage=1, tranct=0.4e-4, tranct.ratio=TRUE,
                             min.period=16, max.period=16, train.len=4, t.thre=2,
                             pred.period=16, criterion="sharp",
                             open.thre.list=seq(from=2e-3, to=4e-3, by=2e-4),
                             better.open.thre.list=seq(from=2e-3, to=4e-3, by=2e-4),
                             best=12,open.thre=0.0026,close.thre=-0.0022,
                             better=TRUE, #original
                             #better=FALSE,
                             train.range=4:5, valid.range=c(1,2,3,6,7),
                             test.start=3,multiplier=10,opti.range=3:7,int.factor=1,night="23:00:00",
                             n.train=3, target=8,good.target=c(7,10),
                             exchange="sc",better.good.target=c(7,8,10))


product.info[["ag"]] <- list(spread=1, slippage=1, tranct=1e-4, tranct.ratio=TRUE,
                             min.period=16, max.period=16, train.len=4, t.thre=2,
                             pred.period=16, criterion="sharp",
                             #open.thre.list=c(seq(from=2e-3, to=6e-3, by=4e-4)),
                             open.thre.list=c(seq(from=1e-3, to=2e-3, by=1e-4)), # original
                             better.open.thre.list=c(seq(from=1e-3, to=2e-3, by=1e-4)),
                             #open.thre.list=c(seq(from=1e-3, to=3e-3, by=2e-4)),
                             #train.range=c(2,3,7), 
                             train.range=c(4,7),
                             valid.range=2:8, test.start=2,
                             best=24, open.thre=0.004, close.thre=-0.0004,
                             better=TRUE,  # original
                             #better=FALSE,
                             multiplier=15,int.factor=1,
                             opti.range=2:8,night="02:30:00",
                             n.train=3, target=5,
                             exchange="sc",
                             better.good.target=c(4,5,6,7,8,9))

product.info[["au"]] <- list(spread=0.05, slippage=0, tranct=4e-4, tranct.ratio=TRUE,
                             #min.period=16, max.period=64,  ## original
                             train.len=4, t.thre=2,
                             ##pred.period=32,  ## original
                             min.period=32, max.period=32,pred.period=32,
                             criterion="sharp",
                             #open.thre.list=seq(from=3e-3, to=4e-3, by=2e-4), ## original
                             open.thre.list=seq(from=1e-3, to=3e-3, by=2e-4), ## original
                             better.open.thre.list=seq(from=1e-3, to=3e-3, by=2e-4), ## original
                             train.range=4:6, valid.range=6:9, test.start=3,
                             best=34, open.thre=0.0036, close.thre=0.0008,
                             better=TRUE,spread.limit=FALSE,multiplier=1000,
                             opti.range=3:9, int.factor=100,night="02:30:00",
                             n.train=3, target=8,
                             exchange="sc", better.good.target=c(5,6,7,9))


R2 <- function(pred, obs,form="traditional") {
  n <- sum(complete.cases(pred))
  return (1 - (sum((obs-pred)^2, na.rm = TRUE)/((n-1)*var(obs, na.rm = TRUE))))
}

get.product.date.time <- function(contracts) { ## 获得品种合约的日期和时间
     date <- c() ## 日期列表
     time <- c() ## 时间列表
     for (contract in contracts) { ## 遍历所有合�?
         load(contract) ## 调用合约
         date <- c(date, data$date[data$continuous]) ## 合并日期
         time <- c(time, data$time[data$continuous]) ## 合并时间
       }
     date.time <- get.date.time(date,time) ## 转换日期和时�?
     return(date.time)
}

get.date.time <- function(date, time) {
  dates <- str_c(substr(date,1,4), substr(date,5,6), substr(date,7,8), sep="/")
  date.time <- as.POSIXlt(paste(dates, time))
  return (date.time)
}

get.weight<- function(m, s, m_star){## ����Ȩ�صĳ���
  s_inv = solve(s) ## ��������
  ones = rep(1, length(m)) ## ȫ����1������
  s_inv_ones = s_inv %*% ones ## ��Щ��ʽ����Ͷ������Ż��Ľ̿���ó�
  s_inv_m = s_inv %*% m
  A = (m %*% s_inv_ones)[1, 1]
  B = (m %*% s_inv_m)[1, 1]
  C = (ones %*% s_inv_ones)[1, 1]
  D = B * C - A^2
  ((B - m_star * A) * s_inv_ones + (m_star * C - A) * s_inv_m) / D
}

get.each.pnl.stat <- function(each.pnl) {
  sharp <- 0
  if (length(each.pnl)>1) sharp <- mean(each.pnl)/sd(each.pnl)*sqrt(length(each.pnl))
  all <- cumsum(each.pnl)
  cum.max.pnl <- cummax(all)
  cum.min.pnl <- cummin(all)
  drop.down <- cum.max.pnl-all
  rise.up <- all-cum.min.pnl
  pl.ratio <- clean(max(rise.up)/max(drop.down))
  win <- each.pnl[each.pnl>0]
  lose <- -each.pnl[each.pnl<0]
  prob <- ifelse(length(each.pnl)>0,length(win)/length(each.pnl),0)
  mean.win <- ifelse(length(win)>0,mean(win),0)
  mean.lose <- ifelse(length(lose)>0,mean(lose),0)
  ratio <- ifelse(length(lose)>0, mean.win/mean.lose, 10)
  index <- prob/(1-prob)*ratio
  num <- length(each.pnl)
  #  return (c(sharp,tail(all,1),pl.ratio,prob,ratio))
  period <- sum(all==cum.max.pnl)/length(all)
  if (length(all)==0) period <- 0
  return (list(sharp=sharp,pnl=tail(all,1),pl.ratio=pl.ratio,
               prob=prob,ratio=ratio,index=index,num=num,period=period))
}

parse.dire.fast <- function(dire,pattern,product,object.dire="d:/liwei/book/data/binary/arb5m/", data.type="futures") { ## ����data.typeһ�������
  file.list <- list.files(dire, recursive=TRUE,pattern=pattern)
  file.list <- file.list[grep(paste("/",product,sep=""), file.list)]
  start.prod <- as.numeric(regexpr(paste("/",product,sep=""),file.list[1]))
  check.dup <- substr(file.list, start.prod+nchar(product)+1,start.prod+nchar(product)+1)
  file.list <- file.list[check.dup>="0" & check.dup<="9"]
  if (length(file.list)==0) return(1)
  for (i in 1:length(file.list)) {
    file <- file.list[i]
    cat(file,"\n")
    if (file.info(paste(dire,file,sep="/"))$size==0) next
    data <- get.data.5m.fast(dire,file,data.type) ## ������data.typeһ��
    if (length(data)==1) next
    contract <- data$contract[1]
    dir.create(paste(object.dire,contract,sep=""),showWarnings=FALSE)
    new.file <- gsub("_","/",paste(substr(file,10,nchar(file)-3),"RData",sep=""))
    heaven <- as.numeric(gregexpr("/",new.file)[[1]])
    if (length(heaven)>1) new.file <- substr(new.file, heaven[1]+1,nchar(new.file))
    save(data,file=paste(object.dire,new.file,sep=""))
  }
}

get.data.5m.fast <- function(dire,file, data.type="futures") {
  #data <- read.csv(paste(dire,file,sep="/"),stringsAsFactors=FALSE)
  data <- fread(paste(dire,file,sep="/"),stringsAsFactors=FALSE)
  data <- as.data.frame(data)
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
    selected.column <- c("contract", "date", "price", "bid", "ask", "bid.qty", "ask.qty", "now.wpr", "new.wpr")
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
