load("d:/liwei/book/data/chpt11.rb.data.RData")

## For the configuration files, please refer to cpp directory
date.time <- get.date.time(all.data$date, all.data$time)

## Figure 11-8
date.range <- all.data$date>="20141117" & all.data$date<="20150302"
plot(date.time[date.range], all.data$price[date.range], xlab="date", ylab="price",
     main="rb low price low volatility", type="l")

## Figure 11-9
date.range <- all.data$date>="20170701" & all.data$date<="20170901"
plot(date.time[date.range], all.data$price[date.range], xlab="date", ylab="price",
     main="rb high volatility trend", type="l")

## Figure 11-10
date.range <- all.data$date>="20170801" & all.data$date<"20171101"
plot(date.time[date.range], all.data$price[date.range], xlab="date", ylab="price",
     main="rb high volatility no trend", type="l")

## Figure 11-11
date.range <- all.data$date>="20161101" & all.data$date<"20161120"
plot(date.time[date.range], all.data$price[date.range], xlab="date", ylab="price",
     main="rb high volatility no trend", type="l")

load("d:/liwei/book/data/20100107.RData")
## Figure 11-12
plot(data$price, type="l", ylab="price")
