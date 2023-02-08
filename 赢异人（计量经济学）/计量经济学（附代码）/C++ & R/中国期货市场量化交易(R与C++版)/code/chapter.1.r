## chapter 1
product <- "rb"
DATA.PATH <- "d:/liwei/book/data/"
get.data <- function(d) {
  load(paste(DATA.PATH, d, sep = ""))
  return(data)
}
data <- get.data("rb1701.RData")
all.data <- data[data$continuous,]
cut.len <- nrow(all.data)
data <- get.data("rb1705.RData")
all.data <- rbind(all.data, data[data$continuous,])
## Figure 1.1
plot(all.data$date.time, all.data$price, type="l", main="rb", xlab="date", ylab="price")
points(all.data$date.time[cut.len], all.data$price[cut.len], cex=1)
points(all.data$date.time[cut.len+1], all.data$price[cut.len+1], cex=1)
