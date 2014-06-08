
source("D:/Allan/DropBox/RWorkingDir/Trading/Dax/addTA_fnc.R")
setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")

Dax_tap <- read.csv("../Data/Dax_tap.csv")

tail(Dax_tap)

ln <- nrow(Dax_tap)

rr <- test2(Dax_tap,2000)
sm <- sum(ifelse(rr$a4>0,rr$pl,-rr$pl));sm
sm / nrow(rr)


rr$pl2 <- ifelse(rr$a4>0,rr$pl,-rr$pl)
sum(rr$pl2>0) / (sum(rr$pl2>0, na.rm=T) + sum(rr$pl2<0, na.rm=T))
sum(rr$pl2>0) / nrow(rr)

p <- rr$pl2[rr$pl2>0]
sum(p)
length(p)
sum(p) / length(p)

neg <- rr$pl2[rr$pl2<0]
sum(neg)
length(neg)
sum(neg) / length(neg)


sum(rr$pl2>0)
sum(rr[rr$pl>0,])

test <- function(Mkt,st){
  res <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  ln <- nrow(Mkt)
  for(i in st:ln){
    mkt1 <- Mkt[1:i,]
    lt_row <- nrow(mkt1)
    r <- r_p(Dax_tap,lt_row)
    res <- rbind(res,r)
    #browser()
  }
  colnames(res) <- c('a1','a2','a3','a4','pl')
  res <- res[-1,]
  return(res)
}

test2 <- function(Mkt,st){
  res <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  ln <- nrow(Mkt)
  for(i in st:ln){
    bg <- (i - 300)
    mkt1 <- Mkt[bg:i,]
    lt_row <- nrow(mkt1)
    r <- r_p(Dax_tap,i)
    res <- rbind(res,r)
    #browser()
  }
  colnames(res) <- c('a1','a2','a3','a4','pl')
  res <- res[-1,]
  return(res)
}