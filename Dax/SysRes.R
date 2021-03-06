source("D:/Allan/DropBox/RWorkingDir/Trading/Dax/addTA_fnc.R")
setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dax_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/CAC_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/N225_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dow_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Oz_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/F100_ta.csv")

ln <- nrow(Mkt) ;ln
lw <- ln - 300
Mkt <- Mkt[lw:ln,]
#Mkt <- Mkt[1000:1300,]
head(Mkt)
nrow(Mkt)

b <- calcT2(Mkt,5)
sum(b$PL2, na.rm=T)
sum(b$PL2>0, na.rm=T) / (sum(b$PL2>0, na.rm=T) + sum(b$PL2<0, na.rm=T))
tail(b$PL2)
tail(b)
ln <- nrow(b)
ln1 <- ln - 10
sm <- b$PL2[ln1:ln]
sum(sm, na.rm=T)

bb <- tail(b[,-c(3,4,7,8,9,11,14,15)],n=100)
tail(bb,n=20)
colnames(b)
write.csv(bb,"test_results_1yr_N225.csv")
write.csv(b,"test_results_1yr_dax.csv")

c('run',' 2')
b <- calcT2(Mkt,2)
sum(b$PL2, na.rm=T)
sum(b$PL2>0, na.rm=T) / (sum(b$PL2>0, na.rm=T) + sum(b$PL2<0, na.rm=T))
bb <- na.omit(b) 
mean(bb[bb$PL2>0,18])
mean(bb[bb$PL2<0,18])

c('run',' 3')
b <- calcT2(Mkt,3)
sum(b$PL2, na.rm=T)
sum(b$PL2>0, na.rm=T) / (sum(b$PL2>0, na.rm=T) + sum(b$PL2<0, na.rm=T))
bb <- na.omit(b) 
mean(bb[bb$PL2>0,18])
mean(bb[bb$PL2<0,18])

c('run',' 4')
b <- calcT2(Mkt,4)
sum(b$PL2, na.rm=T)
sum(b$PL2>0, na.rm=T) / (sum(b$PL2>0, na.rm=T) + sum(b$PL2<0, na.rm=T))
bb <- na.omit(b) 
mean(bb[bb$PL2>0,18])
mean(bb[bb$PL2<0,18])

c('run',' 5')
b <- calcT2(Mkt,5)
sum(b$PL2, na.rm=T)
sum(b$PL2>0, na.rm=T) / (sum(b$PL2>0, na.rm=T) + sum(b$PL2<0, na.rm=T))
bb <- na.omit(b) 
mean(bb[bb$PL2>0,18])
mean(bb[bb$PL2<0,18])


c('run',' 6')
b <- calcT2(Mkt,6)
sum(b$PL2, na.rm=T)
sum(b$PL2>0, na.rm=T) / (sum(b$PL2>0, na.rm=T) + sum(b$PL2<0, na.rm=T))
bb <- na.omit(b) 
mean(bb[bb$PL2>0,18])
mean(bb[bb$PL2<0,18])

c('run',' 7')
b <- calcT2(Mkt,7)
sum(b$PL2, na.rm=T)
sum(b$PL2>0, na.rm=T) / (sum(b$PL2>0, na.rm=T) + sum(b$PL2<0, na.rm=T))
bb <- na.omit(b) 
mean(bb[bb$PL2>0,18])
mean(bb[bb$PL2<0,18])

c('run',' 8')
b <- calcT2(Mkt,8)
sum(b$PL2, na.rm=T)
sum(b$PL2>0, na.rm=T) / (sum(b$PL2>0, na.rm=T) + sum(b$PL2<0, na.rm=T))
bb <- na.omit(b) 
mean(bb[bb$PL2>0,18])
mean(bb[bb$PL2<0,18])

c('run',' 9')
b <- calcT2(Mkt,9)
sum(b$PL2, na.rm=T)
sum(b$PL2>0, na.rm=T) / (sum(b$PL2>0, na.rm=T) + sum(b$PL2<0, na.rm=T))
bb <- na.omit(b) 
mean(bb[bb$PL2>0,18])
mean(bb[bb$PL2<0,18])