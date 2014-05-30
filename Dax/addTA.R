
setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")
library(TTR)
library(candlesticks)
library(Quandl)

#nm <- c("Dax", "CAC", "F100", "Dow", "N225", "Oz")
#yh_tick <- c("YAHOO/INDEX_GDAXI","YAHOO/INDEX_FCHI","YAHOO/INDEX_FTSE","YAHOO/INDEX_DJI",
#           "YAHOO/INDEX_N225","YAHOO/INDEX_AORD")

nm <- c("Dax", "CAC", "F100", "Dow", "Oz")
yh_tick <- c("YAHOO/INDEX_GDAXI","YAHOO/INDEX_FCHI","YAHOO/INDEX_FTSE","YAHOO/INDEX_DJI",
             "YAHOO/INDEX_AORD")

fil <- c("../Data/Dax_2000.csv",
         "../Data/CAC_2000.csv", 
         "../Data/F100_2000.csv",
         "../Data/Dow_2000.csv",
         "../Data/N225_2000.csv",
         "../Data/Oz_2000.csv")

source("D:/Allan/DropBox/RWorkingDir/Trading/Dax/addTA_fnc.R")

ydax = Quandl(YAHOO/INDEX_DJI, "2014-04-30","2014-05-10")
ydax <- ydax[,c(1,2,3,4,5)]
ydax <- ydax[order(ydax$Date),]
ydax$Date <- as.character(ydax$Date)

Mkt <- rbind(Mkt,ydax)
Mkt <- unique(Mkt)
write.csv(Mkt,paste('../Data/', nm[i], '_2000.csv',sep=""),row.names=FALSE)

# 1. update Data
#Update_from_yahoo(fil, nm, yh_tick, 7, "2014-04-30","2014-05-10")

# 2 Add TA values
#addTA(fil,nm)

source("D:/Allan/DropBox/RWorkingDir/Trading/Dax/addTA_fnc.R")
setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")

Get_Num <- function(Mkt, filname){
  #browser()
  Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y')
  addTAInd(Mkt, filname)
  
  ta_fil <- paste('../Data/',filname,sep="")
  Mkt_ta <- read.csv(ta_fil)
  Mkt_ta$Date[nrow(Mkt_ta)]
  ln <- nrow(Mkt_ta) ;ln
  lw <- ln - 300 ;lw
  Mkt_ta <- Mkt_ta[lw:ln,]
  ln <- nrow(Mkt_ta) ;ln
  return(r_p_ind(Mkt_ta, ln))
}

Mkt <- read.csv("../Data/Dax_2000.csv")
as.character(Mkt$Date[nrow(Mkt)])
Get_Num(Mkt,"Dax_ta.csv")

Mkt <- read.csv("../Data/CAC_2000.csv")
as.character(Mkt$Date[nrow(Mkt)])
Get_Num(Mkt,"CAC_ta.csv")

Mkt <- read.csv("../Data/F100_2000.csv")
as.character(Mkt$Date[nrow(Mkt)])
Get_Num(Mkt,"F100_ta.csv")

Mkt <- read.csv("../Data/Dow_2000.csv")
as.character(Mkt$Date[nrow(Mkt)])
Get_Num(Mkt,"Dow_ta.csv")

Mkt <- Mkt <- read.csv("../Data/N225_2000.csv")
as.character(Mkt$Date[nrow(Mkt)])
Get_Num(Mkt,"N225_ta.csv")

Mkt <- read.csv("../Data/Oz_2000.csv")
as.character(Mkt$Date[nrow(Mkt)])
Get_Num(Mkt,"Oz_ta.csv")

#3 indv
#a. ------------- Dax
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dax_2000.csv")
Mkt[nrow(Mkt),]
#Mkt$Date <- as.POSIXct(Mkt$Date,format='%Y-%m-%d') ;Mkt$Date[20]
Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y') ;Mkt$Date[20]
addTAInd(Mkt, "Dax_ta.csv")

Mkt_ta <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dax_ta.csv")
Mkt_ta$Date[nrow(Mkt_ta)]
ln <- nrow(Mkt_ta) ;ln
lw <- ln - 300 ;lw
Mkt_ta <- Mkt_ta[lw:ln,]
ln <- nrow(Mkt_ta) ;ln
r_p_ind(Mkt_ta, ln)

#b. ------------ CAC
Mkt <- read.csv("../Data/CAC_2000.csv")
Mkt[nrow(Mkt),]
#Mkt$Date[20]
#Mkt$Date <- as.POSIXct(Mkt$Date,format='%Y-%m-%d') ;Mkt$Date[20]
Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y') ;Mkt$Date[20]
addTAInd(Mkt, "CAC_ta.csv")

Mkt_ta <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/CAC_ta.csv")
Mkt_ta$Date[nrow(Mkt_ta)]
ln <- nrow(Mkt_ta) ;ln
lw <- ln - 300 ;lw
Mkt_ta <- Mkt_ta[lw:ln,]
ln <- nrow(Mkt_ta) ;ln
r_p_ind(Mkt_ta, ln)

# c. FTSE 
Mkt <- read.csv("../Data/F100_2000.csv")
Mkt[nrow(Mkt),]
#Mkt$Date <- as.POSIXct(Mkt$Date,format='%Y-%m-%d') ;Mkt$Date[20]
Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y') ;Mkt$Date[20]
addTAInd(Mkt, "F100_ta.csv")

Mkt_ta <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/F100_ta.csv")
Mkt_ta$Date[nrow(Mkt_ta)]
ln <- nrow(Mkt_ta) ;ln
r_p_ind(Mkt_ta, ln)

# d. Dow 
Mkt <- read.csv("../Data/Dow_2000.csv")
Mkt[nrow(Mkt),]
#Mkt$Date <- as.POSIXct(Mkt$Date,format='%Y-%m-%d') ;Mkt$Date[20]
Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y') ;Mkt$Date[20]
addTAInd(Mkt, "Dow_ta.csv")

Mkt_ta <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dow_ta.csv")
Mkt_ta$Date[nrow(Mkt_ta)]
ln <- nrow(Mkt_ta) ;ln
lw <- ln - 300 ;lw
Mkt_ta <- Mkt_ta[lw:ln,]
ln <- nrow(Mkt_ta) ;ln
r_p_ind(Mkt_ta, ln)

# e. N225 
Mkt <- read.csv("../Data/N225_2000.csv")
Mkt[nrow(Mkt),]
#Mkt$Date <- as.POSIXct(Mkt$Date,format='%Y-%m-%d') ;Mkt$Date[20]
Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y') ;Mkt$Date[20]
addTAInd(Mkt, "N225_ta.csv")

Mkt_ta <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/N225_ta.csv")
Mkt_ta$Date[nrow(Mkt_ta)]
ln <- nrow(Mkt_ta) ;ln
lw <- ln - 300 ;lw
Mkt_ta <- Mkt_ta[lw:ln,]
ln <- nrow(Mkt_ta) ;ln
r_p_ind(Mkt_ta, ln)

# f. Oz 
Mkt <- read.csv("../Data/Oz_2000.csv")
Mkt[nrow(Mkt),]
#Mkt$Date <- as.POSIXct(Mkt$Date,format='%Y-%m-%d') ;Mkt$Date[20]
Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y') ;Mkt$Date[20]
addTAInd(Mkt, "Oz_ta.csv")

Mkt_ta <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Oz_ta.csv")
Mkt_ta$Date[nrow(Mkt_ta)]
ln <- nrow(Mkt_ta) ;ln
lw <- ln - 300 ;lw
Mkt_ta <- Mkt_ta[lw:ln,]
ln <- nrow(Mkt_ta) ;ln
r_p_ind(Mkt_ta, ln)

row(Mkt_ta$Date[Mkt_ta$Date=="2014-05-08"])

?pdf

savepdf <- function(file, width=16, height=10)
{
  fname <- paste("Figures/",file,".pdf",sep="")
  pdf(fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dax_2000.csv")

t <- tail(Mkt)
t
plot(t)
hist(t$Close)
savepdf("test")
hist(t$Close)
# Plotting commands here
dev.off()

Mkt_ta <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dow_ta.csv")
Mkt_ta$Date[nrow(Mkt_ta)]
ln <- nrow(Mkt_ta) ;ln
write.csv(Mkt_ta2,'../Data/Dow_ta_res.csv',row.names=FALSE)

# all indice
library(TTR)
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dow_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/N225_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Oz_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/F100_ta.csv")
ln <- nrow(Mkt) ;ln
dd <- run_rp(Mkt,ln)
tail(dd)
nrow(dd)
colnames(dd) <- c('au_df','ad_df','os_df','tot','pl2')
dd2 <- as.data.frame(dd)
tail(dd2)
Mkt <- cbind(Mkt[-c(1:30), ], dd2)
tail(Mkt)
length(Mkt)
#write.csv(Mkt[ , c(1,2,3,4,5,6,22,23,24,25,26)], "../Data/Dax_ta_res2.csv", row.names=F)

Mkt <- Mkt[ , c(1,2,3,4,5,6,22,23,24,25,26)]

Mkt$PL <- ifelse(Mkt$tot>0,Mkt$Close-Mkt$Open,Mkt$Open-Mkt$Close)
Mkt$WL <- ifelse(Mkt$PL>0,1,0)
sm <- SMA(Mkt$WL, 10)*10
Mkt <- cbind(Mkt, sm)

sm2 <- SMA(Mkt$sm, 10)
Mkt <- cbind(Mkt, sm2)

Mkt$PL2 <- ifelse(Mkt$sm>Mkt$sm2,Mkt$PL,NA)
tail(Mkt)

sum(Mkt$PL2, na.rm=T)
cc <- Mkt[complete.cases(Mkt),]
nrow(cc)
sum(cc$PL2>0)

sum(cc$PL2>0) / nrow(cc)

nrow(cc[cc$PL2>0],)
vv <- Mkt[Mkt$PL2 > 0,]
vv

sum(Mkt[ Mkt$PL > 0, 6])
sum(Mkt[ Mkt$e < 0, 6])
( sum(Mkt[ Mkt$e > 0, 6]) - sum(dd2[ dd2$e < 0, 6]) ) / 14

