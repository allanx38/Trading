
setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")
source("D:/Allan/DropBox/RWorkingDir/Trading/Dax/addTA_fnc.R")
library(TTR)
library(candlesticks)
library(Quandl)

add_line<- function(indata, line_num, Mkt){
  indata$Date <- as.POSIXct(indata$Date,format='%m/%d/%Y')
  indata$Date <- as.character.Date(indata$Date,format='%d/%m/%Y')
  Mkt_new <- rbind(Mkt,indata[line_num,2:6])
  return(Mkt_new)
}

# read from clipboard
indata <- read.table("clipboard")
colnames(indata) <- c('Mkt','Date','Open','High','Low','Close')

#Dax
Dax <- read.csv("../Data/Dax_2000.csv")
Dax_2 <- add_line(indata,1,Dax)
tail(Dax_2)

#N225
Nik <- read.csv("../Data/N225_2000.csv")
tail(Nik)
Nik_2 <- add_line(indata,4,Nik) #4th line fr Nikkei
tail(Nik_2)
write.csv(Nik_2,"../Data/N225_2000.csv",row.names=FALSE)

tail(dax2)

# create Mkt_tap.csv
#setwd("F:/Allan/R Stuff/Dax")
#source("F:/Allan/R Stuff/Dax/addTA_fnc.R")
Dax <- read.csv("../Data/Dax_2000.csv")
addTAInd_prev(Dax,"Dax")

CAC <- read.csv("../Data/CAC_2000.csv")
addTAInd_prev(CAC,"CAC")

F100 <- read.csv("../Data/F100_2000.csv")
addTAInd_prev(F100,"F100")

Dow <- read.csv("../Data/Dow_2000.csv")
addTAInd_prev(Dow,"Dow")

N225 <- read.csv("../Data/N225_2000.csv")
addTAInd_prev(N225,"N225")

Oz <- read.csv("../Data/Oz_2000.csv")
addTAInd_prev(Oz,"Oz")


Dax_tap <- read.csv("../Data/Dax_tap.csv")

Dax_tap <- read.csv("../Data/Dow_tap.csv")

tail(Dax_tap[, c(1,2,3,4,5)])
ln <- nrow(Dax_tap)
ln <- ln 
r_p(Dax_tap,ln)
r_p_ln(Dax_tap,ln)
r_p_ind(Dax_tap) #curr vals

ln <- nrow(Dax_tap)
tail(Dax_tap[1:(ln-2),])
r_p_ln(Dax_tap[1:(ln-2),])

au <- Dax_tap$prev_aroon_up[ln] 
ad <- Dax_tap$prev_aroon_dn[ln] 
os <- Dax_tap$prev_aroon_os[ln]
df <- Dax_tap$prev_smadiff[ln] 

Mkt <- Dax_tap
sum ( Mkt[ (Mkt$prev_aroon_up == au) & 
             (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
           c(20) ] ,na.rm=T)

Dax_tap[ (Dax_tap$prev_aroon_up == au), c(20)]

colnames(Dax_tap)

# -------------------------------

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



# ----------------------------------------------------

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

source("D:/Allan/DropBox/RWorkingDir/Trading/Dax/addTA_fnc.R")
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
r_p_ind(Mkt_ta, ln)

r_p_ln(Mkt_ta)


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
r_p_ind2(Mkt_ta, ln)

# -----------------------

r_p_ind2 <- function(Mkt, nr){
  #browser()
  Mkt <- AddPrev(Mkt)
  au <- Mkt$aroonUp[nr] 
  ad <- Mkt$aroonDn[nr] 
  os <- Mkt$oscillator[nr] 
  df <- Mkt$Diff[nr] 
  ln <- nrow(Mkt)
  Mkt <- Mkt[-ln,]
  c <- au_df(Mkt,au,df)
  d <- ad_df(Mkt,ad,df)
  e <- os_df(Mkt,os,df)
  e2 <- c+d+e
  return(c(c,d,e,e2))
}

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

