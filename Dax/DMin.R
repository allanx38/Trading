# 1. Add data to csv
# 2. Create TAP files
# 3. Calc DM values
# 4. Generate res file (if necessary)

setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")

# -------------------------------------------------------------
# 1. Add Data to csv

add_line<- function(indata, line_num, Mkt){
  indata$Date <- as.POSIXct(indata$Date,format='%m/%d/%Y')
  indata$Date <- as.character.Date(indata$Date,format='%d/%m/%Y')
  Mkt_new <- rbind(Mkt,indata[line_num,2:6])
  return(Mkt_new)
}

# read from clipboard
indata <- read.table("clipboard")
colnames(indata) <- c('Mkt','Date','Open','High','Low','Close')
tail(indata)

#Dax
Dax <- read.csv("../Data/Dax_2000.csv")
tail(Dax)
#ln <- nrow(Dax)
#Dax <- Dax[-3689,]
Dax_2 <- add_line(indata,1,Dax)
tail(Dax_2)
write.csv(Dax_2,"../Data/Dax_2000.csv",row.names=FALSE)

#CAC
CAC <- read.csv("../Data/CAC_2000.csv")
tail(CAC)
CAC_2 <- add_line(indata,2,CAC)
tail(CAC_2)
write.csv(CAC_2,"../Data/CAC_2000.csv",row.names=FALSE)

#FTSE
F100 <- read.csv("../Data/F100_2000.csv")
tail(F100)
F100_2 <- add_line(indata,3,F100)
tail(F100_2)
write.csv(F100_2,"../Data/F100_2000.csv",row.names=FALSE)

#N225
Nik <- read.csv("../Data/N225_2000.csv")
tail(Nik)
Nik_2 <- add_line(indata,4,Nik) #4th line fr Nikkei
tail(Nik_2)
write.csv(Nik_2,"../Data/N225_2000.csv",row.names=FALSE)

#Oz
Oz <- read.csv("../Data/Oz_2000.csv")
tail(Oz)
Oz_2 <- add_line(indata,5,Oz) #4th line fr Nikkei
tail(Oz_2)
write.csv(Oz_2,"../Data/Oz_2000.csv",row.names=FALSE)

# -------------------------------------------------------------
# 2. Create TAP files

#Add TA, no candlesticks to Data
addTAInd_prev <- function(Mkt, nm){
  Mkt[2:5] <- round(Mkt[2:5])
  #atr
  atr <- ATR(Mkt[,c("High","Low","Close")], n=14)
  Mkt$atr <- round(atr[,"atr"])
  #SMA
  sma <- round(SMA(Mkt["Close"], 10))
  Mkt <- cbind(Mkt, sma)
  Mkt$Diff <- ifelse(!is.na(Mkt$sma), Mkt$Close - Mkt$sma, NA)
  #aroon
  ar <- aroon(Mkt$Close, n=20)
  Mkt <- cbind(Mkt, ar)
  #roc
  Mkt$mom <- round(momentum(Mkt$Close,n=12)) 
  #Add prev
  Mkt$prev_smadiff <- c( NA, Mkt$Diff[ - length(Mkt$Diff) ] )
  Mkt$prev_aroon_up <- c( NA, Mkt$aroonUp[ - length(Mkt$aroonUp) ] )
  Mkt$prev_aroon_dn <- c( NA, Mkt$aroonDn[ - length(Mkt$aroonDn) ] )
  Mkt$prev_aroon_os <- c( NA, Mkt$oscillator[ - length(Mkt$oscillator) ] )
  Mkt$prev_mom <- c( NA, Mkt$mom[ - length(Mkt$mom) ] )
  Mkt$pl <- Mkt$Close - Mkt$Open
  #write csvfile
  write.csv(Mkt,paste('../Data/', nm, '_tap.csv',sep=""),row.names=FALSE)
}

Dax <- read.csv("../Data/Dax_2000.csv")
tail(Dax)
addTAInd_prev(Dax,"Dax")

CAC <- read.csv("../Data/CAC_2000.csv")
tail(CAC)
addTAInd_prev(CAC,"CAC")

F100 <- read.csv("../Data/F100_2000.csv")
tail(F100)
addTAInd_prev(F100,"F100")

Dow <- read.csv("../Data/Dow_2000.csv")
tail(Dow)
addTAInd_prev(Dow,"Dow")

N225 <- read.csv("../Data/N225_2000.csv")
tail(N225)
addTAInd_prev(N225,"N225")

Oz <- read.csv("../Data/Oz_2000.csv")
tail(Oz)
addTAInd_prev(Oz,"Oz")

# -------------------------------------------------------------
# 3. Calc DM values - today

# three comparison functions
au_df <- function(Mkt, au, df){
  sum ( Mkt[ (Mkt$prev_aroon_up == au) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ] ,na.rm=T)
}

ad_df <- function(Mkt, ad, df){
  sum ( Mkt[ (Mkt$prev_aroon_dn == ad) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ] ,na.rm=T)
}

os_df <- function(Mkt, os, df){
  sum ( Mkt[ (Mkt$prev_aroon_os == os) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ] ,na.rm=T)
}

# # applies comp functions to one row, uisng current rows
r_p_ind <- function(Mkt, nr){
  au <- Mkt$aroonUp[nr] 
  ad <- Mkt$aroonDn[nr] 
  os <- Mkt$oscillator[nr] 
  df <- Mkt$Diff[nr] 
  c <- au_df(Mkt,au,df)
  d <- ad_df(Mkt,ad,df)
  e <- os_df(Mkt,os,df)
  e2 <- c+d+e
  return(c(c,d,e,e2))
}

#a. ------------- Dax
Dax_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dax_tap.csv")
Dax_tap$Date[nrow(Dax_tap)]
ln <- nrow(Dax_tap) ;ln
dx_res <- r_p_ind(Dax_tap, ln) ;dx_res

#b. ------------ CAC
CAC_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/CAC_tap.csv")
CAC_tap$Date[nrow(CAC_tap)]
ln <- nrow(CAC_tap) ;ln
cac_res <- r_p_ind(CAC_tap, ln) ;cac_res

# c. ------------ FTSE 
F100_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/F100_tap.csv")
F100_tap$Date[nrow(F100_tap)]
ln <- nrow(F100_tap) ;ln
f100_res <- r_p_ind(F100_tap, ln) ;f100_res

# d. ------------ Dow 
Dow_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dow_tap.csv")
Dow_tap$Date[nrow(Dow_tap)]
ln <- nrow(Dow_tap) ;ln
dow_res <- r_p_ind(Dow_tap, ln) ;dow_res

# e. ------------ N225 
N225_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/N225_tap.csv")
N225_tap$Date[nrow(N225_tap)]
ln <- nrow(N225_tap) ;ln
n225_res <- r_p_ind(N225_tap, ln) ;n225_res

# f. ------------ Oz 
Oz_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Oz_tap.csv")
Oz_tap$Date[nrow(Oz_tap)]
ln <- nrow(Oz_tap) ;ln
oz_res <- r_p_ind(Oz_tap, ln) ;oz_res

paste(c('Dax',as.character.Date(Dax_tap$Date[nrow(Dax_tap)]),dx_res))
paste(c('CAC',as.character.Date(CAC_tap$Date[nrow(CAC_tap)]),cac_res))
paste(c('FTSE',as.character.Date(F100_tap$Date[nrow(F100_tap)]),f100_res))
paste(c('Dow',as.character.Date(Dow_tap$Date[nrow(Dow_tap)]),dow_res))
paste(c('N225',as.character.Date(N225_tap$Date[nrow(N225_tap)]),n225_res))
paste(c('Oz',as.character.Date(Oz_tap$Date[nrow(Oz_tap)]),oz_res))

# ------------------------------------------------
# res
test <- function(Mkt,st){
  res <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  res_mkt <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  #browser()
  colnames(res_mkt) <- colnames(Mkt[c(1,2,3,4,5)])
  ln <- nrow(Mkt)
  
  for(i in st:ln){
    mkt1 <- Mkt[1:i,]
    lt_row <- nrow(mkt1)
    r <- r_p(Mkt,lt_row)
    res <- rbind(res,r)
    res_mkt <- rbind(res_mkt,Mkt[i,c(1,2,3,4,5)])
    #browser()
  }
  colnames(res) <- c('a1','a2','a3','a4','pl')
  res <- cbind(res_mkt,res)
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

# to do
# calc rr files, calc DM pl, merge these ...
Dax_tap <- read.csv("../Data/Dax_tap.csv")
CAC_tap <- read.csv("../Data/CAC_tap.csv")
F100_tap <- read.csv("../Data/F100_tap.csv")
Dow_tap <- read.csv("../Data/Dow_tap.csv")
N225_tap <- read.csv("../Data/N225_tap.csv")
Oz_tap <- read.csv("../Data/Oz_tap.csv")

tail(Dow_tap)

# calc DM pl
dx_rr <- test(Dax_tap,2000)
dx_rr$pl2 <- ifelse(dx_rr$a4>0,dx_rr$pl,-dx_rr$pl)

cc_rr <- test(CAC_tap,2000)
cc_rr$pl2 <- ifelse(cc_rr$a4>0,cc_rr$pl,-cc_rr$pl)

ft_rr <- test(F100_tap,2000)
ft_rr$pl2 <- ifelse(ft_rr$a4>0,ft_rr$pl,-ft_rr$pl)

dw_rr <- test(Dow_tap,2000)
dw_rr$pl2 <- ifelse(dw_rr$a4>0,dw_rr$pl,-dw_rr$pl)

ni_rr <- test(N225_tap,2000)
ni_rr$pl2 <- ifelse(ni_rr$a4>0,ni_rr$pl,-ni_rr$pl)

oz_rr <- test(Oz_tap,2000)
oz_rr$pl2 <- ifelse(oz_rr$a4>0,oz_rr$pl,-oz_rr$pl)

tail(dx_rr)
tail(cc_rr)
tail(ft_rr)
tail(dw_rr)

# Change date format, extract Date and pl, rename pl
dx_rr$Date <- as.POSIXct(dx_rr$Date,format='%d/%m/%Y') ;dx_rr$Date[20]
cc_rr$Date <- as.POSIXct(cc_rr$Date,format='%d/%m/%Y') ;cc_rr$Date[20]
ft_rr$Date <- as.POSIXct(ft_rr$Date,format='%d/%m/%Y') ;ft_rr$Date[20]
dw_rr$Date <- as.POSIXct(dw_rr$Date,format='%d/%m/%Y') ;dw_rr$Date[20]
ni_rr$Date <- as.POSIXct(ni_rr$Date,format='%d/%m/%Y') ;ni_rr$Date[20]
oz_rr$Date <- as.POSIXct(oz_rr$Date,format='%d/%m/%Y') ;oz_rr$Date[20]

tail(un1)

# merge pairs
un1 <- merge(dx_rr[,c(1,11)],
             cc_rr[,c(1,11)], 
            by='Date',all=T)
colnames(un1) <- c('Date', 'DxPL', 'CcPL')

un2 <- merge(ft_rr[,c(1,11)],
             dw_rr[,c(1,11)], 
            by='Date',all=T)
colnames(un2) <- c('Date', 'FtPL', 'DwPL')

un3 <- merge(ni_rr[,c(1,11)],
             oz_rr[,c(1,11)], 
             by='Date',all=T)
colnames(un3) <- c('Date', 'NikPL', 'OzPL')

# final merge
un1_2 <- merge(un1,un2,by='Date',all=T)
un_tot <- merge(un1_2,un3,by='Date',all=T)
un_tot[is.na(un_tot)] <- 0   #remove zeros
un_tot$tot <- un_tot$DxPL+un_tot$CcPL+un_tot$FtPL+un_tot$DwPL+un_tot$NikPL+un_tot$OzPL
tail(un_tot,n=100)
head(un_tot,n=100)

sum(un_tot$tot)

# win rate
p <- un_tot$tot[un_tot$tot>0]
q <- un_tot$tot[un_tot$tot<0]
length(p)/(length(q)+length(p))
sum(p)
sum(q)

# week days
un_tot2 <- un_tot
un_tot2$Date <- as.POSIXct(un_tot2$Date,format='%Y-%m-%d')
un_tot2$wd <- weekdays(un_tot2$Date)

sum(un_tot2[un_tot2$wd=='Monday',8])
p <- un_tot2[un_tot2$tot>0 & un_tot2$wd=='Monday',8]
q <- un_tot2[un_tot2$tot<0 & un_tot2$wd=='Monday',8]
length(p)/(length(q)+length(p))

sum(un_tot2[un_tot2$wd=='Tuesday',8])
sum(un_tot2[un_tot2$wd=='Wednesday',8])
sum(un_tot2[un_tot2$wd=='Thursday',8])
sum(un_tot2[un_tot2$wd=='Friday',8])
