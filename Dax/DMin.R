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
# 3. Calc DM values

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

Mkt_tap <- read.csv("../Data/Dax_tap.csv")
Mkt_tap <- read.csv("../Data/CAC_tap.csv")
Mkt_tap <- read.csv("../Data/F100_tap.csv")
Mkt_tap <- read.csv("../Data/Dow_tap.csv")
Mkt_tap <- read.csv("../Data/N225_tap.csv")
Mkt_tap <- read.csv("../Data/Oz_tap.csv")

tail(Dax_tap)
colnames(Dax_tap)
Dax_tap$Date <- as.POSIXct(Dax_tap$Date,format='%d/%m/%Y') ;Dax_tap$Date[20]
CAC_tap$Date <- as.POSIXct(CAC_tap$Date,format='%d/%m/%Y') ;CAC_tap$Date[20]
F100_tap$Date <- as.POSIXct(F100_tap$Date,format='%d/%m/%Y') ;F100_tap$Date[20]
un <- merge(Dax_tap[,c(1,18)],  
            CAC_tap[,c(1,18)], 
            by='Date')
un <- merge(un,  
            F100_tap[,c(1,18)], 
            by='Date')

            F100_tap[,c(1,18)],
            by='Date')
            Dow_tap[,c(1,18)],
            N225_tap[,c(1,18)],
            Oz_tap[,c(1,18)],
            by='Date')
tail(un)
