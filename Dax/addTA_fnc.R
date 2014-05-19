library(TTR)
library(candlesticks)
library(Quandl)

Quandl.auth("mW11caB1btTqNnBWGhtg")

Update_from_yahoo <- function( fil,nm, yh_tick, x, st, en){
  for(i in 1:length(fil)){
    browser()
    Mkt <- read.csv(fil[i])
    #start <- format(Sys.Date(), format="%Y-%m-%d")
    #end <- format((Sys.Date() - x), format="%Y-%m-%d")
    ydax = Quandl(yh_tick[i], start_date=st, end_date=en)
    ydax <- ydax[,c(1,2,3,4,5)]
    ydax <- ydax[order(ydax$Date),]
    ydax$Date <- as.character(ydax$Date)
    
    Mkt <- rbind(Mkt,ydax)
    Mkt <- unique(Mkt)
    write.csv(Mkt,paste('../Data/', nm[i], '_2000.csv',sep=""),row.names=FALSE)
  }
}

# Adds the TA info to all the indices
addTA <- function(fil, nm){
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
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
    lw <- quantile(Mkt$mom, na.rm=T, probs=0.25) 
    hi <- quantile(Mkt$mom, na.rm=T, probs=0.75)
    Mkt$hi <- round(hi)
    Mkt$lw <- round(lw)
    
    #Candlesticks
    Mkt$Date <- as.POSIXct(Mkt$Date,format='%Y-%m-%d')
    Mkt_xts <- xts(Mkt[,c(2,3,4,5)],Mkt$Date)
    #browser()
    hh <- as.data.frame(CSPHammer(Mkt_xts))
    hi <- as.data.frame(CSPInvertedHammer(Mkt_xts))
    en <- as.data.frame(CSPEngulfing(Mkt_xts))
    dj <- as.data.frame(CSPDoji(Mkt_xts))
    #back to data fram
    Mkt <- cbind(Mkt,hh)
    Mkt <- cbind(Mkt,hi)
    Mkt <- cbind(Mkt,dj)
    Mkt <- cbind(Mkt,en)
    
    #browser()
    write.csv(Mkt,paste(nm[i], '_ta.csv',sep=""),row.names=FALSE)
    #write.csv(Mkt,paste('../Data/', nm[i], '_ta.csv',sep=""),row.names=FALSE)
  }
}

# Add TA info to just one indice
addTAInd <- function(Mkt, nm){
  #browser()
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
  lw <- quantile(Mkt$mom, na.rm=T, probs=0.25) 
  hi <- quantile(Mkt$mom, na.rm=T, probs=0.75)
  Mkt$hi <- round(hi)
  Mkt$lw <- round(lw)
  
  #Candlesticks
  Mkt_xts <- xts(Mkt[,c(2,3,4,5)],Mkt$Date)
  #browser()
  hh <- as.data.frame(CSPHammer(Mkt_xts))
  hi <- as.data.frame(CSPInvertedHammer(Mkt_xts))
  en <- as.data.frame(CSPEngulfing(Mkt_xts))
  dj <- as.data.frame(CSPDoji(Mkt_xts))
  #back to data fram
  Mkt <- cbind(Mkt,hh)
  Mkt <- cbind(Mkt,hi)
  Mkt <- cbind(Mkt,dj)
  Mkt <- cbind(Mkt,en)
  
  #browser()
  #write.csv(Mkt,nm,row.names=FALSE)
  write.csv(Mkt,paste('../Data/', nm, sep=""),row.names=FALSE)
}

# Calculate the System values
# 1 add prev values to Mkt (pre defined)
AddPrev <- function(Mkt){
  Mkt <- Mkt[, 1:12]
  Mkt$prev_smadiff <- c( NA, Mkt$Diff[ - length(Mkt$Diff) ] )
  Mkt$prev_aroon_up <- c( NA, Mkt$aroonUp[ - length(Mkt$aroonUp) ] )
  Mkt$prev_aroon_dn <- c( NA, Mkt$aroonDn[ - length(Mkt$aroonDn) ] )
  Mkt$prev_aroon_os <- c( NA, Mkt$oscillator[ - length(Mkt$oscillator) ] )
  Mkt$prev_mom <- c( NA, Mkt$mom[ - length(Mkt$mom) ] )
  Mkt$pl <- Mkt$Close - Mkt$Open
  return(Mkt)
}

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

# applies comp functions to one row, uisng prev values
r_p <- function(Mkt, nr){
  #browser()
  #Mkt <- AddPrev(Mkt)
  #b <- Mkt$Date[nr]
  au <- Mkt$prev_aroon_up[nr] 
  ad <- Mkt$prev_aroon_dn[nr] 
  os <- Mkt$prev_aroon_os[nr] 
  df <- Mkt$prev_smadiff[nr]  
  c <- au_df(Mkt,au,df)
  d <- ad_df(Mkt,ad,df)
  e <- os_df(Mkt,os,df)
  e2 <- c+d+e
  f <- Mkt$pl[nr]
  
  return(c(c,d,e,e2,f))
}

# loops thru r_p - pass in how many times ...
run_rp <- function(Mkt,ln){
  #browser()
  Mkt <- AddPrev(Mkt)
  a <- c(0,0,0,0,0)
  for(i in 31:ln){
    b <- r_p(Mkt,i)
    a <- rbind(a,b)
  }
  a <- a[-c(1),]
  return(a)
}

# # applies comp functions to one row, uisng current rows
r_p_ind <- function(Mkt, nr){
  #browser()
  #Mkt <- AddPrev(Mkt)
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