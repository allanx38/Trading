# Results 
Mkt = read.csv("F:/Allan/R Stuff/Dax/F100_ta.csv")
Mkt = read.csv("F:/Allan/R Stuff/Dax/N225_ta.csv")
Mkt = read.csv("F:/Allan/R Stuff/Dax/Dow_ta.csv")
Mkt = read.csv("F:/Allan/R Stuff/Dax/Oz_ta.csv")
Mkt = read.csv("F:/Allan/R Stuff/Dax/CAC_ta.csv")

#home
setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")
#Mkt <- read.csv("../Data/Dax_ta.csv", stringsAsFactors = FALSE)
Mkt <- read.csv("../Data/Dax_ta.csv")
Mkt <- read.csv("../Data/Dow_ta.csv")
Mkt <- read.csv("../Data/Dow_ta.csv")
Mkt <- AddPrev(Mkt)
tail(Mkt)

source("D:/Allan/DropBox/RWorkingDir/Trading/Dax/addTA_fnc.R")

# 1 -------- Ind records - prev values
ln <- nrow(Mkt) ;ln
r_p(Mkt, ln) ;Mkt$Date[ln] 

ln <- 3591
r_p(Mkt, ln) ;Mkt$Date[ln]

# ---------- 2 Entire indice
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
write.csv(Mkt[ , c(1,2,3,4,5,6,22,23,24,25,26)], "../Data/Dax_ta_res2.csv", row.names=F)

sum(dd2[ dd2$e > 0, 6])
sum(dd2[ dd2$e < 0, 6])
( sum(dd2[ dd2$e > 0, 6]) - sum(dd2[ dd2$e < 0, 6]) ) / 14

# 3 Indv ---------- today's values
ln <- nrow(Mkt) ;ln
ln <- 3590
#a <- Mkt$aroonUp[ln] ;a
#d <- Mkt$aroonDn[ln] ;d
#s <- Mkt$oscillator[ln] ;s
#df <- Mkt$Diff[ln] ;df
r_p_ind(Mkt, ln)




# --------- Old Stuff

Mkt <- Mkt[, 1:12]
tail(Mkt)
colnames(Mkt)
Mkt$prev_smadiff <- c( NA, Mkt$Diff[ - length(Mkt$Diff) ] )
Mkt$prev_aroon_up <- c( NA, Mkt$aroonUp[ - length(Mkt$aroonUp) ] )
Mkt$prev_aroon_dn <- c( NA, Mkt$aroonDn[ - length(Mkt$aroonDn) ] )
Mkt$prev_aroon_os <- c( NA, Mkt$oscillator[ - length(Mkt$oscillator) ] )
Mkt$prev_mom <- c( NA, Mkt$mom[ - length(Mkt$mom) ] )
Mkt$pl <- Mkt$Close - Mkt$Open

tail(Mkt)

nr <- 2000
Mkt$Date[nr]
au <- Mkt$prev_aroon_up[nr] ;au
ad <- Mkt$prev_aroon_dn[nr] ;ad
os <- Mkt$prev_aroon_os[nr] ;os
df <- Mkt$prev_smadiff[nr]  ;df
au_df(au,df)
ad_df(ad,df)
os_df(os,df)
Mkt$pl[nr]

r_p(3666)

a <- r_p(2737)
b <- r_p(2738)
c <- rbind(a,b)

ln <- nrow(Mkt)
dd <- run_rp(ln)
tail(dd)
colnames(dd) <- c('a','b','c','d','e','f')
dd2 <- as.data.frame(dd)
sum(dd2[ dd2$e > 0, 6])
sum(dd2[ dd2$e < 0, 6])

( sum(dd2[ dd2$e > 0, 6]) - sum(dd2[ dd2$e < 0, 6]) ) / 14


run_rp <- function(ln){
  #browser()
  a <- c(0,0,0,0,0,0)
  for(i in 31:ln){
    b <- r_p(i)
    a <- rbind(a,b)
  }
  return(a)
}

r_p <- function(nr){
  b <- Mkt$Date[nr]
  au <- Mkt$prev_aroon_up[nr] 
  ad <- Mkt$prev_aroon_dn[nr] 
  os <- Mkt$prev_aroon_os[nr] 
  df <- Mkt$prev_smadiff[nr]  
  c <- au_df(au,df)
  d <- ad_df(ad,df)
  e <- os_df(os,df)
  e2 <- c+d+e
  f <- Mkt$pl[nr]
  
  return(c(b,c,d,e,e2,f))
}


# --------------------------------------------
Mkt <- read.csv("../Data/CAC_ta.csv")
Mkt <- read.csv("../Data/Dax_ta.csv")
Mkt <- read.csv("../Data/Oz_ta.csv")
Mkt <- read.csv("../Data/F100_ta.csv")
Mkt <- AddPrev(Mkt)
tail(Mkt[, 1:5])
tail(Mkt)

ln <- nrow(Mkt)
a <- Mkt$aroonUp[ln] ;a
d <- Mkt$aroonDn[ln] ;d
s <- Mkt$oscillator[ln] ;s
df <- Mkt$Diff[ln] ;df
r_p_ind(a,d,s,df)

r_p(3608)

r_p_ind <- function(au, ad, os, df){
  c <- au_df(au,df)
  d <- ad_df(ad,df)
  e <- os_df(os,df)
  e2 <- c+d+e
  return(c(c,d,e,e2))
}

au_df <- function(au, df){
  sum ( Mkt[ (Mkt$prev_aroon_up == au) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ] ,na.rm=T)
}

ad_df <- function(ad, df){
  sum ( Mkt[ (Mkt$prev_aroon_dn == ad) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ] ,na.rm=T)
}

os_df <- function(os, df){
  sum ( Mkt[ (Mkt$prev_aroon_os == os) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ] ,na.rm=T)
}

# combo aroon_up - diff
sum ( Mkt[ (Mkt$prev_aroon_up == au) & 
             (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
           c(17) ] ,na.rm=T)
nrow(Mkt  [ (Mkt$prev_aroon_up == au) & 
              Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10), ])

# combo aroon_ud - diff
sum ( Mkt[ (Mkt$prev_aroon_dn == ad) & 
             (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
           c(17) ] ,na.rm=T)
nrow(Mkt  [ (Mkt$prev_aroon_dn == ad) & 
              Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10), ])

# combo aroon_os - diff
sum ( Mkt[ (Mkt$prev_aroon_os == os) & 
             (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
           c(17) ] ,na.rm=T)
nrow(Mkt  [ (Mkt$prev_aroon_os == os) & 
              Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10), ])



aa <- ifelse(Mkt$prev_aroon_os == -35, -Mkt$pl, NA)
sum(aa, na.rm=T)
aa
Mkt[Mkt$pl[Mkt$prev_aroon_os == -35]

Mkt[Mkt$prev_aroon_os == 35, c(1,25,27)]

#aroon
sum(  Mkt[Mkt$prev_aroon_os == 35, c(27)] ,na.rm=T ) 
sum(  Mkt[Mkt$prev_aroon_dn < 35, c(27)] ,na.rm=T )
sum(  Mkt[Mkt$prev_aroon_up > 65, c(27)] ,na.rm=T )

#sma
sum(  Mkt[Mkt$prev_smadiff >300, c(27)] ,na.rm=T )
sum(  Mkt[Mkt$prev_smadiff <300, c(27)] ,na.rm=T )

  #mom
sum(  Mkt[Mkt$prev_mom > 100, c(27)] ,na.rm=T )
sum(  Mkt[Mkt$prev_mom < 50, c(27)] ,na.rm=T )

nr <- 2991
ar <- Mkt$prev_aroon_up[nr] ;ar
df <- Mkt$prev_smadiff[nr]  ;df

# combo
sum ( Mkt[ (Mkt$prev_aroon_up == ar) & 
           (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
           c(27) ] ,na.rm=T)

nrow(Mkt  [ (Mkt$prev_aroon_up == ar) & 
              Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10), ])
