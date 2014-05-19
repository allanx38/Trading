
Mkt = read.csv("../Data/F100_ta.csv")
Mkt = read.csv("../Data/Dax_ta.csv")
Mkt = read.csv("../Data/N225_ta.csv")

Mkt$prev_smadiff <- c( NA, Mkt$Diff[ - length(Mkt$Diff) ] )
Mkt$prev_aroon_up <- c( NA, Mkt$aroonUp[ - length(Mkt$aroonUp) ] )
Mkt$prev_aroon_dn <- c( NA, Mkt$aroonDn[ - length(Mkt$aroonDn) ] )
Mkt$prev_aroon_os <- c( NA, Mkt$oscillator[ - length(Mkt$oscillator) ] )
Mkt$prev_mom <- c( NA, Mkt$mom[ - length(Mkt$mom) ] )
Mkt$pl <- Mkt$Close - Mkt$Open
Mkt$psm <- 0

CalcPL <- function(df, ar, mm){
  aa <-sum ( Mkt[ (Mkt$prev_aroon_up == ar) & 
                    (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)) &
                    Mkt$mom > (mm - 10) & Mkt$mom < (mm + 10), 
                  c(27) ] ,na.rm=T)
  return(aa)
}

rr <- function(Mkt){
  for(i in 1:nrow(Mkt)){
    ar <- Mkt$prev_aroon_up[i]
    df <- Mkt$prev_smadiff[i]
    mm <- Mkt$prev_mom[i]
    if(is.na(ar)){
      Mkt$psm[i] <- NA
    } else {
      aa <- CalcPL(df, ar)
      Mkt$psm[i] <- aa
    }
  }
  return(Mkt)
}

Mkt <- rr(Mkt)

head(Mkt)
tail(Mkt)

Mkt$tr_pl <- ifelse(Mkt$psm > 0 , Mkt$pl, -Mkt$pl)
sum(Mkt$tr_pl, na.rm=T)

cc <- ifelse(Mkt$tr_pl > 0 , Mkt$tr_pl, NA)
dd <- ifelse(Mkt$tr_pl < 0 , Mkt$tr_pl, NA)
cc[!is.na(cc)]
dd[!is.na(dd)]

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

tail(Mkt)

Mkt$Date[3636]
nr <- 3636
os <- Mkt$prev_aroon_os[nr] ;ar
au <- Mkt$prev_aroon_up[nr] ;au
ad <- Mkt$prev_aroon_dn[nr] ;ad
df <- Mkt$prev_smadiff[nr]  ;df
mm <- Mkt$prev_mom[nr] ;mm
# combo
sum ( Mkt[ (Mkt$prev_aroon_up == ar) & 
           (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)) & 
            Mkt$mom > (mm - 10) & Mkt$mom < (mm + 10), 
           c(27) ] ,na.rm=T)

sum ( Mkt[ (Mkt$prev_aroon_os == os) & 
             (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
           c(27) ] ,na.rm=T)

sum ( Mkt[ (Mkt$prev_aroon_up == au) &  
             Mkt$mom > (mm - 10) & Mkt$mom < (mm + 10), 
           c(27) ] ,na.rm=T)

sum ( Mkt[ (Mkt$prev_aroon_dn == ad) &  
             Mkt$mom > (mm - 10) & Mkt$mom < (mm + 10), 
           c(27) ] ,na.rm=T)


Mkt$psm <- 0

CalcPL <- function(df, ar){
  aa <-sum ( Mkt[ (Mkt$prev_aroon_up == ar) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)) , 
             c(27) ] ,na.rm=T)
  #browser()
  return(aa)
}

rr <- function(Mkt){
  for(i in 1:nrow(Mkt)){
    ar <- Mkt$prev_aroon_up[i]
    df <- Mkt$prev_smadiff[i]
    if(is.na(ar)){
      #browser()
      Mkt$psm[i] <- NA
    } else {
      #browser()
      aa <- CalcPL(df, ar)
      Mkt$psm[i] <- aa
    }
  }
  return(Mkt)
}

rr()

head(Mkt)

Mkt$psm <- ifelse(!is.na(Mkt$aroonUp), CalcPL(Mkt$Diff, Mkt$aroonUp),NA)
tail(Mkt)

nrow(Mkt  [ (Mkt$prev_aroon_up == ar) & 
              Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10), ])
