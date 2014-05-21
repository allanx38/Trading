library(xtable)
setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")
source("D:/Allan/DropBox/RWorkingDir/Trading/Dax/addTA_fnc.R")

Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dax_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/CAC_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/F100_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dow_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/N225_ta.csv")
Mkt <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Oz_ta.csv")

b <- calcT(Mkt)
sum(b$PL2, na.rm=T)
sum(b$PL2>0,na.rm=T) + sum(b$PL2<0,na.rm=T)
round( sum(b$PL2>0,na.rm=T) / (sum(b$PL2>0,na.rm=T) + sum(b$PL2<0,na.rm=T) ),2) * 100

