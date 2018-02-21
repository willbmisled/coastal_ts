#https://github.com/USEPA/rethinking_tsi
################################################################################

# 1. Setup

################################################################################
# 
# pkgs <- c("devtools", "randomForest", "dplyr", "rjags", "arm", "xtable","caret",
#           
#           "ggplot2","knitr")
# 
# cran_no <- pkgs[!pkgs %in% installed.packages()[,1]]
# 
# for(i in cran_no){
#         
#         install.packages(i)
#         
# }
# 
# gh_pkgs <- c("usepa/LakeTrophicModelling")
# 
# gh_no <- gh_pkgs[!basename(gh_pkgs) %in% installed.packages()[,1]]
# 
# for(i in gh_no){
#         
#         devtools::install_github(i)
#         
# }
# 
# all_pkgs <- c(basename(gh_pkgs),pkgs)
# 
# lapply(all_pkgs, library, character.only = T)

library(devtools)
library(randomForest)
library(dplyr)
library(rjags)
library(arm)
library(xtable)
library(caret)




################################################################################

# 2. Data

################################################################################



#data(LakeTrophicModelling)



##################################################
# 
# #All Variables
# 
# #Clean Up Data - Complete Cases
# 
# predictors_all <- predictors_all[predictors_all!="DATE_COL"]
# 
# all_dat <- data.frame(ltmData[predictors_all],LogCHLA=log10(ltmData$CHLA))
# 
# row.names(all_dat)<-ltmData$NLA_ID
# 
# all_dat <- all_dat[complete.cases(all_dat),]  
# 
# 
# 
# ##################################################
# 
# #GIS Variables
# 
# #Clean Up Data - Complete Cases
# 
# 
# 
# 
# 
# 
# gis_dat_NTL <- data.frame(ltmData[predictors_gis],LogNTL=log10(ltmData$NTL))
# 
# gis_dat_PTL <- data.frame(ltmData[predictors_gis],LogPTL=log10(ltmData$PTL))
# 
# 
# 
# # row.names(gis_dat)<-ltmData$NLA_ID
# 
# row.names(gis_dat_NTL)<-ltmData$NLA_ID
# 
# row.names(gis_dat_PTL)<-ltmData$NLA_ID
# 
# 
# 
# # gis_dat <- gis_dat[complete.cases(gis_dat),]
# 
# gis_dat_NTL <- gis_dat_NTL[complete.cases(gis_dat_NTL),]
# 
# gis_dat_PTL <- gis_dat_PTL[complete.cases(gis_dat_PTL),]
setwd("F:/Coastal_TSI")
coastalData<-read.csv(file="CoastalWQ_20170928.csv",header=TRUE)

coastalData<-rename(coastalData, chla=CHLA..ug.L.)
coastalData<-rename(coastalData, TN=TN..mgN.L.)
coastalData<-rename(coastalData, DIN=DIN..mgN.L.)
coastalData<-rename(coastalData, TP=TP..mgP.L.)
coastalData<-rename(coastalData, DIP=DIP..mgP.L.)
coastalData<-rename(coastalData, DIN_DIP=DIN.DIP..Molar.)
coastalData<-rename(coastalData, TN_TP=TN.TP..Molar.)
coastalData<-rename(coastalData, secchi=SECCHI_MEAN..m.)
coastalData<-rename(coastalData, TSS=TSS..mg.L.)

predVar<-c(7,8,9,10,11,14,15,16,17,18,20)


##2010 only model
coast2010<-filter(coastalData,SAMPYEAR==2010)
coast2010<-filter(coast2010,REGION != "Insular", REGION != "Great Lakes")
coast2010<-filter(coast2010,Col_loc == "SURFACE")
coast2010<-filter(coast2010,VISNUM == 1)


x<-complete.cases(coast2010[,13])
coast2010.cc<-coast2010[x,]

x<-complete.cases(coast2010.cc[,predVar])
coast2010.cc<-coast2010.cc[x,]


coast2010.rf<-randomForest(coast2010.cc[,predVar],log1p(coast2010.cc[,13]),ntree=10000,
                           importance=TRUE,proximity=TRUE,na.action=na.omit)

varImpPlot(coast2010.rf,type = 1)






