################################################################################
# 1. Setup
################################################################################
pkgs <- c("devtools", "randomForest", "dplyr", "rjags", "arm", "xtable","caret",
          "ggplot2","knitr")
cran_no <- pkgs[!pkgs %in% installed.packages()[,1]]
for(i in cran_no){
  install.packages(i)
}
gh_pkgs <- c("usepa/LakeTrophicModelling")
gh_no <- gh_pkgs[!basename(gh_pkgs) %in% installed.packages()[,1]]
for(i in gh_no){
  devtools::install_github(i)
}
all_pkgs <- c(basename(gh_pkgs),pkgs)
lapply(all_pkgs, library, character.only = T)

library(magrittr)
library(tidyverse)
library(randomForest)
library(caret)

################################################################################
# 2. Data
################################################################################

coastal <- read.csv("CoastalWQ_20161129.csv", stringsAsFactors = FALSE)
# VISNUM = 1
coastal <- coastal[coastal[,"VISNUM"]==1,]
# Col_loc = "SURFACE" 
coastal <- coastal[coastal[,"Col_loc"]=="SURFACE",]
# SAMPYEAR=2010
coastal <- coastal[coastal[,"SAMPYEAR"]=="2010",]

coastal[,"REGION"] <- as.factor(coastal[,"REGION"])
coastal[,"SUBREGIONS"] <- as.factor(coastal[,"SUBREGIONS"])
#
predictors_coastal <- colnames(coastal)

predictors_coastal <- predictors_coastal[predictors_coastal!="Col_Date" &
                                           predictors_coastal!="VISNUM" &
                                           predictors_coastal!="Col_loc"&
                                           predictors_coastal!="Site_Visnum_Layer"&
                                           predictors_coastal!="UID"&
                                           predictors_coastal!="SITE_ID"&
                                           predictors_coastal!="STATE"&
                                           predictors_coastal!="SAMPYEAR"&
                                           predictors_coastal!="TSS..mg.L."&
                                           predictors_coastal!="CHLA..ug.L."]


coastal <- coastal %>% mutate(TS=cut(CHLA..ug.L., breaks=c(-Inf, 5, 20, 60 , Inf), labels=c("Oligo", "Meso", "Eu","Hyper")))
##################################################
#All Variables
#Clean Up Data - Complete Cases
all_coastal <- data.frame(coastal[predictors_coastal],LogCHLA=log10(coastal$CHLA..ug.L.))
row.names(all_coastal)<-coastal$SITE_ID
all_coastal <- all_coastal[complete.cases(all_coastal),]

################################################################################
# 3. Random Forest for Variable Selection
################################################################################

##################################################
#Model: All Variables
all_rf<-randomForest(y=all_coastal$LogCHLA,x=all_coastal[,predictors_coastal]
                     , ntree=5000, importance=TRUE, proximity=TRUE
                     , keep.forest=TRUE,keep.inbag=TRUE)
################################################################################
# 4. Random Forest for Variable Selection - Evaluation
################################################################################
# 
data_def <- read.csv("data_def.csv", stringsAsFactors = FALSE)
all_imp <- importance(all_rf)

var_importance <- varImportance(all_imp,"All variables")

dplyr::arrange(var_importance,desc(mean_decrease_acc))

importancePlot(all_rf, data_def=data_def,type='acc',size=3)

dplyr::arrange(var_importance,desc(mean_decrease_gini))

importancePlot(all_rf, data_def=data_def,type='gini',size=3)
################################################################################
# 7. JAGS Model
################################################################################
# Removing the missing values:
coastal<- coastal[!is.na(coastal[,"SECCHI_MEAN..m."]) 
                  & !is.na(coastal[,"DIP..mgP.L."]) 
                  & !is.na(coastal[,"DIN..mgN.L."])
                  & !is.na(coastal[,"TN..mgN.L."])
                  & !is.na(coastal[,"TP..mgP.L."])
                  & !is.na(coastal[,"SUBREGIONS"])
                  & !is.na(coastal[,"CHLA..ug.L."]),]

# Replacing the non-detects with MDL
coastal[coastal[,"TP..mgP.L."]==0,"TP..mgP.L."] <- 0.0012
coastal[coastal[,"DIN..mgN.L."]==0,"DIN..mgN.L."] <- 0.001
coastal[coastal[,"DIP..mgP.L."]==0,"DIP..mgP.L."] <- 0.0027

# Data splitting for cross validation 90%/10%
set.seed(100)
Sample <- sample(nrow(coastal),size= round(0.1*dim(coastal)[1]),replace=FALSE)

Evaluation <- coastal[Sample,]
Model <- coastal[-Sample,]

#set up the initializations 
cutpt.inits <- array(dim= c(3))

for (k in 1:3){
  cutpt.inits[k] <- rnorm(1)
}

inits <- function () {list("cutpt_raw" = cutpt.inits)}
# Center, scale, and log transform the predictors
SDD.C <- as.numeric(scale(log(Model$SECCHI_MEAN..m.), center = TRUE, scale = TRUE))
TN.C <- as.numeric(scale(log(Model$TN..mgN.L.), center = TRUE, scale = TRUE))
TP.C <- as.numeric(scale(log(Model$TP..mgP.L.), center = TRUE, scale = TRUE))
DIN.C <- as.numeric(scale(log(Model$DIN..mgN.L.), center = TRUE, scale = TRUE))
DIP.C <- as.numeric(scale(log(Model$DIP..mgP.L.), center = TRUE, scale = TRUE))

DataList = list('TS' = factor(Model[,"TS"])
                ,'SD' = SDD.C
                ,'Nitrogen' = TN.C
                ,'Phosphorus' = TP.C
                ,'DIN' = DIN.C
                ,'DIP' = DIP.C
                ,'Subregion' = factor(Model[,"SUBREGIONS"]))


#The parameter(s) to be monitored
parameters = c('alpha_SD', 'alpha_N', 'alpha_P', 'alpha_DIN', 'alpha_DIP', 'alpha_SubR'
               , 's'
               , 'C')

# Number of steps to "tune" the samplers.
adaptSteps = 3000          

# Number of steps to "burn-in" the samplers.
#changes from 1000 the initail setting
burnInSteps = 5000    

# Number of chains to run.       
nChains = 3

# Total number of steps in chains to save.     
numSavedSteps=50000    

# Number of steps to "thin" (1=keep every step).
thinSteps= 5

# Steps per chain.
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )

# Start the clock!
ptm <- proc.time()
JAGS.TSLogit <- jags.model('coastal_jags.R',data = DataList 
                           , inits, n.chains = nChains, n.adapt = adaptSteps)
# Stop the clock
proc.time() - ptm
################################################################################
################################################################################
#8. JAGS Model Diagnostics
################################################################################
# Start the clock!
ptm <- proc.time()
Coda.Coastal <- coda.samples(JAGS.TSLogit, parameters, n.iter=50000)
# Stop the clock
proc.time() - ptm
#################################################