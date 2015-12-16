# title         : cv.n.r
# purpose       : cross validation;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : points with overlayed covariates 
# outputs       : compressed RDA files ;
# remarks 1     : Takes ca 2.2 hrs(1.9 for BDRICM, 0.3 for BDRLOG) to run with 5 cpus for randomforest in the defualt setting in use 
# remarks 2     : Computing time depends on the setting of *.flag
# remarks 3     : SAPICM will be identifical if the PC.flag is the same

rm(list = ls(all = TRUE))
## Cross-validation of models:

library(raster)
library(sp)
library(randomForest)
library(snowfall)
library(dismo)
library(GSIF)
#library(scales)
#library(plyr)
#library(hexbin)
#library(gridExtra)
#library(lattice)
#library(grDevices)
#library(plotKML)
# global define
nfold <- 5 ## Set CV fold
sub.N= 60000
cell.size = 0.1
n = 50
#sub.N= 35000
#cell.size = 0.1
#n = 20
a.dir <- "/home/shang009/big/"# dir of the project
w.dir <- paste0(a.dir, "/worldgrids")
m.dir <- paste0(a.dir, "/soildepth")
## Cross-validation of models:
setwd(m.dir)
source(paste0(a.dir, "/soildepth/code/head/functions.r"))
source(paste0(a.dir, "soildepth/code/head/cv_nfold.r"))


#fit.name <- "us" 
##defualt
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
#m.flag <- 1  # 0: with only randomfores; 1: with all models 
#source(paste0(a.dir, "soildepth/code/1km/cross.validation.r"))
##
#####PC.flag test
#PC.flag <- 1 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
#m.flag <- 0  # 0: with only randomfores; 1: with all models 
#source(paste0(a.dir, "soildepth/code/1km/cross.validation.r"))
#
#
##soil.flag test
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
#m.flag <- 0  # 0: with only randomfores; 1: with all models 
#source(paste0(a.dir, "soildepth/code/1km/cross.validation.r"))
#
#
fit.name <- "all" 

####default global # too few for us
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
#m.flag <- 0  # 0: with only randomfores; 1: with all models 
#source(paste0(a.dir, "soildepth/code/1km/cross.validation.r"))
#
#####arti.flag test # too few for us
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 0  #1: add artificial points; 0: not
#soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
#m.flag <- 0  # 0: with only randomfores; 1: with all models 
#source(paste0(a.dir, "soildepth/code/1km/cross.validation.r"))

#### final run  to be chosen ? 
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
#m.flag <- 0  # 0: with only randomfores; 1: with all models 
#source(paste0(a.dir, "soildepth/code/1km/cross.validation.r"))
#

#including cv.lst,tbl,c_m
fit.name <- "us" 
#defualt
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 1  #1: add artificial points; 0: not
soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
m.flag <- 1  # 0: with only randomfores; 1: with all models 
load(paste0("./cv/cv_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".RData"))
#source(paste0(a.dir, "soildepth/code/1km/cv.plot.r"))
cv.lst0 <- cv.lst
tbl0 <- tbl
c_m0 <- c_m 

####PC.flag test
PC.flag <- 1 # 0: not use the PC as predictors; 1: not use
arti.flag <- 1  #1: add artificial points; 0: not
soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
m.flag <- 0  # 0: with only randomfores; 1: with all models 
load(paste0("./cv/cv_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".RData"))
#source(paste0(a.dir, "soildepth/code/1km/cv.plot.r"))
cv.lst1 <- cv.lst
tbl1 <- tbl
c_m1 <- c_m 

#soil.flag test
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 1  #1: add artificial points; 0: not
soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
m.flag <- 0  # 0: with only randomfores; 1: with all models 
load(paste0("./cv/cv_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".RData"))
cv.lst2 <- cv.lst
tbl2 <- tbl
c_m2 <- c_m 


fit.name <- "all" 

####default global 
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 0  #1: add artificial points; 0: not
soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
m.flag <- 0  # 0: with only randomfores; 1: with all models 
load(paste0("./cv/cv_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".RData"))
cv.lst4 <- cv.lst
tbl4 <- tbl
c_m4 <- c_m 


####arti.flag test
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 1  #1: add artificial points; 0: not
soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
m.flag <- 0  # 0: with only randomfores; 1: with all models 
load(paste0("./cv/cv_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".RData"))

cv.lst3 <- cv.lst
tbl3 <- tbl
c_m3 <- c_m 

####final prediction
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 1  #1: add artificial points; 0: not
soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
m.flag <- 0  # 0: with only randomfores; 1: with all models 
load(paste0("./cv/cv_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".RData"))
cv.lst5 <- cv.lst
tbl5 <- tbl
c_m5 <- c_m
source(paste0(a.dir, "soildepth/code/1km/cv.plot.r"))
rm(cv.lst,tbl,c_m)
####compare beween different settings
tblc <- data.frame(ATTRIBUTE_LABEL = c("BDRICM", "SAPICM2"))
tblc$var.test.Hpc <- NA
tblc$t.test.Hpc <- NA
tblc$var.test.Hat <- NA
tblc$t.test.Hat <- NA
tblc$var.test.Hs <- NA
tblc$t.test.Hs <- NA

####compare
for(j in 1:2)
{
    #pc
    try( tblc$var.test.Hpc[j] <- round(var.test((cv.lst1[[j]]$rf.pred)-(cv.lst1[[j]]$meas), (cv.lst0[[j]]$rf.pred)-(cv.lst0[[j]]$meas), alternative="greater")$p.value, 11) )
    try( tblc$t.test.Hpc[j] <- round(t.test(abs(cv.lst1[[j]]$rf.pred-cv.lst1[[j]]$meas), abs(cv.lst0[[j]]$rf.pred-cv.lst0[[j]]$meas),paired=T, alternative="greater")$p.value, 11) )
    #at
    try( tblc$var.test.Hat[j] <- round(var.test((cv.lst4[[j]]$rf.pred)-(cv.lst4[[j]]$meas), (cv.lst3[[j]]$rf.pred)-(cv.lst3[[j]]$meas), alternative="greater")$p.value, 11) )
    try( tblc$t.test.Hat[j] <- round(t.test(abs(cv.lst4[[j]]$rf.pred-cv.lst4[[j]]$meas), abs(cv.lst3[[j]]$rf.pred-cv.lst3[[j]]$meas),paired=T, alternative="greater")$p.value, 11) )
    #soil
    try( tblc$var.test.Hs[j] <- round(var.test((cv.lst2[[j]]$rf.pred)-(cv.lst2[[j]]$meas), (cv.lst0[[j]]$rf.pred)-(cv.lst0[[j]]$meas), alternative="greater")$p.value, 11) )
    try( tblc$t.test.Hs[j] <- round(t.test(abs(cv.lst2[[j]]$rf.pred-cv.lst2[[j]]$meas), abs(cv.lst0[[j]]$rf.pred-cv.lst0[[j]]$meas),paired=T, alternative="greater")$p.value, 11) )
}

save(tbl0,tbl1,tbl2,tbl3, tbl4,tblc, c_m0,c_m1,c_m2,c_m3,c_m4, file = paste0("./cv/tbc.RData"))


############see the model OOB results
fit.name <- "all" 
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 1  #1: add artificial points; 0: not
soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
tvar <- "BDRICM"
#tvar <- "SAPICM2"
r2_0 <- NULL
mse_0 <- NULL
for(i in 1:nfold)
{
    load(paste0("./cv/", tvar, "m.rf_", i, "_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".RData"))
    r2_0 <- round(c(r2_0, m.rf$rsq[m.rf$ntree]), 3)
    mse_0 <- round(c(mse_0, m.rf$mse[m.rf$ntree]), 3)
}
sort(r2_0)
sort(mse_0)
tvar <- "BDRLOG"
err.rate_0 <- NULL
confusion_0 <- NULL
for(i in 1:nfold)
{
    load(paste0("./cv/", tvar, "m.rf_", i, "_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".RData"))
    err.rate_0 <- c(err.rate_0, round(m.rf$err.rate[m.rf$ntree, "OOB"]*100, digits=2))
    confusion_0 <- m.rf$confusion
}


# tbl0
#  ATTRIBUTE_LABEL PR_DEPTHS N_OBSERVATIONS OBSERVED_RANGE_MIN OBSERVED_RANGE_MAX  rf.ME rf.RMSE
#1          BDRICM         1        1216246                  0             383000 101.14    1710
#2          SAPICM         1             NA                 NA                 NA     NA      NA
#
# tbl1
#  ATTRIBUTE_LABEL PR_DEPTHS N_OBSERVATIONS OBSERVED_RANGE_MIN OBSERVED_RANGE_MAX   rf.ME rf.RMSE var.test.H1 t.test.H1 rf1.SIGMA_EXPLAINED
#1          BDRICM         1        1216246                  0             383000 163.973    1840           0         0                23.7
#2          SAPICM         1             NA                 NA                 NA      NA      NA          NA        NA                  NA
#  rf0.SIGMA_EXPLAINED
#1                33.2
#2                  NA
# % Var explained, the nfold is exactly the same,
#subsets are corressponding to the order of r2:
#sub1,  sub2, sub3, sub4, sub5
#sub.N= 60000
#cell.size = 0.1
#n = 50
#PC.flag = 0:
# 0.480 0.483 0.519 0.527 0.537
#PC.flag = 1:
#PC.flag = 1, % Var explained:
# 0.419 0.421 0.429 0.457 0.472

#sub.N= 35000
#cell.size = 0.1
#n = 20
#PC.flag = 0:
#40.57 42.74 43.52 49.44 56.43
#PC.flag = 1:
#PC.flag = 1, % Var explained:
#33.71 35.46 40.05 41.50 45.14
