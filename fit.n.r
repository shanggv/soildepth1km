# title         : fit.n.R
# purpose       : Fit models for Global Soil depth;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : WorldGrids layers, SoilProfiles.org points, water well points from various sources
# outputs       : prediction models ;
# remarks 1     : Takes ca several hours(3?) for 1 run, depending on the setting of *.flag;

rm(list = ls(all = TRUE))
try(pkgs <- names(sessionInfo()$otherPkgs))
try(pkgs <- paste('package:', pkgs, sep = ""))
try(lapply(pkgs, detach, character.only = TRUE, unload = TRUE))
library(sp)
library(randomForest)
library(randomForestSRC)
library(raster)
library(snowfall)

# global define
#gdal.dir <-  "/usr/local/bin"
#gdal_setInstallation(search_path=gdal.dir, rescan=TRUE)
# directory
a.dir <- "/data/shang009/big"# dir of the project
w.dir <- paste0(a.dir, "/worldgrids")
m.dir <- paste0(a.dir, "/soildepth")
refit <- FALSE
source(paste0(a.dir, "/soildepth/code/head/functions.r"))
setwd(m.dir) 
options(rf.cores=10, mc.cores=10)


#SAPICM2: only one time
#PC.flag<-0
#for(PC.flag in 0:1)
#{
#    load(paste0(a.dir, "/worldgrids/worldgrids.spc.rda"))
#    if(PC.flag == 1)
#    {   
#        pr.lst <- paste0("PC", 1:(length(worldgrids.spc$center)-2))
#    }else
#    {    
#        pr.lst <- dimnames(worldgrids.spc$rotation)[[1]]
#     }
#    load(paste0("./profs/subs/soil.sp_", PC.flag,".rda"))
#    soil.sp <- subset(soil.sp, !is.na(soil.sp$SAPICM2))
#    # test run
#    #soil.sp <- subset(soil.sp, runif(length(soil.sp))<0.01)
#    #mtry <- tuneRF(x = soil.sp[pr.lst]@data, y = log1p(soil.sp$SAPICM2), stepFactor=1.2)
#    #mtry <- mtry[order(mtry[,2])[1],1]
#    #m_SAPICM2 <- randomForest(x = soil.sp[pr.lst]@data, y = log1p(soil.sp$SAPICM2), na.action = na.omit, ntree = 300, importance = TRUE)
#    formulaString.SAPICM2 <- as.formula(paste0("log1p(SAPICM2) ~ ",  paste(pr.lst, collapse="+")))
#    vs_SAPICM2 <- var.select(formulaString.SAPICM2 , soil.sp@data, ntree =300, seed = -2212)
#    formulaString.SAPICM2 <- as.formula(paste0("log1p(SAPICM2) ~ ",  paste(vs_SAPICM2$topvars, collapse="+")))
#    m_SAPICM2 <-  randomForest(formulaString.SAPICM2, soil.sp@data, ntree =300) 
#    save(vs_SAPICM2, m_SAPICM2, file= paste0("./model/m_SAPICM2_p",PC.flag, ".rda")) 
#}



#defualt
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#fit.name <- "us" # c("eu", "as", "us", "ca", "all")
#soil.flag <-0 # 0: without soil profiles; 1: add soil profiles
#en.num <- 1   #number of ensemble prediction
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.r"))
#
#####PC.flag test
#PC.flag <- 1
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.r"))

#default 2
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#fit.name <- "all" # c("eu", "as", "us", "ca", "all")
#soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
#en.num <- 1   #number of ensemble prediction
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.r"))


####predict  with this ??
#refit <- TRUE #only one time
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#fit.name <- "all" # c("eu", "as", "us", "ca", "all")
#soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
#en.num <- 1
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.r"))
#refit <- FALSE #only one time
####en.num test
#for( en.num in 2:10)   #number of ensemble prediction
#{
#    source(paste0(a.dir, "/soildepth/code/1km/fit_models.r"))
#}


####soil.flag test
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#fit.name <- "us" # c("eu", "as", "us", "ca", "all")
#soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
#en.num <- 1   #number of ensemble prediction
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.r"))
#
#######arti.flag test
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 0  #1: add artificial points; 0: not
#fit.name <- "all" # c("eu", "as", "us", "ca", "all")
#soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
#en.num <- 1   #number of ensemble prediction
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.r"))









