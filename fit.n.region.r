# title         : fit.n.region.R
# purpose       : Fit models for regional Soil depth;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : WorldGrids layers, SoilProfiles.org points, water well points from various sources
# outputs       : prediction models ;
# remarks 1     : Takes ca several hours(3?) for 1 run, depending on the setting of *.flag;

rm(list = ls(all = TRUE))
library(sp)
library(randomForest)
library(randomForestSRC)
library(raster)
library(snowfall)
library(GSIF)
library(stringr)

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
####setting for cross validation by regions
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 0  #1: add artificial points; 0: not
soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
en.num <- 1   #number of ensemble prediction
us.code <- c("us1", paste0("us", 3:17))
ca.code <- c("ca1", paste0("ca", 3:5))


celln <- 3
n.name <- ""
####continent
#fit.name <- "eu" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "as" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "na" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#
####country
#fit.name <- "us" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "ca" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "se" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "ie" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))

###states
#for(fit.name in us.code)
#{ 
#    source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#}
#for(fit.name in ca.code)
#{ 
#    source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#}


celln <- 9
n.name <- 9
####continent
#fit.name <- "eu" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "as" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#
#
####country
#fit.name <- "ca" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "se" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "ie" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))

###states
#for(fit.name in us.code)
#{ 
#    source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#}
#for(fit.name in ca.code)
#{ 
#    source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#}



#celln <- 18
#n.name <- 18
#####continent
#fit.name <- "eu" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "as" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#
#
####country
#fit.name <- "se" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "ie" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#
###states
#for(fit.name in us.code)
#{ 
#    source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#}
#for(fit.name in ca.code)
#{ 
#    source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#}


#celln <- 27
#n.name <- 27
#####continent
#fit.name <- "eu" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "as" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#
####country
#fit.name <- "se" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "ie" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#
###states
#for(fit.name in us.code)
#{ 
#    source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#}
#for(fit.name in ca.code)
#{ 
#    source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#}

####get ca3 and us17 out
#fit.name <- "usm" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
#fit.name <- "cam" 
#source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))

celln <- 3
n.name <- "m3"

###leave one state out
mus.code <- c("mus1", paste0("mus", 3:17))
for(fit.name in mus.code)
{ 
    source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
}
mca.code <- c("mca1", paste0("mca", 3:5))
for(fit.name in mca.code)
{ 
    source(paste0(a.dir, "/soildepth/code/1km/fit_models.region.r"))
}

