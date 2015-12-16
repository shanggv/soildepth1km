# title         : prediction.r
# purpose       : predict the soil depth;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : WorldGrids layers, prediction models
# outputs       :  pedicted tif files;
# remarks 1     : Takes ca 3 hour for global with 6 cpus on donkey, 10 hour for en.num == 1:10;

rm(list = ls(all = TRUE))
library(GSIF)
library(plotKML)
library(sp)
library(randomForest)
library(snowfall)
library(gdalUtils)
library(RSAGA)
library(rgdal)
library(randomForest)
library(randomForestSRC)

# global define
#gdal.dir <- "/home/src"
#gdal_setInstallation(search_path=gdal.dir, rescan=TRUE)
a.dir <- "/home/shang009/big"# dir of the project
w.dir <- paste0(a.dir, "/worldgrids")
m.dir <- paste0(a.dir, "/soildepth")
source(paste0(a.dir, "/soildepth/code/head/functions.r"))
setwd(m.dir)

kml.flag <- 1 # 0:not produce kml, 1: produce kml for predition and covariates
tilestest <- c( "T212","T213", "T460", "T461", "T524", "T560","T441","T442", "T477", "T478")
#T478,477, NO BIG DIFF
#T441,442, MUCH SHALLOW WITH SOIL


#defualt
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 1  #1: add artificial points; 0: not
fit.name <- "us" # c("eu", "as", "us", "ca", "all")
soil.flag <-0 # 0: without soil profiles; 1: add soil profiles
en.num <- 1   #number of ensemble prediction
tilestest <- "all"
kml.flag <- 0
source(paste0(a.dir, "/soildepth/code/1km/prediction.r"))
######PC.flag test
#PC.flag <- 1
#source(paste0(a.dir, "/soildepth/code/1km/prediction.r"))
#
#defualt2
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#fit.name <- "all" # c("eu", "as", "us", "ca", "all")
#soil.flag <-0 # 0: without soil profiles; 1: add soil profiles
#tilestest <- "all"
#kml.flag <- 0
#en.num <- 1   #number of ensemble prediction
#source(paste0(a.dir, "/soildepth/code/1km/prediction.r"))
#en.num <- 1:10   #number of ensemble prediction
#source(paste0(a.dir, "/soildepth/code/1km/prediction.r"))

#####defualt 3
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#fit.name <- "all" # c("eu", "as", "us", "ca", "all")
#soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
#en.num <- 1
#source(paste0(a.dir, "/soildepth/code/1km/prediction.r"))


######arti.flag test
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 0  #1: add artificial points; 0: not
#fit.name <- "all" # c("eu", "as", "us", "ca", "all")
#soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
#en.num <- 1   #number of ensemble prediction
#source(paste0(a.dir, "/soildepth/code/1km/prediction.r"))

####soil.flag test
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#fit.name <- "us" # c("eu", "as", "us", "ca", "all")
#soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
#en.num <- 1   #number of ensemble prediction
#source(paste0(a.dir, "/soildepth/code/1km/prediction.r"))


#####predict all
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#fit.name <- "all" # c("eu", "as", "us", "ca", "all")
#soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
#kml.flag <- 0 # 0:not produce kml, 1: produce kml for predition and covariates
#en.num <- 1 #number of ensemble prediction
#tilestest <- "all"
##source(paste0(a.dir, "/soildepth/code/1km/prediction.r"))
#en.num <- 1:10   #number of ensemble prediction
#source(paste0(a.dir, "/soildepth/code/1km/prediction.r"))



# get tiles for regions
#grd <- gdalinfo(paste0(w.dir,"/ELFUSG3a.tif"),raw_output=FALSE)
#tiles <- getSpatialTiles(grd, block.x=10, block.y=10)
# tiles of US
#as.integer(rownames(tiles[tiles$xl > -120 & tiles$xl < -70 & tiles$yl > 30 & tiles$yl < 50, ]))
#tilestest <- c(439, 440, 441, 442, 443,  475, 476, 477, 478, 479)
#tiles of China
#as.integer(rownames(tiles[tiles$xl > 80 & tiles$xl < 120 & tiles$yl > 20 & tiles$yl < 40, ]))
#tilestest <- c(423, 424, 425, 426, 459, 460, 461, 462)
#tiles of Sweden
#as.integer(rownames(tiles[tiles$xl > 10 & tiles$xl < 20 & tiles$yl > 50 & tiles$yl < 70, ]))
#tilestest <- c(524, 560)
#tiles of Australia
#as.integer(rownames(tiles[tiles$xl > 100 & tiles$xl <160  & tiles$yl > -40 & tiles$yl < -10, ]))
#tilestest <- c(210, 211, 212, 213, 214,  246, 247, 248, 249, 250, 281, 282, 283, 284, 285, 286)


#run only one time
#pntkml <- "./pntkml"
#load("./profs/wells.depth.rda")
#psub <- wells.depth
##writeOGR(psub, ".", "well_T476_477", driver="ESRI Shapefile")
#psub <- subset(psub, psub$LONWGS84 > -110 & psub$LONWGS84 < -90 & psub$LATWGS84 > 40 & psub$LATWGS84 < 50)
#psub <- splitsp(psub, cellsize = 0.1, n = 1, sn = length(1))$sub[[1]]
#kml(psub, colour=log1p(BDRICM), raster_name=paste0("well_T476_477.png"),
#    z.lim = c(3,10),
#    colour_scale = SAGA_pal[[1]],
#    folder.name =  paste0("well_T476_477.kml" ),
#    file.name   = paste0("well_T476_477.kml"))
#
#    flist <- list.files(pattern = "_T")
#    file.copy(flist, paste0(pntkml))
#    file.remove(flist)
#    flist <- list.files(pntkml)
#    system(paste0("tar -zcvf ", pntkml, "/wellT476_477.tar.gz ", pntkml))
#rm(psub,wells.depth)
