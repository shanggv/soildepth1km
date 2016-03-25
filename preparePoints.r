# title         : rw_NCSS.R
# purpose       : prepare soil profiles and water wells for Depth to bedrock / depth to C horizon / depth to saprolite (>80% rocks);
# reference     : 
# producer      : Prepared by W. Shangguan
# last update   : In Wageningen, NL, Sep 2015.
# inputs        : sprofs.depth.rda; wells_us.txt;wells_eu.txt;wells_ca.txt;wells_as.txt
# outputs       : SpatialPoint data frames;
# remarks 1     : LARGE dataset!

rm(list = ls(all = TRUE))
library(aqp)
library(GSIF)
library(plotKML)
library(sp)
library(plyr)
library(raster)
library(lattice)
library(maptools) 
#global
dir <- "E:/data/soildata/depth/code"
setwd(dir)
source("./head/functions.r")

##############################################
## WELL DATA (DRILLINGS)
##############################################
## Wells data compiled by Wei:
us <- read.csv("../points/profs/well/wells_us.txt", sep="\t")
us$BDRICM <- us$D_BR*100
names(us)[2:3] <- c("LONWGS84", "LATWGS84")
us$SOURCEID <- paste0("USWELL_", us$Source, "_", rownames(us))
#get rid of arizona
us <- us[as.character(us$SOURCEID )< "USWELL_2_1280"  | as.character(us$SOURCEID) > "USWELL_2_1808" , ]
us <- us[!is.na(us$BDRICM)&!is.na(us$LONWGS84)&!is.na(us$LATWGS84), ]
EDA(log1p(us$BDRICM))
dev.copy(png,"../points/pic/EDA/EDA_well_us.png", width = 730, height = 480, units = "px")
dev.off()

ca <- read.csv("../points/profs/well/wells_ca.txt", sep="\t")
ca$BDRICM <- ca$D_BR*100
names(ca)[2:3] <- c("LONWGS84", "LATWGS84")
ca$SOURCEID <- paste0("CAWELL_", ca$Source, "_", rownames(ca))
ca <- ca[!is.na(ca$BDRICM)&!is.na(ca$LONWGS84)&!is.na(ca$LATWGS84), ]
EDA(log1p(ca$BDRICM))
dev.copy(png,"../points/pic/EDA/EDA_well_ca.png", width = 730, height = 480, units = "px")
dev.off()

as <- read.csv("../points/profs/well/wells_as2.txt", sep="\t")
as$BDRICM <- as$D_BR*100
names(as)[2:3] <- c("LONWGS84", "LATWGS84")
as$SOURCEID <- paste0("ASWELL_", rownames(as))
as <- as[!is.na(as$BDRICM)&!is.na(as$LONWGS84)&!is.na(as$LATWGS84), ]
EDA(log1p(as$BDRICM))
dev.copy(png,"../points/pic/EDA/EDA_well_as.png", width = 730, height = 480, units = "px")
dev.off()

eu <- read.csv("../points/profs/well/wells_eu.txt", sep="\t")
eu$BDRICM <- eu$D_BR*100
names(eu)[2:3] <- c("LONWGS84", "LATWGS84")
eu$SOURCEID <- paste0("EUWELL_", rownames(eu))
eu <- eu[!is.na(eu$BDRICM)&!is.na(eu$LONWGS84)&!is.na(eu$LATWGS84), ]
EDA(log1p(eu$BDRICM))
dev.copy(png,"../points/pic/EDA/EDA_well_eu.png", width = 730, height = 480, units = "px")
dev.off()


br <- read.csv("../points/profs/well/wells_br.txt", sep="\t")
br$BDRICM <- br$D_BR*100
names(br)[2:3] <- c("LONWGS84", "LATWGS84")
br$SOURCEID <- paste0("BRWELL_", rownames(br))
br <- br[!is.na(br$BDRICM)&!is.na(br$LONWGS84)&!is.na(br$LATWGS84), ]
EDA(log1p(br$BDRICM))
dev.copy(png,"../points/pic/EDA/EDA_well_br.png", width = 730, height = 480, units = "px")
dev.off()

pr <- read.csv("../points/profs/well/wells_pr.txt", sep="\t")
pr$BDRICM <- pr$D_BR*100
names(pr)[2:3] <- c("LONWGS84", "LATWGS84")
pr$SOURCEID <- paste0("PRWELL_", rownames(pr))
pr <- pr[!is.na(pr$BDRICM)&!is.na(pr$LONWGS84)&!is.na(pr$LATWGS84), ]
EDA(log1p(pr$BDRICM))
dev.copy(png,"../points/pic/EDA/EDA_well_pr.png", width = 730, height = 480, units = "px")
dev.off()


cn <- read.csv("../points/profs/well/wells_cn.txt", sep="\t")
cn$BDRICM <- cn$D_BR*100
names(cn)[2:3] <- c("LONWGS84", "LATWGS84")
cn$SOURCEID <- paste0("CNWELL_", rownames(cn))
cn <- cn[!is.na(cn$BDRICM)&!is.na(cn$LONWGS84)&!is.na(cn$LATWGS84), ]
EDA(log1p(cn$BDRICM))
dev.copy(png,"../points/pic/EDA/EDA_well_cn.png", width = 730, height = 480, units = "px")
dev.off()

####combin wells
wells <- do.call(rbind, list( 
        ca[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")], 
        us[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],
        as[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],
        eu[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],
        br[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],
        pr[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],
        cn[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")]))
rm(ca,us,as,eu,br,pr,cn)
wells <- wells[complete.cases(wells), ]
EDA(log1p(wells$BDRICM))
dev.copy(png,"../pic/EDA/EDA_well_all.png", width = 730, height = 480, units = "px")
dev.off()

wells.depth <- wells
coordinates(wells.depth) <- ~ LONWGS84+LATWGS84
proj4string(wells.depth) <- CRS("+proj=longlat +datum=WGS84")     
#writePointsShape(wells.depth, "as")
wells.depth$BDRICM <- as.integer(wells.depth$BDRICM)
save(wells.depth, file="../points/profs/wells.depth.rda")
#load("../points/profs/wells.depth.rda")

##############################################
## SOIL PROFILE DATA
##############################################
###Soil profiles are prepared in the fold of profiles for SoilGrids1km
load("../points/profs/sprofs.depth.rda")
tmp <- sprofs.depth$BDRICM[!is.na(sprofs.depth$BDRICM)]
EDA(log1p(tmp))
dev.copy(png,"../points/pic/EDA/EDA_soil_BDR.png", width = 730, height = 480, units = "px")
dev.off()

tmp <- sprofs.depth$SAPICM[!is.na(sprofs.depth$SAPICM)]
EDA(log1p(tmp))
dev.copy(png,"../points/pic/EDA/EDA_soil_SAP.png", width = 730, height = 480, units = "px")
dev.off()

tmp <- sprofs.depth$SAPICM2[!is.na(sprofs.depth$SAPICM2)]
EDA(log1p(tmp))
dev.copy(png,"../points/pic/EDA/EDA_soil_SAP2.png", width = 730, height = 480, units = "px")
dev.off()



kml(tmp, colour=log1p(BDRICM), colour_scale=SAGA_pal[[1]],z.lim = c(0,6))
shape = "http://plotkml.r-forge.r-project.org/icon1.png"
tmp3 <- tmp2[runif(length(tmp2))<0.01,]
kml(tmp3["BDRICM"], colour=log1p(BDRICM), colour_scale=SAGA_pal[[1]],z.lim = c(0,6))

#plot all
#plotKML(soilwell.depth[runif(length(soilwell.depth))<0.01,], folder.name = "../profs")
grd <- vect2rast(wells.depth["BDRICM"], cell.size=.1)#, bbox=na.bbox)
#plot(log1p(raster(grd)), col=SAGA_pal[[1]])
grd.pol <- grid2poly(as(grd, "SpatialPixelsDataFrame"))
kml(grd.pol, colour=log1p(BDRICM), colour_scale=SAGA_pal[[1]],z.lim = c(3,10))
save.image("../points/profs/prepare.RData")
## end of script;

