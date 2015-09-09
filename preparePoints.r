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
dir <- "E:/data/soildata/depth/points/codegsifb"
setwd(dir)
source("./head/functions.r")

##############################################
## WELL DATA (DRILLINGS)
##############################################
## Wells data compiled by Wei:
us <- read.csv("../profs/well/wells_us.txt", sep="\t")
us$BDRICM <- us$D_BR*100
names(us)[2:3] <- c("LONWGS84", "LATWGS84")
us$SOURCEID <- paste0("USWELL_", us$Source, "_", rownames(us))
EDA(log1p(us$BDRICM))
dev.copy(png,"../pic/EDA/EDA_well_us.png", width = 730, height = 480, units = "px")
dev.off()

ca <- read.csv("../profs/well/wells_ca.txt", sep="\t")
ca$BDRICM <- ca$D_BR*100
names(ca)[2:3] <- c("LONWGS84", "LATWGS84")
ca$SOURCEID <- paste0("CAWELL_", ca$Source, "_", rownames(ca))
EDA(log1p(ca$BDRICM))
dev.copy(png,"../pic/EDA/EDA_well_ca.png", width = 730, height = 480, units = "px")
dev.off()

as <- read.csv("../profs/well/wells_as2.txt", sep="\t")
as$BDRICM <- as$D_BR*100
names(as)[2:3] <- c("LONWGS84", "LATWGS84")
as$SOURCEID <- paste0("ASWELL_", rownames(as))
EDA(log1p(as$BDRICM))
dev.copy(png,"../pic/EDA/EDA_well_as.png", width = 730, height = 480, units = "px")
dev.off()

eu <- read.csv("../profs/well/wells_eu.txt", sep="\t")
eu$BDRICM <- eu$D_BR*100
names(eu)[2:3] <- c("LONWGS84", "LATWGS84")
eu$SOURCEID <- paste0("EUWELL_", rownames(eu))
EDA(log1p(eu$BDRICM))
dev.copy(png,"../pic/EDA/EDA_well_eu.png", width = 730, height = 480, units = "px")
dev.off()

####combin wells
wells <- do.call(rbind, list( 
        ca[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")], 
        us[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],
        as[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],
        eu[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")]))
wells <- wells[complete.cases(wells), ]
EDA(log1p(wells$BDRICM))
dev.copy(png,"../pic/EDA/EDA_well_all.png", width = 730, height = 480, units = "px")
dev.off()

wells.depth <- wells
coordinates(wells.depth) <- ~ LONWGS84+LATWGS84
proj4string(wells.depth) <- CRS("+proj=longlat +datum=WGS84")     
#writePointsShape(wells.depth, "as")
wells.depth$BDRICM <- as.integer(wells.depth$BDRICM)
save(wells.depth, file="../profs/wells.depth.rda")
#load("../profs/wells.depth.rda")

##############################################
## SOIL PROFILE DATA
##############################################
###Soil profiles are prepared in the fold of profiles for SoilGrids1km
load("../profs/sprofs.depth.rda")
tmp <- sprofs.depth$BDRICM[!is.na(sprofs.depth$BDRICM)]
EDA(log1p(tmp))
dev.copy(png,"../pic/EDA/EDA_soil_BDR.png", width = 730, height = 480, units = "px")
dev.off()

tmp <- sprofs.depth$SAPICM[!is.na(sprofs.depth$SAPICM)]
EDA(log1p(tmp))
dev.copy(png,"../pic/EDA/EDA_soil_SAP.png", width = 730, height = 480, units = "px")
dev.off()

tmp <- sprofs.depth$SAPICM2[!is.na(sprofs.depth$SAPICM2)]
EDA(log1p(tmp))
dev.copy(png,"../pic/EDA/EDA_soil_SAP2.png", width = 730, height = 480, units = "px")
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
save.image("../profs/prepare.RData")
## end of script;

