# title         : pre.region.R
# purpose       : prediction of  models for regional Soil depth ;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Beijing.
# inputs        : WorldGrids layers, SoilProfiles.org points, water well points from various sources
# outputs       : prediction models ;
# remarks 1     : Takes ca ??? mins to run with 10 cpus in use;

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

a.dir <- "/data/shang009/big"# dir of the project
w.dir <- paste0(a.dir, "/worldgrids")
m.dir <- paste0(a.dir, "/soildepth")
source(paste0(a.dir, "/soildepth/code/head/functions.r"))
setwd(m.dir)
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 0  #1: add artificial points; 0: not
soil.flag <-0 # 0: without soil profiles; 1: add soil profiles
en.num <- 1   #number of ensemble prediction
kml.flag <- 1 # 0:not produce kml, 1: produce kml for predition and covariates
tilestest <- c( "T477") #iowa


fit.name <- "us4" # c("eu", "as", "us", "ca", "all")
n.name <-"27"

source(paste0(a.dir, "/soildepth/code/1km/pre.region.r"))

fit.name <- "us3" # c("eu", "as", "us", "ca", "all")
n.name <-"27"
source(paste0(a.dir, "/soildepth/code/1km/pre.region.r"))

fit.name <- "us5" # c("eu", "as", "us", "ca", "all")
n.name <-"27"
source(paste0(a.dir, "/soildepth/code/1km/pre.region.r"))

fit.name <- "us14" # c("eu", "as", "us", "ca", "all")
n.name <-"27"
source(paste0(a.dir, "/soildepth/code/1km/pre.region.r"))


fit.name <- "mus4" # c("eu", "as", "us", "ca", "all")
n.name <-"m3"
source(paste0(a.dir, "/soildepth/code/1km/pre.region.r"))

t.dir <- "/data/shang009/big/soildepth/surficial"
setwd(paste0(t.dir, "/iowa"))
unzip("depth_to_bedrock.zip", exdir = "tmp")
tname <- "Iowa_dtb"
src_d <- paste0(t.dir, "/iowa/tmp/depth_to_bedrock.img") 
system(paste0("gdalwarp -ot Int16 -wt Int16 -srcnodata -32768 -dstnodata -32768 -r average ",
            "-t_srs  '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'",
             " -tr 0.00833333333333333 0.00833333333333333 -overwrite ",
             src_d," ", "./tmp/tmp0.tif")) 
tmp0 <- readGDAL("./tmp/tmp0.tif")
tmp0$band1 <- tmp0$band1*30.48


#us
src_d <- paste0(w.dir, "/T477/BDRICM_1km_p0_a1_s0_us_1o_T477.tif")
system(paste0("gdalwarp  -overwrite -te ",            
             tmp0@bbox[1,1]," ", tmp0@bbox[2,1]," ",tmp0@bbox[1,2]," ", tmp0@bbox[2,2]," ",
             src_d," ", "./tmp/tmp1.tif"))
tmp1 <- readGDAL("./tmp/tmp1.tif")
tmp1$band1[is.na(tmp0$band1)] <- NA

#us4
src_d <- paste0(w.dir, "/T477/BDRICM_1km_27p0_a0_s0_us4_1o_T477.tif")
system(paste0("gdalwarp  -overwrite -te ",            
             tmp0@bbox[1,1]," ", tmp0@bbox[2,1]," ",tmp0@bbox[1,2]," ", tmp0@bbox[2,2]," ",
             src_d," ", "./tmp/tmp2.tif"))
tmp2 <- readGDAL("./tmp/tmp2.tif")
tmp2$band1[is.na(tmp0$band1)] <- NA

#usm4
src_d <- paste0(w.dir, "/T477/BDRICM_1km_m3p0_a0_s0_mus4_1o_T477.tif")
system(paste0("gdalwarp  -overwrite -te ",            
             tmp0@bbox[1,1]," ", tmp0@bbox[2,1]," ",tmp0@bbox[1,2]," ", tmp0@bbox[2,2]," ",
             src_d," ", "./tmp/tmp3.tif"))
tmp3 <- readGDAL("./tmp/tmp3.tif")
tmp3$band1[is.na(tmp0$band1)] <- NA

#us3
src_d <- paste0(w.dir, "/T477/BDRICM_1km_27p0_a0_s0_us3_1o_T477.tif")
system(paste0("gdalwarp  -overwrite -te ",            
             tmp0@bbox[1,1]," ", tmp0@bbox[2,1]," ",tmp0@bbox[1,2]," ", tmp0@bbox[2,2]," ",
             src_d," ", "./tmp/tmp4.tif"))
tmp4 <- readGDAL("./tmp/tmp4.tif")
tmp4$band1[is.na(tmp0$band1)] <- NA

#us14
src_d <- paste0(w.dir, "/T477/BDRICM_1km_27p0_a0_s0_us14_1o_T477.tif")
system(paste0("gdalwarp  -overwrite -te ",            
             tmp0@bbox[1,1]," ", tmp0@bbox[2,1]," ",tmp0@bbox[1,2]," ", tmp0@bbox[2,2]," ",
             src_d," ", "./tmp/tmp5.tif"))
tmp5 <- readGDAL("./tmp/tmp5.tif")
tmp5$band1[is.na(tmp0$band1)] <- NA


"tiff12nc"
setwd(m.dir)
plotList <- NULL
p.at <- seq(0,15000,1000)
spplot(tmp0, zcol =c("band1"), col.regions=SAGA_pal[[1]], at = p.at, xlab = "US model")
plotList[[1]] <- spplot(tmp1, zcol =c("band1"), col.regions=SAGA_pal[[1]], at = p.at, xlab = "US model")
plotList[[2]] <- spplot(tmp2, zcol =c("band1"), col.regions=SAGA_pal[[1]], at = p.at, xlab = "Iowa model")
plotList[[3]] <- spplot(tmp3, zcol =c("band1"), col.regions=SAGA_pal[[1]], at = p.at, xlab = "Without Iowa model")
plotList[[4]] <- spplot(tmp4, zcol =c("band1"), col.regions=SAGA_pal[[1]], at = p.at, xlab = "Indiana model")
plotList[[5]] <- spplot(tmp5, zcol =c("band1"), col.regions=SAGA_pal[[1]], at = p.at, xlab = "Northern High Plains model")


bitmap(paste0("./pics/", "Iowaeg.tiff"), width = 7.48, height = 7, units = "in", res =300, type = "tiff12nc", pointsize =11)
par(mar=c(0, 0, 0.1, 0.1)) 
do.call(grid.arrange, c(plotList, ncol=2, nrow =3))
 dev.off()
 par(mar=c(5.1, 4.1, 4.1, 2.1)) 
