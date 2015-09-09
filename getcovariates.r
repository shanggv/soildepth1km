# title         : getWorldGrids.R
# purpose       : Prepared prediction locations for Global Soil depth;
# reference     :
# producer      : Prepared by W. Shangguan
# address       : In Wageningen, NL.
# inputs        : WorldGrids layers
# outputs       : compressed RDA files (per tile);
# remarks 1     : Takes ca 0.7 hrs to run with 10 cpus in use (most costly
# operations: conversion to SpatialPixels, reading of GeoTiFFs and generation
# of the  prediction locations);
rm(list = ls(all = TRUE))
#pkgs = names(sessionInfo()$otherPkgs)
#pkgs = paste('package:', pkgs, sep = "")
#lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
library(rgdal)
library(sp)
library(GSIF)
library(RSAGA)
library(raster)
library(plyr)
library(snowfall)
library(R.utils)

#global define
wdir <- "/data/shang009/big/worldgrids"
setwd(wdir)

fillgap <- function(obj, i, var, type, gmean)
# type = c("saga", "mean", "median", "zero", "majority")
{
    if(sum(is.na(obj@data[[var]])) == 0){
        ret <- obj
    }else if(sum(is.na(obj@data[[var]]))/length(obj@data[[var]]) > 0.5  | sum(!is.na(obj@data[[var]])) < 10000){
        ret <- obj
        if(class(ret[[var]]) == "integer") ret[[var]] <- as.integer(round(gmean))
        else if(class(ret[[var]]) == "factor") ret[[var]] <- gmean
    }else{
        ret <- obj
        if(type == "saga"){
            writeGDAL(ret[var], fname =  paste0("tmp",i,".sdat"), drivername = "SAGA")
            rsaga.geoprocessor(lib="grid_tools", module=7, param=list(INPUT = paste0("tmp",i,".sgrd")))
            ret[[var]]<- as.integer(readGDAL(fname = paste0("tmp",i,".sdat"))$band1[ret@grid.index])
        }else if(type == "mean")
        {
            if(class(ret[[var]]) == "integer")
            {
                ret[[var]][is.na(ret[[var]])]   <- as.integer(round(mean(ret[[var]], na.rm = T)))
            }else ret[[var]][is.na(ret[[var]])] <- mean(ret[[var]], na.rm = T)
        }else if(type == "median")
        {
            if(class(ret[[var]]) == "integer")
            {
                ret[[var]][is.na(ret[[var]])]   <- as.integer(round(median(ret[[var]], na.rm = T)))
            }else ret[[var]][is.na(ret[[var]])] <- mean(ret[[var]], na.rm = T)
        }else if(type == "zero")
        {
            ret[[var]][is.na(ret[[var]])] <- 0
        }else if(type == "majority")
        {
            x.n <- count(ret[[var]])
            ind.first <- order(x.n[2], decreasing = T)[1:2]
            if(is.na(x.n[ind.first[1],1])){
                ret[[var]][is.na(ret[[var]])] <- x.n[ind.first[2],1]
            }else ret[[var]][is.na(ret[[var]])] <- x.n[ind.first[1],1]
        }
    }
    return(ret)
}

###boudingbox = 10, to be added
###saga: what method?
#rsaga.get.usage(lib="grid_tools", module=7)
#rsaga.geoprocessor(lib="grid_tools", module=7, param=list(INPUT="dist.sgrd", RESULT="sp_dist.sgrd"))
# = rsaga.close.gaps("dist.sgrd", "sp_dist.sgrd")
#rsaga.geoprocessor(lib="grid_tools", module=25, param=list(GRID="tmp.sgrd", CLOSED="tmp2.sgrd"))
#rsaga.close.gaps("dist.sgrd", "sp_dist.sgrd")
#r.fillnulls  grass: bilinear,bicubic,rst
#http://grass.osgeo.org/grass64/manuals/r.fillnulls.html


## legends: GLTUHA3x vs LITUSG3a; LFOUSG3a, ELFUSG3a vs L3POBI3b
levs.LITUSG3a <- read.table("LITUSG3a.txt", sep="\t", header = TRUE)
levs.ELFUSG3a <- read.table("ELFUSG3a.txt", sep="\t", header = TRUE)

## prepare a tiling system:
grd <- GDALinfo("LMTGSH3a.tif")
tiles <- getSpatialTiles(grd, block.x=10, block.y=10)
str(tiles)

## Create directories for writing:
tt <- list.dirs(wdir)
for(i in 1:nrow(tiles)){
  if(!(paste0(wdir, "/T", i) %in% tt)){
    dir.create(paste(wdir, "/T", i, "/", sep=""), showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
}


## PREPARING COVARIATES  AND PCA AS TILES
## import, filter and write RDA files to directories (per tile)
#i <- 439 #for test

wrapper.makeRDA <- function(i){
    if(tiles[i,"yu"] < -59){
        unlink(paste(wdir, "/T", i, sep=""), recursive = TRUE, force = TRUE)
    #}else if(1){
   }else if(!(file.exists(file=paste0(wdir, "/T", i, "/grid1kmo_T", i, ".rda")))){
    setwd(wdir)
    m <- readGDAL("LMTGSH3a.tif", offset=c(tiles$offset.y[i], tiles$offset.x[i]), region.dim=c(tiles$region.dim.y[i], tiles$region.dim.x[i]), silent=TRUE)
    names(m) <- "LMTGSH3a"
    m$LMTGSH3a <- as.integer(m$LMTGSH3a==1|m$LMTGSH3a==2)
    m$LMTGSH3a[m$LMTGSH3a == 0] <- NA

    ## if it is empty or less than 4 pixels -> remove folder
    if(sum(m$LMTGSH3a, na.rm = T)<4){
      unlink(paste(wdir, "/T", i, sep=""), recursive = TRUE, force = TRUE)
    } else {
      m <- as(m, "SpatialPixelsDataFrame") ## TH: this process takes time!
      gc()
    ## read data:
    for(j in 1:length(g.lst)){
      m@data[,g.lst[j]] <- readGDAL(set.file.extension(g.lst[j],".tif"), offset=c(tiles$offset.y[i], tiles$offset.x[i]), region.dim=c(tiles$region.dim.y[i], tiles$region.dim.x[i]), silent=TRUE)$band1[m@grid.index]
      ## categorical vars:
      if(g.lst[j]=="LITUSG3a"){
        xf <- data.frame(Number=as.factor(m$LITUSG3a))
        suppressWarnings( m$LITUSG3a <- join(x=xf, y=levs.LITUSG3a[,c("Number","NAME")], type="left", match="first")$NAME )
      }
      if(g.lst[j]=="ELFUSG3a"){
        xf <- data.frame(Number=as.factor(m$ELFUSG3a))
        suppressWarnings( m$ELFUSG3a <- join(x=xf, y=levs.ELFUSG3a[,c("Number","NAME")], type="left", match="first")$NAME )
      }
    }
    for(j in 1:length(g.lst)){
        type <- "mean"#default
        tmp <- c("G01ESA3a", "G02ESA3a", "G03ESA3a", "G04ESA3a", "G05ESA3a",
            "G06ESA3a", "G07ESA3a", "G08ESA3a", "G09ESA3a", "G10ESA3a", "G11ESA3a",
            "G12ESA3a", "G13ESA3a", "G14ESA3a", "G15ESA3a", "G16ESA3a", "G17ESA3a",
            "G18ESA3a", "G19ESA3a", "G20ESA3a", "G21ESA3a", "G22ESA3a", "L11IGB3a",
            "L12IGB3a", "GCPIIA3a")
         if(g.lst[j] %in% tmp) type <- "zero" else
         if(g.lst[j] %in% c("LITUSG3a", "ELFUSG3a")) type <- "majority"
#        print(g.lst[j])
#        print(sum(is.na(m[[g.lst[j]]])& !is.na(m@data["LMTGSH3a"]) ))
        m <- fillgap(m, i, g.lst[j], type, gMeans[[g.lst[j]]])
    }
    flist <- list.files(pattern = "tmp")
    file.remove(flist)
    m@data["LMTGSH3a"] <- NULL
    proj4string(m) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    ## replace factors with indicators:
    for(kk in c("LITUSG3a","ELFUSG3a")){
      if(kk=="LITUSG3a"){ ln <- levels(levs.LITUSG3a$NAME) }
      if(kk=="ELFUSG3a"){ ln <- levels(levs.ELFUSG3a$NAME) }
      for(k in 1:length(ln)){
        vn <- paste(kk, k, sep="_")
        m@data[,vn] <- as.integer(m@data[,kk]==ln[k])
      }
    }
    varsn <- c(dimnames(worldgrids.spc$rotation)[[1]], "LITUSG3a","ELFUSG3a")
    m@data <- m@data[ ,varsn]
    save(m, file=paste(wdir, "/T", i, "/grid1kmo_T", i, ".rda", sep=""))

    varsn <- dimnames(worldgrids.spc$rotation)[[1]]
    x.s <- scale(m@data[,varsn], center=cMeans, scale=cStdev)  ## TH: very important -> global means/stdevs need to be used
    ## This is an elegant way to replace all missing values
    x.s[is.na(x.s)] <- 0  # this is not always good.
    ## predict components using the global PCA model:
    #this is equal to predict(worldgrids.spc, x.s) but shorter time, as.integer to speed up the following steps.
    m@data <- as.data.frame(signif(scale(x.s, worldgrids.spc$center, worldgrids.spc$scale) %*% worldgrids.spc$rotation, 3)) #this takes longest time
    ## We round-up numbers... otherwise the files are HUGE!!
    m@data[,paste0("PC", length(worldgrids.spc$center)-1)] <- NULL
    m@data[,paste0("PC", length(worldgrids.spc$center))]   <- NULL ##  last two components can be removed
    save(m, file=paste(wdir, "/T", i, "/grid1km_T", i, ".rda", sep=""))  ## This takes 10-60 seconds
    }
}
}

#nRuns <- 213
nRuns <- 1:nrow(tiles)
nSlaves <- 10
sfInit(parallel = TRUE, cpus = nSlaves)
#sfInit( parallel=TRUE, type="SOCK", socketHosts = c(rep("localhost",nSlaves), rep("sheep1@sheep1", nSlaves),
#    rep("sheep2@sheep2", nSlaves), rep("sheep3@sheep3", nSlaves)),
#    slaveOutfile="~/errorwg.log")
#sfInit( parallel=TRUE, type="SOCK", socketHosts = c( rep("sheep3@sheep3", 1)))
sfLibrary(rgdal)
sfLibrary(plyr)
sfLibrary(RSAGA)
load("worldgrids.spc.rda")
load("g.rda")
## export all objects that are used in this function:
worldgrids.spc$sdev <- NULL
worldgrids.spc$x <- NULL
sfExport("tiles", "g.lst", "levs.LITUSG3a", "levs.ELFUSG3a", "gMeans","cMeans", "cStdev", "worldgrids.spc", "wdir", "fillgap")
ptm <- proc.time()
x <- sfClusterApplyLB(nRuns, wrapper.makeRDA)
proc.time() - ptm
sfStop()
###2621s with 10 processors on dongky



## Check that all "RDA" files can be 'attached':
rda.lst <- list.files(pattern=glob2rx("grid1km*.rda"), recursive=TRUE, full.names=TRUE)
tmp <- NULL 
for(i in 1:length(rda.lst)){
  suppressWarnings(attach(rda.lst[i]))
  x <- ls(paste("file:",rda.lst[i], sep=""))
  if(!x=="m"){
    warning(paste("Missing object 'm' at node", i))
    tmp <- c (tmp,i)
  }
  detach(paste("file:",rda.lst[i], sep=""), character.only = TRUE)
}

## clean up
rm(list=c("m","x","xf"))


## end of script;
