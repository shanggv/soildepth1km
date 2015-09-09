# title         : getoverlay.R
# purpose       : overlay points with covariates;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : WorldGrids layers, SoilProfiles.org points, water well points, outcrop points and slope points
# outputs       : overlayed spatial points data frame;
# remarks 1     : Takes ca 0.5 hours to run with 10 cpus in use;
rm(list = ls(all = TRUE))
library(sp)
library(snowfall)
#global define
# directory
a.dir <- "/data//shang009/big/"# dir of the project
w.dir <- paste0(a.dir, "/worldgrids")
m.dir <- paste0(a.dir, "/soildepth")
source(paste0(a.dir, "/soildepth/code/head/functions.r"))
setwd(m.dir) 

#j <- 439 #test
#overlay by tiles

sp=atpoint
wrapper.over <- function(j, sp){
  load(rda.lst[j])
  ## subset to tile to speed up:
  sel <- which(sp@coords[,1] > m@bbox[1,1] & sp@coords[,1] < m@bbox[1,2] & sp@coords[,2] > m@bbox[2,1] & sp@coords[,2] < m@bbox[2,2])
  ## overlay only if there is overlap:
  if(!length(sel)==0){
    sp  <- sp[sel,]
    proj4string(sp) <- m@proj4string
    ov <- over(sp, m)
    ov <- cbind(data.frame(sp), ov)
    ov$optional <- NULL
    ov <- ov[rowSums(is.na(ov[,names(m)]))==0,] 
  } else {
    ov <- NULL
  }
  return(ov)
  gc()
}
# change to spatial points
tosp <- function(sub.sp){
    sub.sp$x <- sub.sp$LONWGS84
    sub.sp$y <- sub.sp$LATWGS84
    coordinates(sub.sp) <- ~ x +y
    proj4string(sub.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    return(sub.sp)
}

## overlay points and grids and create regression matrix:
## 369 tiles
##soil profiles
load("./profs/sprofs.depth.rda")
##water wells:
load("./profs/wells.depth.rda")
## list of 2 point data sets: well and soil
soilwell.lst <- list(sprofs.depth, wells.depth)
rm(sprofs.depth, wells.depth)
##atificial points
load("./profs/outpoint.rda")
load("./profs/slppoint.rda")
#select reliable points by slope
##10(3664 points)
outpoint <- subset(outpoint, outpoint$SLPSRT3a > (10*255/90))
outpoint$BDRICM <- as.integer(round(runif(length(outpoint), 0, 20)))
#40(11559 points) will produce too much low value in EU, try 50(2314 points)
slppoint <- subset(slppoint, slppoint$SLPSRT3a > (50*255/90))
slppoint$BDRICM <- as.integer(round(runif(length(slppoint), 0, 20)))
atpoint <- rbind(outpoint, slppoint)
atpoint$SLPSRT3a <- NULL


load(paste0(a.dir, "/worldgrids/worldgrids.spc.rda"))
for(PC.flag in 0:1)
{
    if(PC.flag == 1)
    {
        pr.lst <- paste0("PC", 1:(length(worldgrids.spc$center)-2))
        rda.lst <- list.files(pattern=glob2rx("grid1km_T*.rda"), path = w.dir, recursive=TRUE, full.names=TRUE)
    }else
    {        
        pr.lst <- dimnames(worldgrids.spc$rotation)[[1]]
        rda.lst <- list.files(pattern=glob2rx("grid1kmo_T*.rda"), path = w.dir, recursive=TRUE, full.names=TRUE)
    } 
    ###overlay by tiles       
    nRuns <- 1:length(rda.lst)
    sfInit(parallel=TRUE, cpus=10, slaveOutfile="~/errorwg.log")
    sfLibrary(sp)
    sfExport("rda.lst")
    ## overlay:
    ov.lst <- NULL
    ov.at <- NULL
    ptm <- proc.time()
    for(i in 1:length(soilwell.lst)){
      gc()
      ov <- sfClusterApplyLB(nRuns, wrapper.over, sp=soilwell.lst[[i]])
      ov.lst[[i]] <- do.call(rbind, ov)
    }
    ov <- sfClusterApplyLB(nRuns, wrapper.over, sp=atpoint)
    ov.at <- do.call(rbind, ov)
    print( proc.time() - ptm)
    ## overlay takes ca 1171 sec for PC.flag =1, 550 for  PC.flag = 0
    sfStop()
    names(ov.lst) <- c("soil","well")
    names(ov.at)[1:3] <- c("SOURCEID","LONWGS84", "LATWGS84")
    ov.at$SOURCEID <- paste0(ov.at$SOURCEID, "_",1:length(ov.at$SOURCEID))
    # change to spatial points
    psub <- ov.lst[[c("soil")]] 
    soil.sp <- tosp(psub)
    psub <- ov.lst[[c("well")]]   
    #get rid data points from Arizona, too much high values
    psub <- psub[as.character(psub$SOURCEID )< "USWELL_2_1280"  | as.character(psub$SOURCEID) > "USWELL_2_1808" , ]
    well.sp <- tosp(psub)
    arti.sp <- tosp(ov.at)
    save(soil.sp, file=paste0("./profs/subs/soil.sp_", PC.flag, ".rda"))
    save(well.sp, file=paste0("./profs/subs/well.sp_", PC.flag, ".rda"))
    save(arti.sp, file=paste0("./profs/subs/arti.sp_", PC.flag, ".rda"))  
}




