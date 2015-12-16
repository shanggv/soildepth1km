# title         : getsubpoints.R
# purpose       : subset points for BDRICM in different ways;
# reference     :
# producer      : Prepared by W. Shangguan
# address       : In Wageningen, NL.
# inputs        : soil, well, artificial points
# outputs       : compressed RDA files;
# remarks 1     : Takes several minites
# of the  prediction locations);
rm(list = ls(all = TRUE))
library(sp)
library(maps)
library(maptools)
library(raster)
# global define
en.num <- 10   #number of ensemble prediction
# directory
a.dir <- "/data//shang009/big/"# dir of the project
m.dir <- paste0(a.dir, "/soildepth")
setwd(m.dir)
source(paste0(a.dir, "/soildepth/code/head/functions.r"))

for (PC.flag in 0:1)
{
load(paste0("./profs/subs/soil.sp_", PC.flag, ".rda"))
load(paste0("./profs/subs/well.sp_", PC.flag, ".rda"))
load(paste0("./profs/subs/arti.sp_", PC.flag, ".rda")) 
soil.sp <- soil.sp[, names(well.sp)]
soil.sp <- subset(soil.sp, !is.na(soil.sp$BDRICM))
soil.sp$type <- 1
well.sp$type <- 2
arti.sp$type <- 3

#prepare country
country.m = map('world', plot=FALSE, fill=TRUE)
IDs <- sapply(strsplit(country.m$names, ":"), function(x) x[1])
country <- map2SpatialPolygons(country.m, IDs=IDs)
proj4string(country) <- proj4string(soil.sp)
names(country)
#get country for soil.sp
ov <- over(soil.sp, country)
soil.sp$country <- ov

save(soil.sp, file = paste0("./profs/subs/subs.sp_", PC.flag, "_soil.rda"))

#only well and arti
val.r <- c("eu", "as", "na")
lonmin <- c(-10, 110,-165)
lonmax <- c( 30, 160, -50)
latmin <- c( 50, -50,  30)
latmax <- c( 70, -10,  90)
names(lonmin) <- names(lonmax) <- names(latmin) <- names(latmax) <- val.r

sub.sp <- as.list(NULL)
sub.sp.at <- as.list(NULL)
for(i in 1:3)
{
  fit.name <- val.r[i]
  sub.sp[[i]] <- subset(well.sp, well.sp$LONWGS84 > lonmin[fit.name] & 
     well.sp$LONWGS84 < lonmax[fit.name] & 
     well.sp$LATWGS84 > latmin[fit.name] & 
     well.sp$LATWGS84 < latmax[fit.name])
  sub.sp.at[[i]] <- subset(arti.sp, arti.sp$LONWGS84 > lonmin[fit.name] & 
     arti.sp$LONWGS84 < lonmax[fit.name] & 
     arti.sp$LATWGS84 > latmin[fit.name] & 
     arti.sp$LATWGS84 < latmax[fit.name])
}

###split na into us and canada
s_us_ca <- function(sp)
{
    ov <- over(sp[[3]], country)
    sp[[3]]$country <- ov
    #without alaska
    sp[[4]] <- subset(sp[[3]], sp[[3]]$country==217 )
    sp[[5]] <- subset(sp[[3]], sp[[3]]$country==38)
    sp[[3]] <- NULL
    names(sp) <- c("eu", "as", "us", "ca")
    return(sp)
}
sub.sp <- s_us_ca(sub.sp)  ####takes 2 mins
sub.sp.at <- s_us_ca(sub.sp.at)
sub.sp[[5]] <- well.sp
sub.sp.at[[5]] <-  arti.sp
names(sub.sp)[5] <- "all"
names(sub.sp.at)[5] <- "all"

rm(well.sp, arti.sp)
###split wells and combine with artificial points
#total: "eu"：311560, "as"：5911, "us"： 652376 (without 48019 from alaska), "ca"：509411 ，"all":1527277
#subset: "eu"：8071, "as"：4078, "us"：41549, "ca"：15965 ，"all":71087
for(i in 1:length(sub.sp))
{
    set.seed(1014)
    subs.sp <- splitsp(sub.sp[[i]], cellsize = 0.1, n = 3, sn = en.num)$sub
    subs.sp <- lapply(subs.sp, function(x){rbind(x,sub.sp.at[[i]])})  
    subs.sp <- lapply(subs.sp, function(x){x$SOURCEID <- as.character(x$SOURCEID); return(x)}) 
    save(subs.sp, file=paste0("./profs/subs/subs.sp_", PC.flag, "_", names(sub.sp)[i], ".rda")) 
    suba.sp <- rbind(sub.sp[[i]],sub.sp.at[[i]])
    suba.sp$SOURCEID <- as.character(suba.sp$SOURCEID) # to avoid factor taking big storage
       suba.sp$source <- sapply(suba.sp$SOURCEID, USE.NAMES = F, function(x) 
        {
            y <- as.integer(strsplit(x, "_")[[1]][2])       
        })
    save(suba.sp, file=paste0("./profs/subs/suba.sp_", PC.flag, "_", names(sub.sp)[i], ".rda"))      
}

}# end of for PC.flag


