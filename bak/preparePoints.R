## Depth to bedrock / depth to saprolite (>80% rocks):
rm(list = ls(all = TRUE))
library(aqp)
library(GSIF)
library(plotKML)
library(sp)
library(plyr)
library(raster)
library(maptools) 
#global
dir <- "E:/data/soildata/depth/points/codegsifb"
setwd(dir)
source("./head/functions.r")


##############################################
## SOIL PROFILE DATA
##############################################
## Mexican soil profile observations:
#LIM_ROCA:!!!
#An X appears if the physical limitation of the soil is the bedrock.
#LIM_REGO:
#An X appears if the physical limitation of the soil is the regolith.
#LIM_CEME:????
#An X appears if the physical limitation of the soil is a cementation.
#LIM_NIVF:!!!!
#An X appears if the physical limitation of the soil is the water table.
inegi <- read.csv("../profs/soil/INEGIv1_1km.csv")
inegi.xy <- inegi[, c("IDENTIFI", "X_COORD", "Y_COORD")]
coordinates(inegi.xy) <- ~ X_COORD + Y_COORD
mx.csy <- paste0("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=", 
    "-102 +x_0=2500000 +y_0=0 +ellps=WGS84 +units=m +no_defs")
proj4string(inegi.xy) <- CRS(mx.csy)
inegi.ll <- as.data.frame(reproject(inegi.xy))
inegi.ll[!(inegi$X_COORD > 0&inegi$Y_COORD > 0), 2:3] <- NA
inegi$LONWGS84 <- inegi.ll$x
inegi$LATWGS84 <- inegi.ll$y
## Depth to bedrock i.e. 'R' horizon:
sel.r1 <- grep(pattern="x", inegi$LIM_ROCA, ignore.case=FALSE) 
sel.r2 <- grep(pattern="R", inegi$HSIMBOLO, ignore.case=FALSE)
#inegi$HSIMBOLO[sel.r2]
sel.R  <- unique(sel.r1, sel.r2)
inegi$BDRICM <- NA
inegi$BDRICM[sel.R] <- inegi$PROFUNDI[sel.R]
#View(inegi[, c("IDENTIFI", "PROFUNDI", "HSIMBOLO", "LIM_ROCA", "BDRICM")])
bdr.d <- aggregate(inegi$BDRICM, list(inegi$IDENTIFI), max, na.rm=TRUE)
names(bdr.d) <- c("IDENTIFI", "BDRICM")
bdr.d$BDRICM <- ifelse(is.infinite(bdr.d$BDRICM), NA, bdr.d$BDRICM)
MX.spdb <- plyr::join(bdr.d, inegi[, c("IDENTIFI", "LONWGS84", "LATWGS84")], 
            type="left", match ="first")
MX.spdb$SOURCEID <- paste0("MX_", MX.spdb$IDENTIFI)
MX.spdb <- MX.spdb[complete.cases(MX.spdb), ]
#ploting
MX.ll <- MX.spdb[,2:4]
coordinates(MX.ll) <- ~ LONWGS84+LATWGS84
proj4string(MX.ll) <- CRS("+proj=longlat +datum=WGS84")
#plotKML(MX.ll)
#EDA(MX.spdb$BDRICM)

## USA data:
load("../profs/soil/NCSS_all.rda")
sel.r  <- grep("R", NCSS_all$horizons$hzn_desgn, ignore.case = FALSE)
sel.rn <- grep("IR", NCSS_all$horizons$hzn_desgn, ignore.case = FALSE) 
sel.r  <- sel.r[!(sel.r %in% sel.rn)]   
length(sel.r)
#View(sort(NCSS_all$horizons[sel.r, c("hzn_desgn")]
#NCSS_all$horizons <- NCSS_all$horizons[sel.r, ]
NCSS_all$horizons$BDRICM <- NA
## fix typos:
max.d <- aggregate(NCSS_all$horizons$hzn_bot, 
        list(NCSS_all$horizons$site_key), max, na.rm=TRUE)
names(max.d)[1] <- "site_key"
NCSS_all$horizons$hzn_topF <- ifelse(NCSS_all$horizons$layer_sequence>1&
            NCSS_all$horizons$hzn_top==0, 
            plyr::join(NCSS_all$horizons["site_key"], max.d)$x, 
            NCSS_all$horizons$hzn_top) 
NCSS_all$horizons$BDRICM[sel.r] <- NCSS_all$horizons$hzn_topF[sel.r]
bdr2.d <- aggregate(NCSS_all$horizons$BDRICM[sel.r], 
            list(NCSS_all$horizons$site_key[sel.r]), min, na.rm=TRUE)
names(bdr2.d) <- c("site_key", "BDRICM")
bdr2.d$BDRICM <- ifelse(is.infinite(bdr2.d$BDRICM), NA, bdr2.d$BDRICM)
US.spdb <- plyr::join(bdr2.d, NCSS_all$sites[, c("site_key", "LON", "LAT")], 
            type="left", match ="first")
names(US.spdb)[3:4] <- c("LONWGS84", "LATWGS84")
US.spdb$SOURCEID <- paste0("US_", US.spdb$site_key)
US.spdb <- US.spdb[complete.cases(US.spdb), ]
#BDRICM == 1189 is checked
US.ll <- US.spdb[,2:4]
coordinates(US.ll) <- ~ LONWGS84+LATWGS84
proj4string(US.ll) <- CRS("+proj=longlat +datum=WGS84")
#plotKML(US.ll)
#EDA(US.spdb$BDRICM)

## Canada:
load("../profs/soil/CanSIS.rda")
#H2O_TABLE water table depth
#PFROST_DEPTH Permafrost - Depth to
#FAMILY_DEPTH class: Family Criteria - Soil Depth 
names(CanSIS) <- c("sites", "horizons")
summary(CanSIS$horizons$PS_VCSAND)  ## Coarse fragments
## depth to bedrock (R horizon):
summary(as.factor(CanSIS$sites$FAMILY_DEPTH))
View(CanSIS$sites[!CanSIS$sites$FAMILY_DEPTH ==" ", 
        c("SOURCEID","FAMILY_DEPTH")])
#bulk density > 2.67
#sel.b <- which(!is.na(CanSIS$horizons$BD) &CanSIS$horizons$BD>2.67)
sel.r  <- grep("R", CanSIS$horizons$HORIZON, ignore.case=FALSE)
sel.rn <- c(grep ("DRIFT", CanSIS$horizons$HORIZON, ignore.case=FALSE),
           grep ("GIR", CanSIS$horizons$HORIZON, ignore.case=FALSE)) 
sel.r  <- sel.r[!(sel.r %in% sel.rn)]    
CanSIS$horizons[!is.na(CanSIS$horizons$BD) & CanSIS$horizons$BD>2.67, ]     
#CanSIS$horizons[sel.r,"HORIZON"]
#not nure, but use the deepest first
sel.n <- which(CanSIS$horizons$SOURCEID %in% CanSIS$sites$SOURCEID[grep("lit", 
        CanSIS$sites$FAMILY_DEPTH, ignore.case=TRUE)])         
View(CanSIS$horizons[sel.r,c("SOURCEID","UDEPTH.x","LDEPTH.x","HORIZON")])
CanSIS$horizons$BDRICM <- NA
CanSIS$horizons$BDRICM[sel.n] <- CanSIS$horizons$LDEPTH.x[sel.n]
CanSIS$horizons$BDRICM[sel.r] <- CanSIS$horizons$UDEPTH.x[sel.r]
bdr3.d <- aggregate(CanSIS$horizons$BDRICM, list(CanSIS$horizons$SOURCEID), max, na.rm=TRUE)
names(bdr3.d) <- c("SOURCEID", "BDRICM")
bdr3.d$BDRICM <- ifelse(is.infinite(bdr3.d$BDRICM), NA, bdr3.d$BDRICM)
CA.spdb <- plyr::join(bdr3.d, CanSIS$sites[,c("SOURCEID", "LONWGS84", "LATWGS84")], type="left", match ="first")
CA.spdb <- CA.spdb[complete.cases(CA.spdb), ]
CA.ll <- CA.spdb[,2:4]
coordinates(CA.ll) <- ~ LONWGS84+LATWGS84
proj4string(CA.ll) <- CRS("+proj=longlat +datum=WGS84")
#plotKML(CA.ll)
#EDA(CA.spdb$BDRICM)


##############################################
## WELL DATA (DRILLINGS)
##############################################
## Wells data compiled by Wei:
us <- read.csv("../profs/well/wells_us.txt", sep="\t")
us$BDRICM <- us$D_BR*100
names(us)[2:3] <- c("LONWGS84", "LATWGS84")
us$SOURCEID <- paste0("USWELL_", us$Source, "_", rownames(us))
EDA(log1p(us$BDRICM))
dev.copy(png,"../pic/EDA/EDA_us.png", width = 730, height = 480, units = "px")
dev.off()


ca <- read.csv("../profs/well/wells_ca.txt", sep="\t")
ca$BDRICM <- ca$D_BR*100
names(ca)[2:3] <- c("LONWGS84", "LATWGS84")
ca$SOURCEID <- paste0("CAWELL_", ca$Source, "_", rownames(ca))
EDA(log1p(ca$BDRICM))
dev.copy(png,"../pic/EDA/EDA_ca.png", width = 730, height = 480, units = "px")
dev.off()

as <- read.csv("../profs/well/wells_as2.txt", sep="\t")
as$BDRICM <- as$D_BR*100
names(as)[2:3] <- c("LONWGS84", "LATWGS84")
as$SOURCEID <- paste0("ASWELL_", rownames(as))
EDA(log1p(as$BDRICM))
dev.copy(png,"../pic/EDA/EDA_as.png", width = 730, height = 480, units = "px")
dev.off()

eu <- read.csv("../profs/well/wells_eu.txt", sep="\t")
eu$BDRICM <- eu$D_BR*100
names(eu)[2:3] <- c("LONWGS84", "LATWGS84")
eu$SOURCEID <- paste0("EUWELL_", rownames(eu))
EDA(log1p(eu$BDRICM))
dev.copy(png,"../pic/EDA/EDA_eu.png", width = 730, height = 480, units = "px")
dev.off()

####plot wells
na.bbox <- matrix(c(-175,14,-58,80), nrow=2) # north America
wells <- do.call(rbind, list( 
        ca[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")], 
        us[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],
        as[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],
        eu[, c("SOURCEID","LONWGS84","LATWGS84","BDRICM")]))
wells <- wells[complete.cases(wells), ]
EDA(log1p(wells$BDRICM))
dev.copy(png,"../pic/EDA/EDA_all.png", width = 730, height = 480, units = "px")
dev.off()

#wells <- wells[NA.wells$LATWGS84 > na.bbox[2,1] & 
#        wells$LATWGS84 < na.bbox[2,2] & wells$LONWGS84 < na.bbox[1,2] &
#        wells$LONWGS84 > na.bbox[1,1], ]
#plot(wells[, c("LONWGS84", "LATWGS84")])          
wells.depth <- wells
coordinates(wells.depth) <- ~ LONWGS84+LATWGS84
proj4string(wells.depth) <- CRS("+proj=longlat +datum=WGS84")     
#writePointsShape(wells.depth, "as")
wells.depth$BDRICM <- as.integer(wells.depth$BDRICM)
save(wells.depth, file="../profs/wells.depth.rda")

## plot soils:                                                                                                                
soils <- do.call(rbind, list(  
         US.spdb[,c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],   
         MX.spdb[,c("SOURCEID","LONWGS84","LATWGS84","BDRICM")],   
         CA.spdb[,c("SOURCEID","LONWGS84","LATWGS84","BDRICM")])) 
soils <- soils[complete.cases(soils), ]
#soils <- soils[soils$LATWGS84 > na.bbox[2,1] & 
#        soils$LATWGS84 < na.bbox[2,2] & soils$LONWGS84 < na.bbox[1,2] &
#        soils$LONWGS84 > na.bbox[1,1], ]
plot(soils[, c("LONWGS84", "LATWGS84")])
soils.depth <- soils
coordinates(soils.depth) <- ~ LONWGS84+LATWGS84
proj4string(soils.depth) <- CRS("+proj=longlat +datum=WGS84")     
#writePointsShape(wells.depth, "as") 
soils.depth$BDRICM <- as.integer(soils.depth$BDRICM)      
save(soils.depth, file="../profs/soils.depth.rda")       

#-------------------------------combine        
soilwell <- do.call(rbind, list(   
         wells[, c("SOURCEID", "LONWGS84", "LATWGS84", "BDRICM")],   
         soils[, c("SOURCEID", "LONWGS84", "LATWGS84", "BDRICM")])) 
## subset to NorthAmerica continent:
soilwell <- soilwell[complete.cases(soilwell), ]
#soilwell <- soilwell[soilwell$LATWGS84 > na.bbox[2,1] & 
#        soilwell$LATWGS84 < na.bbox[2,2] & soilwell$LONWGS84 < na.bbox[1,2] &
#        soilwell$LONWGS84 > na.bbox[1,1], ]
#plot(soilwell[, c("LONWGS84", "LATWGS84")])
#str(soilwell)
#EDA(log1p(soilwell$BDRICM))
#only wells
soilwell.depth <- soilwell
coordinates(soilwell.depth) <- ~ LONWGS84+LATWGS84
proj4string(soilwell.depth) <- CRS("+proj=longlat +datum=WGS84")
soilwell.depth$BDRICM <- as.integer(soilwell.depth$BDRICM)
save(soilwell.depth, file="../profs/soilwell.depth.rda")


#plot all
soilwell.depth <-  wells.depth
#plotKML(soilwell.depth[runif(length(soilwell.depth))<0.01,], folder.name = "../profs")
grd <- vect2rast(soilwell.depth["BDRICM"], cell.size=.1)#, bbox=na.bbox)
plot(log1p(raster(grd)), col=SAGA_pal[[1]])
grd.pol <- grid2poly(as(grd, "SpatialPixelsDataFrame"))
kml(grd.pol, colour=log1p(BDRICM), colour_scale=SAGA_pal[[1]],z.lim = c(3,10))
save.image("../profs/prepare.RData")
## end of script;

