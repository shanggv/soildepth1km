rm(list = ls(all = TRUE))
library(sp)
library(maps)
library(grDevices)
library(maptools)
library(lattice)
#par(resetPar())
# global define
#gdal.dir = shortPathName("C:\\ms4w")
#gdal_setInstallation(search_path=gdal.dir, rescan=TRUE)
a.dir <- "/data/shang009/big"# dir of the project
w.dir <- paste0(a.dir, "/worldgrids")
m.dir <- paste0(a.dir, "/soildepth")
setwd(m.dir)
source(paste0(a.dir, "/soildepth/code/head/functions.r"))


resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}
Partmp <- resetPar() 
par(Partmp)
load(paste0("./profs/wells.depth.rda"))
load(paste0("./profs/sprofs.depth.rda"))


#prepare country
country.m = map('world', plot=FALSE, fill=TRUE)
IDs <- sapply(strsplit(country.m$names, ":"), function(x) x[1])
country <- as(map2SpatialPolygons(country.m, IDs=IDs), "SpatialLines")



#point locations for BDRICM
bitmap(paste0("./pics/", "p.soil.BDRICM.tiff"), width = 7.48, height = 3.74, units = "in", res =300, type = "tiffgray", pointsize =11)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(country, col="darkgrey")
soil.BDR <- subset(sprofs.depth, !is.na(sprofs.depth$BDRICM))
points(soil.BDR, pch=21, bg="black", cex=.3, col="black")
#dev.copy(png,"./pics/p.soil.BDRICM.png", width = 960, height = 480, units = "px")
dev.off()

bitmap(paste0("./pics/", "p.well.BDRICM.tiff"), width = 7.48, height = 3.74, units = "in", res =300, type = "tiffgray", pointsize =11)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(country, col="darkgrey")
points(wells.depth, pch=21, bg="blue", cex=.3, col="blue")
#dev.copy(png,"./pics/p.well.BDRICM.png", width = 960, height = 480, units = "px")
dev.off()



###cluster
bbox <- matrix(c(-85,35,-84,36), nrow=2)
tmp <- subset(wells.depth, wells.depth@coords[,1]>bbox[1,1] & wells.depth@coords[,1]<bbox[1,2] 
        & wells.depth@coords[,2]>bbox[2,1] & wells.depth@coords[,2]<bbox[2,2])
par(mar=c(0,0,0,0), oma=c(0,0,0,0))        
bitmap(paste0("./pics/", "p.well.cluster.tiff"), width = 3, height = 3.6, units = "in", res =300, type = "tiffgray", pointsize =11)
plot(tmp, pch=21, bg="black", cex=.1, col="black")
lines(x= c(-85,-84),y=c(35,35))
lines(x= c(-85,-84),y=c(36,36))
lines(x= c(-85,-85),y=c(35,36))
lines(x= c(-84,-84),y=c(35,36))
 # north Americ
#dev.copy(png,"./pics/p.well.cluster.png", width = 400, height = 480, units = "px")
dev.off()


#point locations for SAPICM2
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(country, col="darkgrey")
soil.SAP2 <- subset(sprofs.depth, !is.na(sprofs.depth$SAPICM2))
points(soil.SAP2 , pch=21, bg="blue", cex=.6, col="blue")
dev.copy(png,"./pics/p.soil.SAPICM2.png", width = 960, height = 480, units = "px")
dev.off()


####EDA for BDRICM
bitmap(paste0("./pics/", "Hist_well.tiff"), width = 7.48, height = 4, units = "in", res =1000, type = "tiffcrle", pointsize =11)
par(mar = c(4,4,0,0), mfcol=c(1,2))
#EDA(log1p(wells.depth$BDRICM))

hist(wells.depth$BDRICM, main = "", xlim =  quantile(wells.depth$BDRICM, probs = c(0, 0.99)), breaks =1000 , xlab = "Depth to bedrock (cm)")
hist(log1p(wells.depth$BDRICM), main = "", xlab = "Log-transformed depth to bedrock (cm)")
dev.off()

par(mar = c(5.1,4.1,4.1,2.1), mfcol=c(1,1))


#EDA(soil.BDR$BDRICM)
hist(soil.BDR$BDRICM, main = "", xlab = "Depth to bedrock (cm)")
dev.copy(png,"./pics/EDA_soil_BDR.png", width = 400, height = 400, units = "px")
dev.off()





####EDA for SAPICM2
#EDA(soil.SAP2$SAPICM2)
hist(soil.SAP2$SAPICM2, main = "", xlab = "Depth to saprolite (cm)")
dev.copy(png,"./pics/EDA_soil_SAP2.png", width = 400, height = 400, units = "px")
dev.off()

####artificial points
load("./profs/outpoint.rda")
load("./profs/slppoint.rda")
outpoint <- subset(outpoint, outpoint$SLPSRT3a > (10*255/90))
#40(11559 points) will produce too much low value in EU, try 50(2314 points)
slppoint <- subset(slppoint, slppoint$SLPSRT3a > (50*255/90))

par(mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(country, col="darkgrey")
points(outpoint, pch=21, bg="blue", cex=.6, col="blue")
points(slppoint, pch=21, bg="red",  cex=.6, col="red")
dev.copy(png,"./pics/p.arti.png", width = 960, height = 480, units = "px")
dev.off()

#tmp <- subset(slppoint, slppoint@coords[,1]>90 & slppoint@coords[,1]<100 
#        & slppoint@coords[,2]>30 & slppoint@coords[,2]<40)
#plot(tmp)
#dev.copy(png,"./pics/p.artit460.png", width = 600, height = 480, units = "px")
#dev.off()
#kml(slppoint, colour = "blue")
## compare BDR from soils and wells:  
 
                                                                                                              
tmp <- subset(sprofs.depth, !is.na(sprofs.depth$BDRICM))
tname <- "all"
na1 <-tmp

png(file = paste0("./pics/", tname, "_soil.png"), width = 730, height = 480, units = "px")
l1 <- list(country, col = 'grey')
    spplot(na1["BDRICM"], xlim = na1@bbox[1,], ylim = na1@bbox[2,], sp.layout = l1, cex = 0.5,colorkey = list(
    	right = list( 
    		fun = draw.colorkey, 
    		args = list(
    			key = list(
    				at = seq(min(na1$BDRICM), max(na1$BDRICM), 40), # colour breaks
    				col = bpy.colors(11), # colours
    				labels = list(
    					at = seq(min(na1$BDRICM), max(na1$BDRICM), 40), 
    					labels = as.character(seq(min(na1$BDRICM), max(na1$BDRICM), 40)),
                        cex = 2
    				)
    			)
    		)
    	)   
    ))
     
 dev.off()

tmp <- subset(tmp, tmp$BDRICM < 200)
tmp2 <- subset(wells.depth, wells.depth$BDRICM < 200)
tmp2 <- GSIF:: sample.grid(tmp2, cell.size = c(0.2, 0.2), n = 1 )$subset
tmp3 <- subset(wells.depth, wells.depth$BDRICM > 200)
tmp3 <- GSIF:: sample.grid(tmp3, cell.size = c(0.2, 0.2), n = 1 )$subset
tmp3$BDRICM [tmp3$BDRICM >400] <- 400





na.bbox <- matrix(c(-85,35,-80,40), nrow=2) # north Americ
tname <- "na5"
bbox <- na.bbox
#source(paste0(a.dir, "/soildepth/code/1km/plot.points.spplot.r"))

na.bbox <- matrix(c(-175,14,-58,80), nrow=2) # north Americ
tname <- "na"
bbox <- na.bbox


as.bbox <- matrix(c(110,-50,160,-10), nrow=2) # astralia
tname <- "as"
bbox <- as.bbox


eu.bbox <- matrix(c(-10,50,30,70), nrow=2) # astralia
tname <- "eu"
bbox <- eu.bbox




