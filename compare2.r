#compare with raster
rm(list = ls(all = TRUE))
library(rgdal)
library(gdalUtils)
library(raster)
w.dir <- "/home/shang009/big/soildepth/surficial"
t.dir <- paste0("/home/shang009/big/worldgrids")
a.dir <- "/home/shang009/big/"# dir of the project
source(paste0(a.dir, "/soildepth/code/head/functions.r"))
r0<-tmp0
r1 <-tmp1
comparedepth <- function(r0, r1, tname)
{
    r1$dif<- r1$band1-r0$band1
    r2 <- cor(r1$band1,r0$band1, use = "complete.obs")
    me <- mean(r1$dif, na.rm = T)
    rmse <- signif(sqrt(mean(r1$dif ^ 2, na.rm = T)), 3)
    r0$cl <- toclass(r0$band1, dclass)
    r1$cl <- toclass(r1$band1, dclass)
    com.c <- getcor(r0$cl, r1$cl, length(dclass$class))
    plotList <- NULL
    p.at <- seq(0,20000,1000)
    plotList[[1]] <- spplot(r1, zcol =c("band1"), col.regions=SAGA_pal[[1]], at = p.at, xlab = "r1")
    plotList[[2]] <- spplot(r0, zcol =c("band1"), col.regions=SAGA_pal[[1]], at = p.at, xlab = "r0")
    plotList[[3]] <- spplot(r1, zcol =c("dif"), col.regions=SAGA_pal[[1]], xlab = "dif")
    do.call(grid.arrange, c(plotList, ncol=2, nrow =2))
    sp.text(c(700,400), paste0("r2: ", round(r2, 3), "\nme: ", round(me,3), "\nrmse: ", rmse,
        "\n sumc1:", round(com.c$sumc1,3), "\n sumc2:", round(com.c$sumc2,3)))
    #plot(plotList)
    dev.copy(png,paste0("../", tname, ".png"),  width = 1000, height = 700, units = "px")
    dev.off()
    return(list(r2 = r2, me = me, rmse =rmse, com.c = com.c))
}

kmldtb <- function(tmp,tname,i)
{
    kml(tmp, colour = log1p(dtb), z.lim =c(3,10),
        raster_name = paste0(tname, i, ".png"),
        colour_scale = SAGA_pal[[1]],
        folder.name =   paste0(tname, i, ".kml" ),
        file.name = paste0(tname, i, ".kml"))
    flist <-  list.files(pattern = tname)
    file.copy(from=flist, to = paste0(w.dir, "/comparekml/"), overwrite = T)
    file.remove(flist)
}



st <- NULL
#iowa:T477
i<-1
setwd(paste0(w.dir, "/iowa"))
unzip("depth_to_bedrock.zip", exdir = "tmp")
tname <- "Iowa_dtb"
src_d <- paste0(w.dir, "/iowa/tmp/depth_to_bedrock.img")
gdalwarp(src_d, dstfile="./tmp/tmp0.tif", tr = c(0.008333333, 0.008333333),
	r = "average", dstnodata = NA,
    t_srs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
    overwrite=TRUE,verbose=TRUE)
tmp0 <- readGDAL("./tmp/tmp0.tif")
tmp0$band1 <- tmp0$band1*30.48

src_d <- paste0(t.dir, "/T477/BDRICM_1kmo_T477.tif")
gdalwarp(src_d, dstfile="./tmp/tmp1.tif", te = as.vector(tmp0@bbox),
    overwrite=TRUE,verbose=TRUE)
tmp1 <- readGDAL("./tmp/tmp1.tif")
tmp1$band1 <- expm1(tmp1$band1)
st[[1]] <- comparedepth(tmp0, tmp1, tname)
del_unzip()
tmp0$dtb <- tmp0$band1
kmldtb(tmp0,tname,0)


#Ohio: T442, T478
i <-2
setwd(paste0(w.dir, "/ohio"))
tname <- "Ohio_dtb"
src_d <- paste0(w.dir, "/ohio/depthb")
gdalwarp(src_d, dstfile="./tmp0.tif", tr = c(0.008333333, 0.008333333),
	r = "average", dstnodata = NA,
    t_srs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
    overwrite=TRUE,verbose=TRUE)
tmp0 <- readGDAL("./tmp0.tif")
tmp0$band1 <- tmp0$band1*30.48
tmp0$band1[tmp0$band1<0] <- 0

tmp1 <- raster(paste0(t.dir, "/T442/BDRICM_1kmo_T442.tif"))
tmp2 <- raster(paste0(t.dir, "/T478/BDRICM_1kmo_T478.tif"))
tmp1 <- mosaic(tmp1, tmp2, fun = min)
tmp1 <- as(tmp1, "SpatialGridDataFrame")
writeGDAL(tmp1, fname = "tmp1.tif", drivername = "GTIFF")
src_d <- paste0("./tmp1.tif")
gdalwarp(src_d, dstfile="./tmp2.tif", te = as.vector(tmp0@bbox),
    overwrite=TRUE,verbose=TRUE)
tmp1 <- readGDAL("./tmp2.tif")
tmp1$band1 <- expm1(tmp1$band1)###############
st[[2]] <- comparedepth(tmp0, tmp1, tname)
tmp0$dtb <- tmp0$band1
kmldtb(tmp0,tname,0)

#australia: T210, 211, 212, 213, 214,  246, 247, 248, 249, 250, 281, 282, 283, 284, 285, 286, meter
i<-3
setwd(paste0(w.dir, "/Astralia"))
tname <- "AS_dtb"
tmp0 <- readGDAL("./DER_1km.tif")
tmp0$band1 <- tmp0$band1*100

ts <- c(210, 211, 212, 213, 214,  246, 247, 248, 249, 250, 281, 282, 283, 284, 285, 286)
tmp1 <- list(NULL)
for(i in 1:length(ts))
{
    tmp1[[i]] <- raster(paste0(t.dir, "/T", ts[i], "/BDRICM_1kmo_T", ts[i], ".tif"))
}

tmp1 <- do.call(merge, tmp1)
tmp1 <- as(tmp1, "SpatialGridDataFrame")
writeGDAL(tmp1, fname = "tmp1.tif", drivername = "GTIFF")

src_d <- paste0("./tmp1.tif")
gdalwarp(src_d, dstfile="./tmp2.tif", te = as.vector(tmp0@bbox),
    overwrite=TRUE,verbose=TRUE)
tmp1 <- readGDAL("./tmp2.tif")

st[[3]] <- comparedepth(tmp0, tmp1, tname)
tmp0$dtb <- tmp0$band1
kmldtb(tmp0,tname,0)

save.image(paste0(w.dir, "/compare2.rdata"))


w.dir <- "/home/shang009/big/soildepth/surficial"
setwd(w.dir)
load(paste0(w.dir, "/compare2.rdata"))
st[[1]]
st[[2]]


#other2: T475 : not comparable
#i<-3
#setwd(paste0(w.dir, "/other"))
#tname <- "Other2_dtb"
#src_d <- paste0(w.dir, "/other/dep")
#gdalwarp(src_d, dstfile="./tmp0.tif", tr = c(0.008333333, 0.008333333),
#	r = "average", dstnodata = NA,
#    t_srs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
#    overwrite=TRUE,verbose=TRUE)
#tmp0 <- readGDAL("./tmp0.tif")
#tmp0$band1 <- tmp0$band1*30.48
#
#
#src_d <- paste0(t.dir, "/T475/BDRICM_1kmo_T475.tif")
#gdalwarp(src_d, dstfile="./tmp1.tif", te = as.vector(tmp0@bbox),
#    overwrite=TRUE,verbose=TRUE)
#tmp1 <- readGDAL("./tmp1.tif")
#tmp1$band1 <- expm1(tmp1$band1)
#st[[i]] <- comparedepth(tmp0, tmp1, tname)
