#compare with shaple file


rm(list = ls(all = TRUE))
library(rgdal)
library(plotKML)
library(gdalUtils)
library(grDevices)
library(maptools)
w.dir <- "E:/data/soildata/depth"
dir_f <- "E:\\data\\soildata\\depth\\points\\codegsifb\\head\\"
source(paste(dir_f, "functions.r", sep = ""))

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


#Arizona: T439, T440
setwd(paste0(w.dir, "/points/states/Arizona"))
do_unzip()
tname <- "Arizona_dtb"
tmp <- readOGR("./tmp/DGM-52(DepthToBedrock)v.1.0/Shapefiles", "DTB_Contours")
tmp$dtb <- as.integer(as.character(tmp$Label))*30.48
tmp <- subset(tmp, tmp$dtb > 0)
i<-1
kmldtb(tmp,tname,i)
del_unzip()
# very high value, based on ca 470 points, this caused the high prediciton here


#Colorado: T440, T476
setwd(paste0(w.dir, "/states/colorado/LostCreek"))
tmp <- readGDAL("ds507_regthk")
tname <- "Colorado_dtb"
i<-1
tmp$dtb <- tmp$band1*30.48
kmldtb(tmp,tname,i)
# pattern looks like lithology



setwd(paste0(w.dir, "/states/colorado/ColoradoFrontRange"))
ogr2ogr("aaa", "bbb")
tmp <- readOGR("./bbb","ARC")
tmp$dtb <- tmp$CONTOUR * 30.48
i <- 2
kmldtb(tmp,tname,i)
#pattern looks like DEM,lithology,EVHMOD

# both above have not been captured by the prediction well, but the prediction is much higher
# however, covs with high importance do not have the pattern, such as TNSMOD, PX1-4WCL, LAHMOD


#Illinois: T441,442,477,478, no point data from this state
setwd(paste0(w.dir, "/states/illinois"))
tname <- "Illinois_dtb"
tmp <- readOGR("./IL_Drift_Thickness","IL_Drift_Thickness_Ln")
tmp <- subset(tmp, tmp$CONTOUR <999)
tmp$dtb <- tmp$CONTOUR *30.48
i <- 1
kmldtb(tmp,tname,i)
# do not match in the northwest, pattern with covs not very clear


#Indiana: T442, 478
setwd(paste0(w.dir, "/states/Indiana"))
tname <- "Indiana"
tmp <- readOGR("./Surficial_Unconsolidated_Thickness","unconsol_th_mm37_in")
tmp$dtb <- (tmp$MIN_THICK + tmp$MAX_THICK)/2 *30.48
i <- 1
kmldtb(tmp,tname,i)
# more extrem values than the prediction.



#iowa:T477
setwd(paste0(w.dir, "/states/iowa"))
unzip("depth_to_bedrock.zip", exdir = "tmp")
tname <- "Iowa_dtb"
for(i in 7:10)
{
    tmp <- readGDAL("./tmp/depth_to_bedrock.img", offset =c(320*(i-1),0), region.dim = c(320, 4868))
    tmp$dtb <- tmp$band1*30.48
    kmldtb(tmp,tname,i)
}
del_unzip()
# more extrem values than the prediction. similar pattern



#Minnesota: T477, just interpolation, do not use
setwd(paste0(w.dir, "/points/states/Minnesota/dtb"))
do_unzip()
tname <- "Minnesota_dtb"
tmp <- readGDAL("./tmp/OFR06_02/dt_loc")
i<-1
tmp$dtb <- tmp$band1*30.48
kmldtb(tmp,tname,i)
del_unzip()
# more extrem values than the prediction. similar pattern


#Missouri:
setwd(paste0(w.dir, "/states/missouri/"))
unzip("MO_2014_Overburden_Thickness_shp.zip", exdir = "tmp")
tname <- "Missouri_dtb"
tmp <- readOGR("./tmp","MO_2014_Overburden_Thickness_shp")
i<-1
tmp$dtb <- tmp$CONTOUR*30.48
kmldtb(tmp,tname,i)
del_unzip()
# quite close


#Ohio: T442, T478
setwd(paste0(w.dir, "/states/ohio/processed"))
tname <- "Ohio_dtb"
for(i in 1:13)
{
    tmp <- readGDAL("depthb", offset =c(0,(i-1)*472), region.dim = c(6646, 472))
    tmp$dtb <- tmp$band1*30.48
    kmldtb(tmp,tname,i)
}
#more extrem value than the prediction, and pattern not same
# some problem in projection???


#other2: T475!!!!!!! to be compared
setwd(paste0(w.dir, "/states/other/2"))
tname <- "Other2_dtb"
for(i in 1:2)
{
    tmp <- readGDAL("dep", offset = c((i-1)*1479, 0), region.dim = c(1479, 3151))
    tmp$dtb <- tmp$band1 * 30.48
    kmldtb(tmp,tname,i)
}
# pattern differs much
