# title         : ziptiles.r
# purpose       : make a whole tif of prediction and zip all tiles into one folder;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : pedicted tif by tiles 
# outputs       :  pedicted tif files;
# remarks 1     : Takes ca 4.5 hour for global ;
rm(list = ls(all = TRUE))
library(RSAGA)
library(gdalUtils)
gdal.dir <-  "/usr/local/bin"
gdal_setInstallation(search_path=gdal.dir, rescan=TRUE)
a.dir <- "/data/shang009/big/"# dir of the project
w.dir <- paste0(a.dir, "/worldgrids")
m.dir <- paste0(a.dir, "/soildepth")
outdir <- (paste0(a.dir, "/soildepth/tiled"))
te <- c(-180, -60, 180, 90)
setwd(m.dir)

PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 1  #1: add artificial points; 0: not
fit.name <- "us" # c("eu", "as", "us", "ca", "all")
soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
en.num <- 1
##----------------------------------
## create mosaics and export to Tiff:
## generate all output file names:

tifout.lst <- c("BDRICM", "SAPICM2" , "BDRLOG", "Featurespace")
k <- 1
#for(k in 1:length(tifout.lst)){
for(k in 1:1){
  if(tifout.lst[k] == "SAPICM2")
  {
    tmp.lst <- list.files(path=w.dir, pattern=glob2rx(paste0(
        "SAPICM_1km_p", PC.flag, "*.tif$")), recursive = TRUE, full.names = TRUE)
  }else{
  tmp.lst <- list.files(path=w.dir, pattern=glob2rx(paste0(
    tifout.lst[k],"_1km_p", PC.flag, "_a", arti.flag, "_s", soil.flag, "_", 
    fit.name, "_", max(en.num), "*.tif$")), recursive = TRUE, full.names = TRUE)
  }
  ## if is missing 2 or more tiles, then stop!
  #which(!( sapply(rda.lst, function(x){strsplit(x, "/")[[1]][2]}) %in% sapply(tmp.lst, function(x){strsplit(x, "/")[[1]][4]}) ))
  #if(length(tmp.lst)==length(rda.lst)|length(tmp.lst)==(length(rda.lst)-1)){
    outn = paste(tifout.lst[k], "_", "17_Sep_2015", sep="")  ## format(Sys.Date(), "%d_%b_%Y")
    if(!file.exists(set.file.extension(outn, ".tif.gz"))){
      #gdalbuildvrt(tmp.lst, output.vrt = paste(tifout.lst[k], ".vrt"))
      ## resample:
      unlink(set.file.extension(outn, ".tif"))
      variable <- strsplit(tifout.lst[k], "_")[[1]][1]
      if(variable %in% c("BDRLOG", "Featurespace")){ # 50 mins
          system(paste0("gdalwarp -ot Byte  -srcnodata 255 -dstnodata 255 -r near -te ",
             do.call(paste, as.list(te))," -tr 0.00833333333333333 0.00833333333333333 ",
             "-multi -wo NUM_THREADS=11 ",
             do.call(paste, as.list(tmp.lst))," ", set.file.extension(outn, ".tif")))
      }else if(variable == "SAPICM"){# 1.5 hour
        system(paste0("gdalwarp -ot Int16 -wt Int16 -srcnodata -2147483648 -dstnodata -32768 -r near -te ",
             do.call(paste, as.list(te))," -tr 0.00833333333333333 0.00833333333333333 ", 
             "-multi -wo NUM_THREADS=11 ",
             do.call(paste, as.list(tmp.lst))," ", set.file.extension(outn, ".tif")))
#          gdalwarp(tmp.lst, set.file.extension(outn, ".tif"), ot = "Int16",
#            srcnodata = "-", dstnodata = "-32768", r = "near", te = te, tr = c(1/120, 1/120))
      }else if(variable == "BDRICM"){# Int 16 will set the biggest value as 32768 # 2.2 hour
        system(paste0("gdalwarp -ot Int16 -wt Int32 -srcnodata -2147483648 -dstnodata -32768 -r near -te ",
             do.call(paste, as.list(te))," -tr 0.00833333333333333 0.00833333333333333 ",
             "-multi -wo NUM_THREADS=11 ", 
             do.call(paste, as.list(tmp.lst))," ", set.file.extension(outn, ".tif")))   
      }
      system(paste("gzip -k ",  set.file.extension(outn, ".tif")))
      #system(paste("7za a", "-tgzip", set.file.extension(outn, ".tif.gz"), set.file.extension(outn, ".tif")))
      #system(paste("7za a", "-tgzip -mx=9", set.file.extension(outn, ".tif.gz"), set.file.extension(outn, ".tif"))) ## TAKES 5-6 minutes per TIF!!!
      #system(paste("xcopy", set.file.extension(outn, ".tif.gz"), shortPathName(normalizePath(outdir))))
      system(paste("mv",  set.file.extension(outn, ".tif.gz"), outdir))
      #unlink(paste(tifout.lst[k], ".vrt", sep=""))
      #unlink(set.file.extension(outn, ".tif"))
    }
  #} else {
  #    warning(paste("Tiles missing for variable:", tifout.lst[k]))
  #}
}
#
### create directories per soil var:
#tto <- list.dirs("./tiled")
##for(k in 1:177){
#for(k in 1:length(tifout.lst)){
#  if(!any(paste(tto, "/", sep="") %in% paste("../tiled/", tifout.lst[k], "/", sep=""))){
#    dir.create(paste0("./tiled/", tifout.lst[k], "/"), showWarnings = TRUE, recursive = FALSE, mode = "0777")
#  }
#}
#
### compress all individual files to a single dir per soil var:
#
#wrapper.tiled <- function(k){
#  outdir <- paste("./tiled/", tifout.lst[k], "/", sep="")
#  tmp.lst <- list.files(path=w.dir, pattern=glob2rx(paste0(
#    tifout.lst[k],"_1km_p", PC.flag, "_a", arti.flag, "_s", soil.flag, "_", 
#    fit.name, "_", max(en.num), "*.tif$")), recursive = TRUE, full.names = TRUE)
#  if(length(tmp.lst)>0){
#    for(i in 1:length(tmp.lst)){
#      fname = strsplit(tmp.lst[i], "/")[[1]]
#      fname = paste0(strsplit(fname[length(fname)], "\\.")[[1]][1], "_", format(Sys.Date(), "%d_%b_%Y"))
#      outn <- paste0(outdir, set.file.extension(fname, ".tif.gz"))
#      if(!file.exists(outn)){
#        system(paste("gzip -k",  tmp.lst[i]))
#        system(paste0("mv ",  tmp.lst[i], ".gz ", outdir, "/", set.file.extension(fname, ".tif.gz")))
#        #system(paste("7za a", "-tgzip", set.file.extension(fname, ".tif.gz"), tmp.lst[i]))
#        #system(paste("7za a", "-tgzip -mx=9", set.file.extension(fname, ".tif.gz"), tmp.lst[i]))
#        #system(paste("xcopy", shortPathName(normalizePath(set.file.extension(fname, ".tif.gz"))), shortPathName(normalizePath(outdir))))
#        #unlink(set.file.extension(fname, ".tif.gz"))
#      }
#    }
#   }
#}
#sfInit(parallel=TRUE, cpus=8, slaveOutfile="~/errorwg.log")
#sfLibrary(RSAGA)
#sfExport("tifout.lst", "w.dir")
#x <- sfClusterApplyLB(1:length(tifout.lst), wrapper.tiled)
#sfStop()
#gc()
