# title         : prediction.r
# purpose       : predict the soil depth, called by prediction.n.r;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : WorldGrids layers, prediction models
# outputs       :  pedicted tif files;
# remarks 1     : Takes ca  1 min for 1 tile;

load(paste0(a.dir, "/worldgrids/worldgrids.spc.rda"))
if(PC.flag == 1)
{   
    pr.lst <- paste0("PC", 1:(length(worldgrids.spc$center)-2))
    rda.lst <- list.files(pattern=glob2rx("grid1km_T*.rda"), path = w.dir, recursive=TRUE, full.names=TRUE)
}else
{
    pr.lst <- dimnames(worldgrids.spc$rotation)[[1]]
    rda.lst <- list.files(pattern=glob2rx("grid1kmo_T*.rda"), path = w.dir, recursive=TRUE, full.names=TRUE)
}



#i<- 214 #test
################################## predict by tiles
wrapper.predict_BDR <- function(i){

  out0 <- set.file.extension(gsub(pattern="grid1km", replacement=paste(
    "BDRLOG_1km_p", PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", max(en.num) ,sep=""), x=rda.lst[i]), ".tif")
  out1 <- set.file.extension(gsub(pattern="grid1km", replacement=paste(
    "BDRICM_1km_p", PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", max(en.num) ,sep=""), x=rda.lst[i]), ".tif")
  out2 <- set.file.extension(gsub(pattern="grid1km", replacement=paste(
    "SAPICM_1km_p", PC.flag, sep=""), x=rda.lst[i]), ".tif")
  if(!file.exists(out0) | !file.exists(out1)|!file.exists(out2)){ 
  #if(1){  
    load(rda.lst[i])
  }
  if(!file.exists(out0)){
  # if(1){
    xm <- 0
    for(j in 1 : en.num)
    {
        load(paste0("./model/m_BDRLOG_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", j,".rda")) 
        cnames <- dimnames(m_BDRLOG[["importance"]])[[1]]      
        try(xm <- xm +(predict(m_BDRLOG, m@data[,cnames], na.action=na.pass, type = "vote")[,2]) )
    }
    if(!class(.Last.value)[1]=="try-error"){
      m@data[,"BDRLOG"] <- as.integer(xm / en.num * 100)
      writeGDAL(m["BDRLOG"], out0, type="Byte", mvFlag=255)
      rm(m_BDRLOG)
      gc()
    }else {
      warning(paste("Failed to compute 'BDRLOG' for tile": i))
    }
  }
  if(!file.exists(out1)){
  #if(1){
    dm <- 0
    for(j in 1 : en.num)
    {
        load(paste0("./model/m_BDRICM_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", j, ".rda"))
        cnames <- dimnames(m_BDRICM[["importance"]])[[1]]
        try( dm <- dm + predict(m_BDRICM, m@data[,cnames], na.action=na.pass) )
    }
      if(!class(.Last.value)[1]=="try-error"){
        m@data[,"BDRICM"] <- as.integer(expm1(dm/en.num))
        writeGDAL(m["BDRICM"], out1, type = "Int32")
        rm(m_BDRICM)
        gc()
    }else{
      warning(paste("Failed to compute 'BDRICM' for tile": i))
    }
  }
  if(!file.exists(out2)){
  #if(1){
    load(paste0("./model/m_SAPICM2_p",PC.flag, ".rda"))
    cnames <- dimnames(m_SAPICM2[["importance"]])[[1]]
    try( sm <- predict(m_SAPICM2, m@data[,cnames]))
    if(!class(.Last.value)[1]=="try-error"){
        m@data[,"SAPICM2"] <- expm1(sm)
        writeGDAL(m["SAPICM2"], out2, type="Int16", mvFlag = -32768)
        gc()
    }else {
      warning(paste("Failed to compute 'SAPICM' for tile": i))
    }
  }
}

if(tilestest == "all")
{
    nRuns <- 1:length(rda.lst)
}else nRuns <- sapply (tilestest, function(x) grep(pattern = x, rda.lst))
sfInit(parallel=TRUE, cpus = 10, slaveOutfile="~/errorwg.log")
sfLibrary(rgdal)
sfLibrary(randomForest)
sfLibrary(randomForestSRC)
sfLibrary(sp)
sfLibrary(RSAGA)
sfExport("rda.lst", "PC.flag", "en.num", "fit.name", "arti.flag", "soil.flag", "fit.name")
ptm <- proc.time()
sfClusterApplyLB(nRuns, wrapper.predict_BDR)
proc.time() - ptm
# 4 min for 1 tile
sfStop()

###prediction
#PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
#arti.flag <- 1  #1: add artificial points; 0: not
#fit.name <- "all" # c("eu", "as", "us", "ca", "all")
#soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
#en.num <- 1



if(kml.flag > 0)
{
    outkml <- paste0("./outkml", PC.flag)
    if(PC.flag ==0)
    { 
        covkml <- "./covkml" 
        flag  <- "o"  
    } else {
        covkml <- "./pckml"
        flag   <- ""
    }
    ####inspect output  
    for(i in tilestest)
    {
        dir.create(paste0(outkml, "/",i), showWarnings = TRUE, recursive = FALSE, mode = "0777")
        fname <- paste0("BDRICM_1km_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", max(en.num), flag, "_", i)
        out1 <- paste0("../worldgrids/", i, "/", fname, ".tif")
        BDRkml <- readGDAL(out1)
        proj4string(BDRkml) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        
        kml(BDRkml, colour=log1p(band1), raster_name=paste0(fname, ".png"),
            z.lim = c(3,10),
            colour_scale = SAGA_pal[[1]],
            folder.name =  paste0(fname,".kml" ),
            file.name   = paste0(fname,".kml"))   
        flist <- list.files(pattern = "_")
        file.copy(flist, paste0(outkml, "/",i))
        file.remove(flist)
        #system(paste0("tar -zcvf ", paste0(outkml, "/",i), ".tar.gz ", paste0(outkml, "/",i)))
    }

    ####inspect covariates
     cov.lst <- list.files(pattern=glob2rx("*.kml"), path = covkml, recursive=TRUE, full.names=TRUE)
    load(paste0("./model/m_BDRICM_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_1.rda"))
    #only model used covariates
    cnames <- dimnames(m_BDRICM[["importance"]])[[1]]
    rm(m_BDRICM)
    for(i in tilestest)
    {
        tmp <- paste0("../worldgrids/", i, "/grid1km", flag , "_", i, ".rda")
        load(tmp)
        for(j in cnames)
        {   fname <- paste0(names(m[j]),"_T", i)
            if(length(grep(pattern=fname,cov.lst ))==0  )
            {
                kml(m[j], raster_name=paste0(fname, ".png"),
                    colour_scale=SAGA_pal[[1]],
                    folder.name =   paste0(fname, ".kml" ),
                    file.name = paste0(fname, ".kml"))
            }
        }
        dir.create(paste0(covkml, "/T",i), showWarnings = TRUE, recursive = FALSE, mode = "0777")
        flist <-  list.files(pattern = paste0("_T", i))
        file.copy(flist, paste0(covkml, "/T", i))
        file.remove(flist)
        system(paste0("tar -zcvf ", paste0(covkml, "/T", i), ".tar.gz ", paste0(covkml, "/T", i)))
    }

}