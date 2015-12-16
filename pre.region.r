# title         : prediction.r
# purpose       : predict the soil depth, called by prediction.n.r;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : WorldGrids layers, prediction models
# outputs       :  pedicted tif files;
# remarks 1     : Takes ca  1-3 min for 1 tile, ca 6-18min for en.num == 1:10;

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

  out1 <- set.file.extension(gsub(pattern="grid1km", replacement=paste(
    "BDRICM_1km_", n.name, "p", PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", max(en.num) ,sep=""), x=rda.lst[i]), ".tif")
   if( !file.exists(out1)){ 
  #if(1){  
    load(rda.lst[i])
  }
 
  if(!file.exists(out1)){
  
    dm <- 0
    for(j in en.num)
    {
        load(paste0("./model/m_BDRICM_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", j, ".rda"))
        cnames <- dimnames(m_BDRICM[["importance"]])[[1]]
        try( dm <- dm + predict(m_BDRICM, m@data[,cnames], na.action=na.pass) )
    }
      if(!class(.Last.value)[1]=="try-error"){
        m@data[,"BDRICM"] <- as.integer(expm1(dm/length(en.num)))
        writeGDAL(m["BDRICM"], out1, type = "Int32")
        m$BDRICM<- NULL
        rm(m_BDRICM,dm)
        gc()
    }else{
      warning(paste("Failed to compute 'BDRICM' for tile": i))
    }
  }
 
}

if(tilestest == "all")
{
    nRuns <- 1:length(rda.lst)
}else nRuns <- sapply (tilestest, function(x) grep(pattern = x, rda.lst))
sfInit(parallel=TRUE, cpus = 6, slaveOutfile="~/errorwg.log")
sfLibrary(rgdal)
sfLibrary(randomForest)
sfLibrary(randomForestSRC)
sfLibrary(sp)
sfLibrary(RSAGA)
sfExport("rda.lst", "PC.flag", "en.num", "fit.name", "arti.flag", "soil.flag", "fit.name", "n.name")
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
    
    ####inspect output  
    for(i in tilestest)
    {
        dir.create(paste0(outkml, "/",i), showWarnings = TRUE, recursive = FALSE, mode = "0777")
        fname <- paste0("BDRICM_1km_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", max(en.num), flag, "_", i)
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

}
