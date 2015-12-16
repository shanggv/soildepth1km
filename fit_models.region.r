# title         : fit_models.region.R
# purpose       : Fit models for regional Soil depth call by fit.n.region.r;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : WorldGrids layers, SoilProfiles.org points, water well points from various sources
# outputs       : prediction models ;
# remarks 1     : Takes ca ??? mins to run with 10 cpus in use;

load(paste0(a.dir, "/worldgrids/worldgrids.spc.rda"))
if(PC.flag == 1)
{   
    pr.lst <- paste0("PC", 1:(length(worldgrids.spc$center)-2))
}else
{    
    pr.lst <- dimnames(worldgrids.spc$rotation)[[1]]
}

if(fit.name %in% c("eu", "as", "us", "ca"))
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", fit.name, ".rda"))
}else if (fit.name == "na")
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "us", ".rda"))
    tmp <- suba.sp
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "ca", ".rda"))
    suba.sp <- rbind(tmp, suba.sp)    
}else if (fit.name == "se")
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "eu", ".rda"))
    suba.sp <- subset(suba.sp,  suba.sp$LONWGS84 > 0)
}else if (fit.name == "ie")
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "eu", ".rda"))
    suba.sp <- subset(suba.sp,  suba.sp$LONWGS84 < 0)  
}else if (fit.name %in% us.code)
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "us", ".rda"))
    suba.sp <- subset(suba.sp, suba.sp$source==as.integer(substr(fit.name, 3,5))) 
    #a bug for subset, the following does not work
#    source <- as.integer(substr(fit.name, 3,5))
#    suba.sp <- subset(suba.sp, subset = c(suba.sp$source == as.integer(source)))
       
}else if (fit.name %in% ca.code)
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "ca", ".rda"))
    suba.sp <- subset(suba.sp, suba.sp$source==as.integer(substr(fit.name, 3,5))) 
}else if (fit.name=="usm")
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "us", ".rda"))
    suba.sp <- subset(suba.sp, suba.sp$source!=17) 
}else if (fit.name=="cam")
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "ca", ".rda"))
    suba.sp <- subset(suba.sp, suba.sp$source!=3) 
}else if (fit.name %in% mus.code)
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "us", ".rda"))
    suba.sp <- subset(suba.sp, suba.sp$source!=as.integer(substr(fit.name, 4,6))) 

       
}else if (fit.name %in% mca.code)
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "ca", ".rda"))
    suba.sp <- subset(suba.sp, suba.sp$source!=as.integer(substr(fit.name, 4,6))) 
}

if(arti.flag == 0) suba.sp <- subset(suba.sp,  suba.sp$type == 2)
if(length(suba.sp)<10000)
{
  subs.sp <- subset(suba.sp, runif(length(suba.sp))>0.3)  
}else subs.sp <- subsp(suba.sp, cellsize = 0.1, n = celln)

tmp<- subset(suba.sp,  !(suba.sp$SOURCEID %in% subs.sp$SOURCEID) )


save(subs.sp, file=paste0("./profs/subs/subs.sp_", n.name, "p", PC.flag, "_a",arti.flag, "_", fit.name, ".rda")) 

#test run
#subs.sp <- subset(subs.sp, runif(length(subs.sp))<0.01)

tmp <- list(NULL)
tmp[[1]] <- subs.sp 
subs.sp <- tmp
rm(tmp)

## fitting the modles
wrapper.fit_BDR <- function(i)
{
    set.seed(100)
    if(length(en.num) == 1) {
        options(rf.cores=detectCores()-1, mc.cores=detectCores()-1)
    }else options(rf.cores=2, mc.cores=2)
    if(!file.exists( paste0("./model/m_BDRICM_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", i, ".rda")))
    {
        formulaString.BDRICM <- as.formula(paste0("log1p(BDRICM) ~ ",  paste(pr.lst, collapse="+")))
        try( vs_BDRICM <- var.select(formulaString.BDRICM , subs.sp[[i]][c(pr.lst, "BDRICM")]@data, 
            ntree =300, seed = -2212, nsplit = 10, refit = refit) )
        formulaString.BDRICM <- as.formula(paste0("log1p(BDRICM) ~ ",  paste(vs_BDRICM$topvars, collapse="+")))
        m_BDRICM <- randomForest(formulaString.BDRICM, subs.sp[[i]][c(vs_BDRICM$topvars, "BDRICM")]@data, na.action = na.omit, ntree =300)
        save(m_BDRICM, vs_BDRICM, file= paste0("./model/m_BDRICM_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", i, ".rda"))   
    }   
    rm(m_BDRICM, vs_BDRICM)
    gc()      
}

nRuns <- en.num
nSlaves <- 5
if(length(en.num) >1) {
    sfInit(parallel=TRUE, cpus = nSlaves, slaveOutfile="~/errorwg.log")
} else{    
    sfInit(parallel=FALSE, slaveOutfile="~/errorwg.log")    
}

sfLibrary(sp)
sfLibrary(raster)
sfLibrary(randomForest)
sfLibrary(randomForestSRC)
sfExport( "subs.sp", "pr.lst",  "PC.flag", "arti.flag", "soil.flag", "fit.name", "en.num" ,"refit","n.name")
ptm <- proc.time()
x <- sfClusterApplyLB(nRuns, wrapper.fit_BDR)
proc.time() - ptm
sfStop()


