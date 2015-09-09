# title         : fit_models.R
# purpose       : Fit models for Global Soil depth call by fit.n.r;
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

load(paste0("./profs/subs/subs.sp_", PC.flag, "_", fit.name, ".rda"))
if(soil.flag == 1) 
{
    load(paste0("./profs/subs/subs.sp_", PC.flag, "_soil.rda")) 
    #names(country) from getsubpoints.r
    if(fit.name == "us") soil.sp <- subset(soil.sp, soil.sp$country == 217) else
    if(fit.name == "ca") soil.sp <- subset(soil.sp, soil.sp$country == 38) else
    if(fit.name == "as") soil.sp <- subset(soil.sp, soil.sp$country == 14) else
    if(fit.name == "eu") soil.sp <- subset(soil.sp, soil.sp$country == 96 | soil.sp$country == 202) 
    if(fit.name %in% c("eu", "as", "all") )soil.sp$country <- NULL
    subs.sp <- lapply(subs.sp, function(x) {rbind(x, soil.sp)})      
}
if(arti.flag == 0) subs.sp <- lapply(subs.sp, function(x) {subset(x, x$type == 2)})
subs.sp <- lapply(subs.sp, function(x) {x$BDRLOG <- as.factor(x$BDRICM <= 200); return(x)})

# test run
#subs.sp <- lapply(subs.sp, function(x) {subset(x, runif(length(x))<0.01)})



## fitting the modles
wrapper.fit_BDR <- function(i)
{
    set.seed(100)
    if(length(en.num) == 1) {
        options(rf.cores=detectCores()-1, mc.cores=detectCores()-1)
    }else options(rf.cores=2, mc.cores=2)
    if(!file.exists( paste0("./model/m_BDRICM_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", i, ".rda")))
    {
        formulaString.BDRICM <- as.formula(paste0("log1p(BDRICM) ~ ",  paste(pr.lst, collapse="+")))
        try( vs_BDRICM <- var.select(formulaString.BDRICM , subs.sp[[i]][c(pr.lst, "BDRICM")]@data, 
            ntree =300, seed = -2212, nsplit = 10, refit = refit) )
        formulaString.BDRICM <- as.formula(paste0("log1p(BDRICM) ~ ",  paste(vs_BDRICM$topvars, collapse="+")))
        m_BDRICM <- randomForest(formulaString.BDRICM, subs.sp[[i]][c(vs_BDRICM$topvars, "BDRICM")]@data, na.action = na.omit, ntree =300)
        save(m_BDRICM, vs_BDRICM, file= paste0("./model/m_BDRICM_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", i, ".rda"))   
    }   
    rm(m_BDRICM, vs_BDRICM)
    gc()
    formulaString.BDRLOG <- as.formula(paste0("BDRLOG ~ ",  paste(pr.lst, collapse="+")))
    try( vs_BDRLOG <- var.select(formulaString.BDRLOG , subs.sp[[i]][c(pr.lst, "BDRLOG")]@data, 
        ntree =300, seed = -2212,  nsplit = 10 ,refit = refit) )
    formulaString.BDRLOG <- as.formula(paste0("BDRLOG ~ ",  paste(vs_BDRLOG$topvars, collapse="+")))
    m_BDRLOG <- randomForest(formulaString.BDRLOG, subs.sp[[i]][c(vs_BDRLOG$topvars, "BDRLOG")]@data, na.action = na.omit, ntree =300)
    save(m_BDRLOG, vs_BDRLOG, file=paste0("./model/m_BDRLOG_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", i,".rda"))
    rm(m_BDRLOG, vs_BDRLOG)
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
sfExport( "subs.sp", "pr.lst",  "PC.flag", "arti.flag", "soil.flag", "fit.name", "en.num" ,"refit")
ptm <- proc.time()
x <- sfClusterApplyLB(nRuns, wrapper.fit_BDR)
proc.time() - ptm
sfStop()




#for us: When cellsize = 0.1, n = 3,  55725+5572  (PC.flag = 1): 1.5 hours and  55725+5572 points(PC.flag = 0)
##########ntree=300, cellsize = 0.1, n = 3 is enough for test 
###0.5 hours if using indicator and interger, 1 hour if using factor
#####m_BDRICM_0 with fill and artificial points
#Var explained: 59.91
#cellsize = 0.1, n = 3
#test: ntree = 300, r2 ==57.72%
#test_500: ntree = 500,r2 == 57.36%
#cellsize = 0.1, n = 2
#test_500: ntree = 500,r2 == 56.4%
#cellsize = 0.1, n = 1
#test2: ntree =300,r2 == 46.22;
#test2_500: ntree = 500, r2 ==46.94

######glm of BDRLOG
#   load(paste0("ov.lst_", PC.flag,".rda"))
#    m_BDRLOG2 <- glm(as.formula(paste("BDRLOG ~", paste(pr.lst, collapse="+"))), data=ov.lst[["BDRICM"]], family=binomial(link = "logit"))
#    #takes ca  52 (31)sec to fit with  1,238,855points
#    summary(m_BDRLOG2)
#    signif(100-summary(m_BDRLOG2)$deviance/summary(m_BDRLOG2)$null.deviance*100, 3)
#    if (PC.flag == 1) {save(m_BDRLOG2, file="m_BDRLOG2.rda")
#    }else save(m_BDRLOG2, file="mo_BDRLOG2.rda")


#### fit variogram of residual of rf, pure nugget, no need to do it
#load(paste0("subs.sp_", split.flag, "_", PC.flag, fit.name, ".rda"))
#i <- 1
#if(art.flag == 1)
#{
#    load(paste0("subs.sp.at_", split.flag, "_", PC.flag, fit.name, ".rda"))
#    subs.sp[[i]]<- rbind(subs.sp[[i]], subs.sp.at[[i]])
#}
#load(paste0("m_BDRICM_", PC.flag, "_", i, ".rda"))
#d <- subs.sp[[i]]@data[c("LONWGS84", "LATWGS84")]
#d$resid.rf <- m_BDRICM$predicted - m_BDRICM$y
#EDA(d$resid.rf)
#dev.copy(png,paste0("./pics/EDAresid.rf.png"),  width = 780, height = 480, units = "px")
#dev.off()
#plot(m_BDRICM$y, d$resid.rf, xlab = "Fitted (%)", ylab = "Residual (%)")
#abline(0,0)
#dev.copy(png, paste0("./pics/resid.y.png"),  width = 780, height = 480, units = "px")
#dev.off()
#plot(m_BDRICM$predicted, d$resid.rf, xlab = "Fitted (%)", ylab = "Residual (%)")
#abline(0,0)
#dev.copy(png, paste0("./pics/resid.pre.png"),  width = 780, height = 480, units = "px")
#dev.off()
#coordinates(d) <- ~LONWGS84+LATWGS84
## set coordinate projection
#proj4string(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#merc.csy <- "+proj=merc +lat_ts=0.0 +lon_0=0.0 +k=1.0 +x_0=0.0 +y_0=0.0 +a=6378137.0 +b=6378137.0 +units=m +nadgrids=@null +no_defs"
#d <- spTransform(d, CRS(merc.csy))
## compute sample variogram
#v.rf <- variogram(d$resid.rf~1, data = d, boundaries=c(5000,1e4,2e4,5e4,1e5,2e5))
#plot(v.rf)
#dev.copy(png, paste0("./pics/variogram.png"),  width = 780, height = 480, units = "px")
#dev.off()
# define variogram model: initial values for psill, range, nugget based on sample variogram
#vm <- vgm(psill = 1e6,  range = 6e6, nugget = 2e6, model = "Gau")
## fit variogram model to the experimental variogram
#vmf.rf <- fit.variogram(v.rf, model = vm)
#plot(v.rf,vmf.rf)
#vmf.rf


####################model for SAPICM
#### need to take -999 as null ??????
#make a subset
#load(paste0("ov.lst_", PC.flag,".rda"))
#psub <- ov.lst[[c("SAPICM")]]
#psub <- psub[psub$SAPICM >= 0,]
#sub.sp.sap <- psub
#sub.sp.sap$x <- sub.sp.sap$LONWGS84
#sub.sp.sap$y <- sub.sp.sap$LATWGS84
#coordinates(sub.sp.sap) <- ~ x +y
#proj4string(sub.sp.sap) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
######adding artificial points!!!!!!!!!!!!!!!!!!!!!
###load(paste0("sub.sp.at_", split.flag, "_", PC.flag, ".rda"))
###sub.sp.at$SAPICM <- 0
###sub.sp.at$BDRLOG <- NULL
###sub.sp.sap <- rbind(sub.sp.sap[names(sub.sp.at)], sub.sp.at)
#save(sub.sp.sap, file = paste0("sub.sp.sap_", PC.flag, ".rda"))
#formulaString.SAPICM <- as.formula(paste('log1p(SAPICM) ~ ', paste(pr.lst, collapse="+")))
#ptm <- proc.time()
#m_SAPICM <- randomForest(formulaString.SAPICM, data=sub.sp.sap@data, na.action=na.omit, importance=TRUE)
#proc.time() - ptm
#varImpPlot(m_SAPICM)
#save(m_SAPICM, file=paste0("m_SAPICM_",PC.flag, ".rda"))  
#m_SAPICM
#% Var explained: 48.56
#rm(psub)
#rm(ov.lst)

