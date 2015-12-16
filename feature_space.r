# title         : feature_space.r
# purpose       : calculates the distance in the feature space of all geografic points;
# reference     : Hidden extrapolation: (Montgomery, Peck, & Vining, 2001; page 109), and Brus, 2002
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : pedicted tif by tiles 
# outputs       :  pedicted tif files;
# remarks 1     : Takes ca 1 hour for global ;
# remarks 2     : for a nice graph, the log of the normalized distance is calculated

rm(list = ls(all = TRUE))
library(snowfall)

PC.flag <- 0 # wheather use the PC as predictors
a.dir <- "/home/shang009/big/"# dir of the project
w.dir <- paste0(a.dir, "/worldgrids")
m.dir <- paste0(a.dir, "/soildepth")
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
setwd(m.dir)


fit.name <- "all" # c("all", "us", "eu", "as")
load(paste0("./profs/subs/subs.sp_", PC.flag, "_", fit.name,".rda"))
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", fit.name,".rda"))
###only for continuous variables
vs_covars = pr.lst[1:19]
tmp <-subs.sp[[1]]@data
#tmp<- subset(tmp, runif(dim(tmp)[1])>0.999)
tmp <- get_h_max(tmp, vs_covars)
hmax <- tmp$h_max
hmax
#0.02501247 max
mn_XXinv <- tmp$mn_XXinv

#####check the validation subset's feature space
val.p <- subset(suba.sp, !(suba.sp$SOURCEID %in% subs.sp[[1]]$SOURCEID) )
obj<-subset(val.p@data, runif(length(val.p))>0.99999)
tmp2 <- check_feature_space(val.p@data, vs_covars, hmax, mn_XXinv)

val.p$log_h <- tmp2$log_h
val.p$mask <- tmp2$mask
print(tmp2$frac)
#0
rm(subs.sp, suba.sp)




fit.name <- "us" # c("all", "us", "eu", "as")
load(paste0("./profs/subs/subs.sp_", PC.flag, "_", fit.name,".rda"))
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", fit.name,".rda"))
###only for continuous variables
vs_covars = pr.lst[1:19]
#tmp <- get_h_max(subs.sp[[1]]@data, vs_covars)
tmp <- get_h_max(subs.sp[[1]]@data, vs_covars)
hmax <- tmp$h_max
hmax
#0.02007895 max
mn_XXinv <- tmp$mn_XXinv

#####check the validation subset's feature space
val.p <- subset(suba.sp, !(suba.sp$SOURCEID %in% subs.sp[[1]]$SOURCEID) )
tmp2 <- check_feature_space(val.p@data, vs_covars, hmax, mn_XXinv)
#tmp2 <- check_feature_space(val.p@data[1,], vs_covars, hmax, mn_XXinv)
#val.p$log_h <- tmp2$log_h
#val.p$mask <- tmp2$mask
print(tmp2$frac)
#0
fit.name <- "all" # c("all", "us", "eu", "as")
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", fit.name,".rda"))
val.p <- subset(suba.sp, !(suba.sp$SOURCEID %in% subs.sp[[1]]$SOURCEID) )
tmp2 <- check_feature_space(val.p@data, vs_covars, hmax, mn_XXinv)
print(tmp2$frac)
#0.004721209
rm(subs.sp, suba.sp)
#####check the prediction's feature space


wrapper.featurespace <- function(i){
  out1 <- set.file.extension(gsub(pattern="grid1km", replacement=paste("Featurespace_1km" ,sep=""), x=rda.lst[i]), ".tif")
  #if(!file.exists(out1)){
  if(1){
    load(rda.lst[i])
    tmp2 <- check_feature_space(m@data, vs_covars, hmax, mn_XXinv)
    m$mask <- tmp2$mask
    writeGDAL(m["mask"], out1, type="BYTE", mvFlag = 255)
  }
  return(c(i, tmp2$n_in, tmp2$n_all))
}

#tilestest <- 1:2
nRuns <- 1:length(rda.lst)
#nRuns <- tilestest
sfInit(parallel=TRUE, cpus = 10, slaveOutfile="~/errorwg.log")
sfLibrary(rgdal)
sfLibrary(RSAGA)
rda.lst <- normalizePath(rda.lst)
print(rda.lst[1])
sfExport("rda.lst", "PC.flag", "vs_covars", "hmax", "mn_XXinv", "check_feature_space", "get_h_0", "m.dir")
ptm <- proc.time()
n_in_all <- sfClusterApplyLB(nRuns, wrapper.featurespace)
proc.time() - ptm
# 1 min for 1 tile with full coverage, 0.3  hour
sfStop()
n_df <- do.call("rbind", n_in_all)
n_df <- cbind(n_df, n_df[,2]/n_df[,3])
rda.lst[sort(subset(n_df[order(n_df[,4]),1], n_df[,4]<0.8))]
print(round(1- sum(n_df[,2])/ sum(n_df[,3]),4))
#0.1552 if not
#0.00088 if takes artiticial points from the world
save.image("featurespace.rdata")
