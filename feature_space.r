####
# calculates the distance in the feature space of all geografic points
# see Hidden extrapolation: (Montgomery, Peck, & Vining, 2001; page 109), and Brus, 2002
# for a nice graph, the log of the normalized distance is calculated
rm(list = ls(all = TRUE))
library(snowfall)

PC.flag <- 0 # wheather use the PC as predictors
fit.name <- "us" # c("all", "us", "eu", "as")
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
load(paste0("sub.sp_", split.flag, "_", PC.flag, ".rda"))
load(paste0("sub.sp.at_", split.flag, "_", PC.flag, ".rda"))

#get h_max
get_h_max <- function(obj, names_cov){
    n_of_covars = length(names_cov)
    n_of_observations = nrow(obj)
    mn_X = as.matrix(obj[,names_cov])
    mn_X = cbind(1,mn_X)
    mn_XXinv <- solve(t(mn_X) %*% mn_X)
    #h_max = max(diag(mn_X %*% mn_XXinv %*% t(mn_X)))  ##takes too much memory if n_of_observations are too big
    # subset the matrix to avoid high consumption of memory, and do the same thing as the above line
    tmp <- mn_X %*% mn_XXinv
    tmp2 <-  t(mn_X)
    len_sub <- 10000
    n_sub <- ceiling(n_of_observations/len_sub)
    h_max <- - 999
    for(i in 1: n_sub)
    {
        i_start <- (i-1)*len_sub +1
        if(i == n_sub) {
            i_end <- n_of_observations
        }else {
            i_end <- i*len_sub
        }
        tmp3 <- tmp[i_start : i_end,] %*%  tmp2[ ,i_start : i_end]
        tmp4 <- max(diag(tmp3))
        #tmp4 <- sort(diag(tmp3), decreasing = TRUE)[4]
        if(h_max < tmp4) h_max <- tmp4
    }
    ret <- list(h_max = h_max, mn_XXinv = mn_XXinv)
    return(ret)
}

# get log(h_0/h_max)
get_h_0 <- function(obj, h_max, mn_XXinv)
{
  if (!any(is.na(obj)))
    {
      v_x <- as.matrix(obj)
      v_x <- c(1,v_x) # add leading one
      # X_0 in my paper, x_0 in Montgomery
      h_0 <- t(v_x) %*% mn_XXinv %*% (v_x)
      # the distance of the point in feature space to the center of the ellipsoid
      ret <- log(h_0 / h_max)
      # the fraction of the distance of proposed covariate over the maximum distance that followed
      # out of the calibration set
      # to make nice maps, I calculated the logarithm; that's my own addition. So the border is 0
      return(drop(ret))
    }
  else
  {
    return(NA)
  }
}


check_feature_space <- function(obj, names_cov, h_max, mn_XXinv)
{
    n_of_obj <- dim(obj)[1]
    #subset the obj to avoid high consumption of memory
    len_sub <- 100000
    n_sub <- ceiling(n_of_obj/len_sub)
    log_h <- NULL
    for(i in 1:n_sub)
    {
        i_start <- (i-1)*len_sub +1
        if(i == n_sub){
         i_end <- n_of_obj
        }else{
         i_end <- i*len_sub
        }
        log_h[i_start : i_end] <-
            apply(obj[i_start : i_end, names_cov],
            MARGIN=1, get_h_0, h_max = h_max, mn_XXinv = mn_XXinv)
    }
    flag <- which(log_h <= 0)
    mask <- rep(NA, times = length(log_h))
    mask[flag] <- 1
    ###percentage of points which are not in the calibration feature space
    n_of_in <- sum(!is.na(mask))
    frac <- 1 - n_of_in / n_of_obj
    ret <- list(log_h = log_h, mask = mask, frac = frac, n_in = n_of_in, n_all = n_of_obj)
    return(ret)
}

###only for continuous variables
vs_covars = pr.lst[1:19]
tmp <- rbind(subs.sp[[1]]@data, subs.sp.at[[1]]@data)
tmp <- get_h_max(tmp, vs_covars)
hmax <- tmp$h_max
hmax
#0.009990788 max
#0.009987938 2nd max
#0.008368055 3rd max
#0.006742014 4th max
mn_XXinv <- tmp$mn_XXinv

#####check the validation subset's feature space
val.p <- rbind(subset(sub.sp, !(sub.sp$SOURCEID %in% subs.sp[[1]]$SOURCEID) ),
        subset(sub.sp.at, !(sub.sp.at$SOURCEID %in% subs.sp.at[[1]]$SOURCEID)))
#val.p <- subset(val.p, as.integer(val.p$SOURCEID) <1000)
tmp2 <- check_feature_space(val.p@data, vs_covars, hmax, mn_XXinv)
val.p$log_h <- tmp2$log_h
val.p$mask <- tmp2$mask
print(tmp2$frac)
#0.0055
rm(subs.sp, subs.sp.at,sub.sp, sub.sp.at)
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
