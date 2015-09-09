# title         : cross.validation.R
# purpose       : cross validation, call by cv.n.r;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : points with overlayed covariates 
# outputs       : compressed RDA files ;
# remarks 1     : Takes ca 1.2 hrs to run with 5 cpus for randomforest in the defualt setting in use 


## list all tiles and covariates:
load(paste0(a.dir, "/worldgrids/worldgrids.spc.rda"))
if(PC.flag == 1)
{
    pr.lst <- paste0("PC", 1:(length(worldgrids.spc$center)-2))
 }else
{
    pr.lst <- dimnames(worldgrids.spc$rotation)[[1]]
}

##Load regression matrices:
# load BDRICM
load(paste0("./profs/subs/suba.sp_", PC.flag,  "_", fit.name,".rda"))
if(soil.flag == 1) 
{
    load(paste0("./profs/subs/subs.sp_", PC.flag, "_soil.rda")) 
    #names(country) from getsubpoints.r
    if(fit.name == "us") soil.sp <- subset(soil.sp, soil.sp$country == 217) else
    if(fit.name == "ca") soil.sp <- subset(soil.sp, soil.sp$country == 38) else
    if(fit.name == "as") soil.sp <- subset(soil.sp, soil.sp$country == 14) else
    if(fit.name == "eu") soil.sp <- subset(soil.sp, soil.sp$country == 96 | soil.sp$country == 202) 
    if(fit.name == "all" | fit.name == "eu" | fit.name == "as") soil.sp$country <- NULL
    suba.sp <- rbind(suba.sp, soil.sp)     
}
if(arti.flag == 0) suba.sp <- subset(suba.sp, suba.sp$type == 2)
suba.sp$BDRLOG <- as.factor(suba.sp$BDRICM <= 200)

# load SAPICM2
load(paste0("./profs/subs/soil.sp_", PC.flag,".rda"))
soil.sp <- subset(soil.sp, !is.na(soil.sp$SAPICM2))

## Target vars:
tvar.lst <- c("BDRICM", "SAPICM2" , "BDRLOG")

## Summary table:
tbl <- data.frame(ATTRIBUTE_LABEL=tvar.lst, PR_DEPTHS=rep(1, length(tvar.lst)))
tbl$N_OBSERVATIONS <- NA
tbl$OBSERVED_RANGE_MIN <- NA
tbl$OBSERVED_RANGE_MAX <- NA
tbl$rf.SIGMA_EXPLAINED.log <- NA
if(m.flag == 1)
{
    tbl$rk.SIGMA_EXPLAINED.log <- NA
    tbl$gbm.SIGMA_EXPLAINED.log <- NA 
    tbl$var.test.H1 <- NA
    tbl$t.test.H1 <- NA
    tbl$var.test.H2 <- NA
    tbl$t.test.H2 <- NA
}
tbl$rf.ME.log <- NA
tbl$rf.RMSE.log <- NA
if(m.flag == 1)
{
    tbl$rk.ME.log <- NA
    tbl$gbm.ME.log <- NA
    tbl$rk.RMSE.log <- NA
    tbl$gbm.RMSE.log <- NA
}
tbl$rf.ME <- NA
tbl$rf.RMSE <- NA
if(m.flag == 1)
{
    tbl$rk.ME<- NA
    tbl$gbm.ME <- NA
    tbl$rk.RMSE <- NA
    tbl$gbm.RMSE <- NA
}

## Prepare list to store CV output
cv.lst <- as.list(rep(NA, nrow(tbl)))
names(cv.lst) <- tvar.lst

j<-1
## Run cross-validation and var.test and write the results to a table
## Subset to sub.N samples to speed things up!
for(j in 1:length(tbl$ATTRIBUTE_LABEL)){
#for(j in 1:3){
   tvar <- paste(tbl$ATTRIBUTE_LABEL[j])
   #merge
   if(tvar == "SAPICM2"){
    rmat <- soil.sp@data
    rm(soil.sp)
   }else{
    rmat <- suba.sp@data
   }
   set.seed(10002)
   #### subset just as test!!!!!
   #rmat <- rmat[sample(1:dim(rmat)[1], 100), ]
   sel <- !is.na(rmat[, tvar])
   tbl$N_OBSERVATIONS[j] <- sum(sel)
   if(tvar == "BDRLOG"){
    rv <- range(as.character(rmat[, tvar]))
    fm.g <- as.formula(paste0(tvar, " ~ ", paste(pr.lst, collapse="+")))
   } else{
    rv <- quantile(rmat[,tvar], c(.005,.995), na.rm=TRUE)
    fm.g <- as.formula(paste0("log1p(", tvar, ") ~ ", paste(pr.lst, collapse="+")))
   }
   tbl$OBSERVED_RANGE_MIN[j] <- rv[1]
   tbl$OBSERVED_RANGE_MAX[j] <- rv[2]
   nfold.x <- kfold(rmat, nfold)
   ####n-fold in parrelal
   sfInit(parallel=TRUE, cpus=nfold)
   sfLibrary(randomForest)
   sfLibrary(randomForestSRC)
   if(m.flag ==1)
   {
       sfLibrary(gbm)
       sfLibrary(kernlab)
      
   }
   sfLibrary(GSIF)
   sfLibrary(sp)
   sfExport("PC.flag", "pr.lst", "arti.flag", "soil.flag", "fit.name","m.flag" )
   cv.tvar <- sfClusterApplyLB(1 : nfold, cv_nfold, rmat=rmat,
    nfold.x=nfold.x, fm.g=fm.g, tvar=tvar, sub.N=sub.N, cell.size=cell.size , n=n)
   sfStop()
   gc()
   cv.lst[[j]] <- do.call(rbind, cv.tvar)
}
rm(suba.sp)

for(j in 1:length(tbl$ATTRIBUTE_LABEL))
{
    
  tvar <- paste(tbl$ATTRIBUTE_LABEL[j])
    if(tvar %in% c("BDRICM", "SAPICM2")){
        ## derive ME & RMSE & r2 on log prediction:
        try( tbl$rf.ME.log[j] <- round( mean(cv.lst[[j]]$rf.pred-cv.lst[[j]]$meas, na.rm=TRUE), 3) )
        try( tbl$rf.RMSE.log[j] <- signif( sqrt(mean((cv.lst[[j]]$rf.pred-cv.lst[[j]]$meas)^2, na.rm=TRUE)), 3) )    
        try( tbl$rf.SIGMA_EXPLAINED.log[j] <- round( (1-(var(cv.lst[[j]]$rf.pred-cv.lst[[j]]$meas, na.rm=TRUE)/var(cv.lst[[j]]$meas, na.rm=TRUE)))*100, 1) )
        ## derive ME & RMSE & r2:        
        try( tbl$rf.ME[j] <- round( mean(expm1(cv.lst[[j]]$rf.pred)-expm1(cv.lst[[j]]$meas), na.rm=TRUE), 3) )
        try( tbl$rf.RMSE[j] <- signif( sqrt(mean((expm1(cv.lst[[j]]$rf.pred)-expm1(cv.lst[[j]]$meas))^2, na.rm=TRUE)), 3) )
        try( tbl$rf.SIGMA_EXPLAINED[j] <- round( (1-(var(expm1(cv.lst[[j]]$rf.pred)-expm1(cv.lst[[j]]$meas), na.rm=TRUE)/var(expm1(cv.lst[[j]]$meas), na.rm=TRUE)))*100, 1) )       
        if(m.flag==1){
        ## derive ME & RMSE & r2 on log prediction:
        try( tbl$rk.ME.log[j] <- round( mean(cv.lst[[j]]$rk.pred-cv.lst[[j]]$meas, na.rm=TRUE), 3) )
        try( tbl$rk.RMSE.log[j] <- signif( sqrt(mean((cv.lst[[j]]$rk.pred-cv.lst[[j]]$meas)^2, na.rm=TRUE)), 3) )
        try( tbl$gbm.ME.log[j] <- round( mean(cv.lst[[j]]$gbm.pred-cv.lst[[j]]$meas, na.rm=TRUE), 3) )
        try( tbl$gbm.RMSE.log[j] <- signif( sqrt(mean((cv.lst[[j]]$gbm.pred-cv.lst[[j]]$meas)^2, na.rm=TRUE)), 3) )
        try( tbl$rk.SIGMA_EXPLAINED.log[j] <- round( (1-(var((cv.lst[[j]]$rk.pred)-(cv.lst[[j]]$meas), na.rm=TRUE)/var((cv.lst[[j]]$meas), na.rm=TRUE)))*100, 1) )
        try( tbl$gbm.SIGMA_EXPLAINED.log[j] <- round( (1-(var((cv.lst[[j]]$gbm.pred)-(cv.lst[[j]]$meas), na.rm=TRUE)/var((cv.lst[[j]]$meas), na.rm=TRUE)))*100, 1) )
        ## derive ME & RMSE & r2:
        try( tbl$rk.ME[j] <- round( mean(expm1(cv.lst[[j]]$rk.pred)-expm1(cv.lst[[j]]$meas), na.rm=TRUE), 3) )
        try( tbl$rk.RMSE[j] <- signif( sqrt(mean((expm1(cv.lst[[j]]$rk.pred)-expm1(cv.lst[[j]]$meas))^2, na.rm=TRUE)), 3) )
        try( tbl$gbm.ME[j] <- round( mean(expm1(cv.lst[[j]]$gbm.pred)-expm1(cv.lst[[j]]$meas), na.rm=TRUE), 3) )
        try( tbl$gbm.RMSE[j] <- signif( sqrt(mean((expm1(cv.lst[[j]]$gbm.pred)-expm1(cv.lst[[j]]$meas))^2, na.rm=TRUE)), 3) )
        try( tbl$rk.SIGMA_EXPLAINED[j] <- round( (1-(var(expm1(cv.lst[[j]]$rk.pred)-expm1(cv.lst[[j]]$meas), na.rm=TRUE)/var(expm1(cv.lst[[j]]$meas), na.rm=TRUE)))*100, 1) )
        try( tbl$gbm.SIGMA_EXPLAINED[j] <- round( (1-(var(expm1(cv.lst[[j]]$gbm.pred)-expm1(cv.lst[[j]]$meas), na.rm=TRUE)/var(expm1(cv.lst[[j]]$meas), na.rm=TRUE)))*100, 1) )
         #rk vs rf on log prediction
        try( tbl$var.test.H1[j] <- round(var.test((cv.lst[[j]]$rk.pred)-(cv.lst[[j]]$meas), (cv.lst[[j]]$rf.pred)-(cv.lst[[j]]$meas), alternative="greater")$p.value, 11) )
        try( tbl$t.test.H1[j] <- round(t.test(abs(cv.lst[[j]]$rk.pred-cv.lst[[j]]$meas), abs(cv.lst[[j]]$rf.pred-cv.lst[[j]]$meas),paired=T, alternative="greater")$p.value, 11) )
        #gbm vs rf
        try( tbl$var.test.H2[j] <- round(var.test((cv.lst[[j]]$gbm.pred)-(cv.lst[[j]]$meas), (cv.lst[[j]]$rf.pred)-(cv.lst[[j]]$meas), alternative="greater")$p.value, 11) )
        try( tbl$t.test.H2[j] <- round(t.test(abs(cv.lst[[j]]$gbm.pred-cv.lst[[j]]$meas), abs(cv.lst[[j]]$rf.pred-cv.lst[[j]]$meas),paired=T, alternative="greater")$p.value, 11) )
         }
    }
    if(tvar %in% c("BDRLOG")){
        #get confucitonmatrix
        c_m <- list(NULL)
        c_m[[1]] <- getconfusion(cv.lst[[j]]$meas,cv.lst[[j]]$rf.pred)
        names(c_m) <- "rf"
        if(m.flag == 1){            
            c_m[[2]] <- getconfusion(cv.lst[[j]]$meas,cv.lst[[j]]$svm.pred)
            names(c_m)[2] <- "svm"
        }                   
      }
}

save(cv.lst,tbl,c_m, file = paste0("./cv/cv_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".RData"))
rm(cv.tvar,cv.lst)
