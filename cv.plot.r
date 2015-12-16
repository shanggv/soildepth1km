# title         : cv.plot.R
# purpose       : plot cross validation, call by cv.n.r;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : 
# outputs       : png files ;
# remarks 1     : Takes ca 1.2 hrs to run with 5 cpus for randomforest in the defualt setting in use 
library(hexbin)
library(lattice)
library(gridExtra)
data(R_pal, package = "plotKML")
val.c <- NULL
plotList <- list(NULL)
j<-1
log.flag <- 1
for(j in 1:2){
  ## derive ME & RMSE:
    if(log.flag == 1)
    {
        cv.lst[[j]]$rf.predlog <- cv.lst[[j]]$rf.pred
        cv.lst[[j]]$measlog <- cv.lst[[j]]$meas
        cv.lst[[j]]$rf.pred <- expm1(cv.lst[[j]]$rf.predlog)
        cv.lst[[j]]$meas <- expm1(cv.lst[[j]]$measlog)
    }else if(log.flag == 0){
        cv.lst[[j]]$rf.predlog <- log1p(cv.lst[[j]]$rf.pred)
        cv.lst[[j]]$measlog <- log1p(cv.lst[[j]]$meas)
    }
    cv.lst[[j]]$rf.predcl <- toclass(cv.lst[[j]]$rf.pred, dclass)
    cv.lst[[j]]$meascl <- toclass(cv.lst[[j]]$meas, dclass)
    val.c[[j]] <- getcor(cv.lst[[j]]$meascl, cv.lst[[j]]$rf.predcl, length(dclass$class))
    tvar <- paste0(tbl$ATTRIBUTE_LABEL[j])
    plotList[[j]] <- hexbinplot(cv.lst[[j]]$rf.predlog ~ cv.lst[[j]]$measlog,
             colramp=colorRampPalette(R_pal[["bpy_colors"]]), main = tvar,
             xlab="measured", ylab="predicted",
             type="g", lwd=1, lcex=8, inner=.2, cex.labels=.8,
             xlim=range(cv.lst[[j]]$measlog), ylim=range(cv.lst[[j]]$rf.measlog),
             asp=1, xbins=25, density=40, panel=pfun)
}
#plot(plotList[[1]])
do.call(grid.arrange, c(plotList, ncol=2))
dev.copy(png,paste0("./pics/cv_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".png"),  width = 800, height = 480, units = "px")
dev.off()

