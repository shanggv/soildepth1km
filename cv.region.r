# title         : cv.region.R
# purpose       : goodness of fitting and cross validation by regions, called by cv.region.n;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : points with overlayed covariates 
# outputs       : compressed RDA files and png files ;
# remarks 1     : Takes several minites
##### Goodness of fit:
r2.oob <- NULL
r2 <- NULL
me <- NULL
rmse <- NULL
r2.log <- NULL
me.log <- NULL
rmse.log <- NULL
fit.c <- NULL
plotList <- list(NULL)
if(fit.name == "all") len <-2 else len <-1
#for(i in 1:length(m.lst)){
for(i in 1:len){
    if(t.lst[i] == "BDRICM"){
     load(paste0("./model/rf/m_", t.lst[i], "_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_1.rda"))  
    }else if(t.lst[i] == "SAPICM2"){
     load(paste0("./model/rf/m_", t.lst[i], "_p",PC.flag, ".rda")) 
    }
    m <- get(m.lst[i])
    rm(list = m.lst[i])
    # correlation plot
    if(log.flag ==1 ){
        m$prelog <- m$predicted
        m$ylog <- m$y
        m$predicted <- expm1(m$prelog)
        m$y <- expm1(m$ylog)
    }else{
        m$prelog <- log1p(m$predicted)
        m$ylog <- log1p(m$y)
    }
    plotList[[i]] <- hexbinplot(m$prelog~ m$ylog,
        colramp=colorRampPalette(R_pal[["bpy_colors"]]), main=paste0(fit.name, "\n(fitting)"),
        xlab="measured", ylab="predicted",
        type="g", lwd=1, lcex=8, inner=.2, cex.labels=.8,
        xlim=range(m$ylog), ylim=range(m$prelog),
        asp=1, xbins=25, density=40, panel=pfun)
    # r2 value OOB
    r2.oob[[i]] <- round(m$rsq[length(m$rsq)], digits=3)
    # orignal scale
    r2[[i]] <- cor(m$predicted, m$y)
    me[[i]] <- signif(mean(m$predicted - m$y), 3)
    rmse[[i]] <- signif(sqrt(mean((m$predicted - m$y) ^ 2)), 3)
    # log scale
    r2.log[[i]] <- cor(m$prelog, m$ylog)
    me.log[[i]] <- signif(mean(m$prelog - m$ylog), 3)
    rmse.log[[i]] <- signif(sqrt(mean((m$prelog - m$ylog) ^ 2)), 3)
    gc()
    if(t.lst[i] == "BDRICM"){
        m$precl <- toclass(m$predicted, dclass)
        m$ycl <- toclass(m$y, dclass)
        fit.c[[i]] <- getcor(m$ycl, m$precl, length(dclass$class))
    }
}


if(fit.name == "all"){
    plotList[[1]]$main <- "BDRICM"
    plotList[[2]]$main <- "SAPICM"
    do.call(grid.arrange, c(plotList, ncol=2))    
    dev.copy(png,paste0("./pics/fit_", PC.flag, "all.png"),  width = 1000, height = 480, units = "px")
    dev.off()
    st.rg <- list(r2.oob =r2.oob, r2 = r2, me = me, rmse = rmse, r2.log = r2.log, me.log = me.log , rmse.log = rmse.log, fit.c = fit.c)
    save(st.rg, file = paste0("./model/rf/st_", t.lst[i], "_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, ".rda"))
}else{
####validation by regions

val.lst <- sub.sp
load(paste0("./profs/subs/subs.sp_", PC.flag, "_", fit.name, ".rda"))
val.lst[[fit.name]] <- subset( val.lst[[fit.name]], !(val.lst[[fit.name]]$SOURCEID %in% subs.sp[[1]]$SOURCEID))
rm(subs.sp) 
val.r <- names(val.lst)
r2.v <- NULL
me.v <- NULL
rmse.v <- NULL
r2.log.v <- NULL
me.log.v <- NULL
rmse.log.v <- NULL
val.c <- NULL
#only for BDRICM
#for(i in 1:length(t.lst)){
for(i in 1:1){
   
    load(paste0("./model/rf/m_", t.lst[i], "_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_1.rda"))
    m <- get(m.lst[i])
    rm(list = m.lst[i])
    for(j in 1:length(val.r))
    #for(j in 2:3)
    {
        val.d <- NULL
        dm <- predict(m, val.lst[[j]]@data, na.action=na.pass)
        val.d <- data.frame(val.lst[[j]][[t.lst[i]]])
        names(val.d) <- "y"
        val.d$ylog <- log1p(val.d$y)
        if(log.flag == 1){
            val.d$p <- as.integer(expm1(dm))
            val.d$plog <- dm
        }else{
            val.d$p <- as.integer(dm)
            val.d$plog <- as.integer(log1p(dm))
        }
        r2.v[[j]]   <- signif(cor(val.d$p, val.d$y) ^ 2, 3)
        me.v[[j]]   <- signif(mean(val.d$p - val.d$y), 3)
        rmse.v[[j]] <- signif(sqrt(mean((val.d$p - val.d$y) ^ 2)), 3)
        r2.log.v[[j]]   <- signif(cor(val.d$plog, val.d$ylog) ^ 2, 3)
        me.log.v[[j]]   <- signif(mean(val.d$plog - val.d$ylog), 3)
        rmse.log.v[[j]] <- signif(sqrt(mean((val.d$plog - val.d$ylog) ^ 2)), 3)
        val.d$precl <- toclass(val.d$p, dclass)
        val.d$ycl <- toclass(val.d$y, dclass)
        val.c[[j]] <- getcor(val.d$ycl, val.d$precl, length(dclass$class))
        # correlation plot
        plotList[[j+1]] <- hexbinplot(val.d$plog~ val.d$ylog,
             colramp=colorRampPalette(R_pal[["bpy_colors"]]), main=paste0(val.r[[j]], "\n(validation)"),
             xlab="measured", ylab="predicted",
             type="g", lwd=1, lcex=8, inner=.2, cex.labels=.8,
             xlim=range(val.d$plog), ylim=range(val.d$ylog),
             asp=1, xbins=25, density=40, panel=pfun)
    }
    names(r2.v) <- names(me.v) <- names(rmse.v) <- names(val.c) <-val.r
    names(r2.log.v) <- names(me.log.v) <- names(rmse.log.v) <- val.r
    do.call(grid.arrange, c(plotList, nrow =3, ncol=2))
    dev.copy(png,paste0("./pics/", t.lst[i],"_val_", PC.flag, fit.name,".png"),  width = 500, height = 800, units = "px")
    dev.off()    
 st.rg <- list(r2.oob =r2.oob, r2 = r2, me = me, rmse = rmse, r2.log = r2.log, me.log = me.log , rmse.log = rmse.log, 
 r2.v = r2.v, me.v = me.v, rmse.v = rmse.v, r2.log.v = r2.log.v, me.log.v = me.log.v , rmse.log.v = rmse.log.v, fit.c = fit.c, val.c = val.c )
 save(st.rg, file= paste0("./model/rf/st_", t.lst[i], "_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, ".rda"))

}

}# end of if(fit.name != "all")

#BDRLOG
#load(paste0("m_BDRLOG_", PC.flag,  "_",  fit.name, "_1.rda"))
#m <- m_BDRLOG
#rm(m_BDRLOG)
#gc()
#err.rate <- round(m$err.rate[m$ntree, "OOB"]*100, digits=2)
#confusion <-m$confusion
#
#
#confusion.v <- NULL
#err.rate.v <- NULL
#plotList <- list(NULL)
#for(j in 1:length(val.r))
##for(j in 2:3)
#{
#    val.d <- NULL
#    dm <- predict(m, val.lst[[j]]@data, na.action=na.pass)
#  
#    val.d <- data.frame(val.lst[[j]][["BDRLOG"]])
#    names(val.d) <- "y"
#    val.d$p <- as.factor(dm)
#    err.rate.v[[j]]  <- signif(100 - sum(val.d$p==val.d$y)/length(val.d$y)*100, 3)
#    tmp <- matrix(0, nrow = 2, ncol =3 )
#    tmp[1,1] <-  sum((val.d$y == F) & (val.d$p == F))
#    tmp[1,2] <-  sum((val.d$y == F) & (val.d$p == T))
#    tmp[2,1] <-  sum((val.d$y == T) & (val.d$p == F))
#    tmp[2,2] <-  sum((val.d$y == T) & (val.d$p == T))
#    tmp[1,3] <-  tmp[1,2]/(tmp[1,1]+tmp[1,2])
#    tmp[2,3] <-  tmp[2,1]/(tmp[2,1]+tmp[2,2])
#    colnames(tmp) <- colnames(confusion)
#    rownames(tmp) <- rownames(confusion)
#    confusion.v[[j]] <- tmp
#}
#names(confusion.v) <- names(err.rate.v) <-val.r
#
#
#rm(sub.sp,subs.sp,val.lst)
#
#
#r2.oob
#r2
#me
#rmse
#r2.log
#me.log
#rmse.log
#
#r2.v
#me.v
#rmse.v
#r2.log.v
#me.log.v
#rmse.log.v
#
#fit.c
#val.c
#
#err.rate
#err.rate.v
#confusion
#confusion.v
