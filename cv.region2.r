# title         : cv.region2.R
# purpose       : goodness of fitting and cross validation by regions from continent to states, called by cv.region.n;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Beijing.
# inputs        : points with overlayed covariates 
# outputs       : compressed RDA files and png files ;
# remarks 1     : Takes several minites
getfullname <- function(name)
{
    if(name == "as") fname <- "Australia" else
    if(name == "eu") fname <- "Europe" else
    if(name == "na") fname <- "North America" else
    if(name == "ca") fname <- "Canada" else
    if(name == "us") fname <- "United States" else
    if(name == "se") fname <- "Sweden" else
    if(name == "ie") fname <- "Ireland" else
    if(name == "us1" | name =="mus1") fname <- "Alaska" else
    if(name == "us3" | name =="mus3") fname <- "Indiana" else
    if(name == "us4" | name =="mus4") fname <- "Iowa" else
    if(name == "us5" | name =="mus5") fname <- "South central Kansas" else
    if(name == "us6" | name =="mus6") fname <- "Kentuky" else
    if(name == "us7" | name =="mus7") fname <- "Maine" else
    if(name == "us8" | name =="mus8") fname <- "Minnesota" else
    if(name == "us9" | name =="mus9") fname <- "Missouri" else
    if(name == "us10" | name =="mus10") fname <- "Nevada" else
    if(name == "us11" | name =="mus11") fname <- "New Hampshire" else
    if(name == "us12" | name =="mus12") fname <- "New York" else
    if(name == "us13" | name =="mus13") fname <- "Ohio" else
    if(name == "us14" | name =="mus14") fname <- "Northern High Plains" else
    if(name == "us15" | name =="mus15") fname <- "Pennsylvania" else
    if(name == "us16" | name =="mus16") fname <- "Tennessee" else
    if(name == "us17" | name =="mus17") fname <- "Vermont" else
    if(name == "ca1" | name =="mca1") fname <- "British Columbia" else
    if(name == "ca3" | name =="mca3") fname <- "Nova Scotia" else
    if(name == "ca4" | name =="mca4") fname <- "Ontario" else
    if(name == "ca5" | name =="mca5") fname <- "Québec"  
    return(fname)  
}

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
       load(paste0("./model/m_", t.lst[i], "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_1.rda"))  
    }else if(t.lst[i] == "SAPICM2"){
     load(paste0("./model/m_", t.lst[i], "_p",PC.flag, ".rda")) 
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
    plotList[[i]] <- hexbinplot(m$prelog~ m$ylog,
        colramp=colorRampPalette(R_pal[["bpy_colors"]]), 
        main=paste0(getfullname(fit.name), "\n(fitting, ", "R square: ", signif(r2.log[[i]],digits = 2), ")"),
        xlab="measured", ylab="predicted", cex.title = "1",
        type="g", lwd=1, lcex=8, inner=.2, cex.labels=.8,
        xlim=range(m$ylog), ylim=range(m$ylog),
        asp=1, xbins=25, density=40, panel=pfun)
}


####validation by regions
load(paste0("./profs/subs/subs.sp_", n.name, "p", PC.flag, "_a",arti.flag, "_", fit.name, ".rda")) 
val.lst <- sub.sp
val.lst[[fit.name]] <- subset( val.lst[[fit.name]], !(val.lst[[fit.name]]$SOURCEID %in% subs.sp$SOURCEID))
val.r <- names(val.lst)

#initialize the statistics
r2.v <- rep(NA,length(val.r))
me.v <- rep(NA,length(val.r))
rmse.v <- rep(NA,length(val.r))
r2.log.v <- rep(NA,length(val.r))
me.log.v <- rep(NA,length(val.r))
rmse.log.v <- rep(NA,length(val.r))
val.c <- as.list(rep(NA,length(val.r)))
sim <-  rep(NA,length(val.r))
sims <-  as.list(rep(NA,length(val.r)))
sim2 <-  rep(NA,length(val.r))
# rearrage the plot
val.or <- c(which(fit.name == val.r),subset(1:length(val.r), fit.name != val.r))

#only for BDRICM
#for(i in 1:length(t.lst)){
#for(i in 1:1){
    load(paste0("./model/m_", t.lst[i], "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_1.rda")) 
    m <- get(m.lst[i])
    rm(list = m.lst[i])
    vars <- vs_BDRICM$topvars    
    donor <- subs.sp@data[vars]
    covd<- cov(donor)
    try(Dc <- mahalanobis(donor, colMeans(donor), covd))
    if(class(.Last.value)[1]=="try-error")
    {
        vars <- subset(vars, substr(vars,1,3) !="LIT" &  substr(vars,1,3) !="ELF" &   substr(vars,4,6) !="ESA")
        donor <- subs.sp@data[vars]
        covd<- cov(donor)
        Dc <- mahalanobis(donor, colMeans(donor), covd)
    }
    thshold <- quantile(Dc, probs = 0.975)
    tmp <- get_h_max(donor, vars)
    hmax <- tmp$h_max
    mn_XXinv <- tmp$mn_XXinv
    
    k<-2
    for(j in val.or)
    #for(j in 2:3)
    {
        if(length(val.lst[[j]])>1000)
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
            #simialrity
            recip <- val.lst[[j]]@data[vars]
            Dr <- mahalanobis(recip, colMeans(donor), covd)
            sim[[j]] <- sum(Dr<thshold)/length(Dr)
            sims[[j]] <- Dr<thshold
            tmp <- check_feature_space(recip, vars, hmax, mn_XXinv)
            sim2[[j]] <- 1-tmp$frac
            # correlation plot
            plotList[[k]] <- hexbinplot(val.d$plog~ val.d$ylog,
                 colramp=colorRampPalette(R_pal[["bpy_colors"]]), 
                 main=paste0(getfullname(val.r[[j]]), "\n(validation, ", "R sqaure: ",signif(r2.log.v[[j]],digits = 2),")"),
                 xlab="measured", ylab="predicted", cex.title = "1",
                 type="g", lwd=1, lcex=8, inner=.2, cex.labels=.8,
                 xlim=range(val.d$ylog), ylim=range(val.d$ylog),
                 asp=1, xbins=25, density=40, panel=pfun)
            k <- k+1
        }
    }
    names(r2.v) <- names(me.v) <- names(rmse.v) <- names(val.c) <-val.r
    names(r2.log.v) <- names(me.log.v) <- names(rmse.log.v) <- val.r
    
    par(mar=c(0, 0, 0, 0))
    bitmap(paste0("./pics/", t.lst[i], "_n", n.name,"_val_p", PC.flag, 
    fit.name,".tiff"), width = 7.48, height = ceiling(length(plotList)/2)*3.74, units = "in", res =300, type = "tiff12nc", pointsize =11)
    do.call(grid.arrange, c(plotList, nrow =ceiling(length(plotList)/2), ncol=2))
    #dev.copy(png,paste0("./pics/", t.lst[i], "_n", n.name,"_val_p", PC.flag, 
    #fit.name,".png"),  width = 500, height = ceiling(length(plotList)/2)*250, units = "px")     
    dev.off() 
    par(mar=c(5.1, 4.1, 4.1, 2.1))    
    st.rg <- list(r2.oob =r2.oob, r2 = r2, me = me, rmse = rmse, r2.log = r2.log, me.log = me.log , rmse.log = rmse.log, 
    r2.v = r2.v, me.v = me.v, rmse.v = rmse.v, r2.log.v = r2.log.v, me.log.v = me.log.v , rmse.log.v = rmse.log.v, sim = sim, sim2 = sim2, fit.c = fit.c, val.c = val.c )
    save(st.rg, file= paste0("./model/st_", t.lst[i], "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, ".rda"))

#}



#
r2.oob
r2
me
rmse
r2.log
me.log
rmse.log

r2.v
me.v
rmse.v
r2.log.v
me.log.v
rmse.log.v
sim
sims
#
#fit.c
#val.c
#
#err.rate
#err.rate.v
#confusion
#confusion.v
