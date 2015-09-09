rm(list = ls(all = TRUE))
#pkgs = names(sessionInfo()$otherPkgs)
#pkgs = paste('package:', pkgs, sep = "")
#lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
library(sp)
library(randomForest)
library(maps)
library(grDevices)
library(maptools)
#par(resetPar())
# global define
#gdal.dir = shortPathName("C:\\ms4w")
#gdal_setInstallation(search_path=gdal.dir, rescan=TRUE)
a.dir <- "/data/shang009/big"# dir of the project
w.dir <- paste0(a.dir, "/worldgrids")
m.dir <- paste0(a.dir, "/soildepth")
setwd(m.dir)
source(paste0(a.dir, "/soildepth/code/head/functions.r"))

PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 0  #1: add artificial points; 0: not
fit.name <- "all" # c("eu", "as", "us", "ca", "all")
soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
en.num <- 1   #number of ensemble prediction

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}


varImpPlot2 <- function(imp , main ="BDR")
{
    n.var = min(30, nrow(imp))
    sort = TRUE
    ## If there are more than two columns, just use the last two columns.
    if (ncol(imp) > 2) imp <- imp[, -(1:(ncol(imp) - 2))]
    nmeas <- ncol(imp)
    if (nmeas > 1) {
        op <- par(mfrow=c(1, 2), mar=c(4, 5, 4, 1), mgp=c(2, .8, 0),
                  oma=c(0, 0, 2, 0), no.readonly=TRUE)
        on.exit(par(op))
    }
    for (i in 1:nmeas) {
        ord <- if (sort) rev(order(imp[,i],
                                   decreasing=TRUE)[1:n.var]) else 1:n.var
        xmin <- if (colnames(imp)[i] %in%
                    c("IncNodePurity", "MeanDecreaseGini")) 0 else min(imp[ord, i])
        dotchart(imp[ord,i], xlab=colnames(imp)[i], ylab="",
                 main=if (nmeas == 1) main else NULL,
                 xlim=c(xmin, max(imp[,i])))
    }
    if (nmeas > 1) mtext(outer=TRUE, side=3, text=main, cex=1.2)
    invisible(imp)
}

pca.imp <- function (rf.imp, spc, n.imp = 5)
{
    pc.len <- dim(rf.imp)[1]
    rf.imp <- rf.imp[ , (dim(rf.imp)[2]-1) : dim(rf.imp)[2]] # only the last two
    rot <- spc$rotation[,1:pc.len]
    scale.cof <- apply(rot, 2, function(x) sum(abs(x)))
    rot.scale <- t(apply(rot, 1, function(x, sca){x/scale.cof}, sca = scale.cof))
    ####apply(rot.scale, 2, function(x) sum(abs(x)))
    cov.imp <- NULL
    for(i in 1:2){
        cov.pc.imp <- apply(abs(rot.scale), 1, function(x, imp){ # negative coefficient turned into positive
            x*imp
        }, imp = rf.imp[,i])
        cov.imp <- cbind(cov.imp, apply (cov.pc.imp, 2, function(x){sum(x)}))
    }
    colnames(cov.imp) <- colnames(rf.imp)
    return(cov.imp)
}
Partmp <- resetPar() 
par(Partmp)



nRuns <-1
imp_BDRICM <- as.list(rep(0,length(nRuns)))
imp_BDRLOG <- as.list(rep(0,length(nRuns)))
for(i in nRuns)
{
    load(paste0("m_BDRICM_",PC.flag, "_", fit.name, "_", i, ".rda"))
    load(paste0("m_BDRLOG_",PC.flag, "_", fit.name, "_", i, ".rda"))
    imp_BDRICM[[i]] <- importance(m_BDRICM)
    imp_BDRLOG[[i]] <- importance(m_BDRLOG)
    varImpPlot2(imp_BDRICM[[i]])
    dev.copy(png, paste0("./pics/imp_BDRICM_", PC.flag, "_", fit.name, "_", i,".png"))
    dev.off()
    varImpPlot2(imp_BDRLOG[[i]])
    dev.copy(png, paste0("./pics/imp_BDRLOG_", PC.flag, "_", fit.name, "_", i,".png"))
    dev.off()
}



load(paste0(a.dir, "/worldgrids/worldgrids.spc.rda"))
if(PC.flag == 1) {
    imp.all   <- matrix(0, nrow = dim(imp_BDRICM[[1]])[1]+2, ncol = 2)
    colnames(imp.all) <- colnames(imp_BDRICM[[1]])
    for(i in nRuns)
    {

        tmp <- pca.imp(imp_BDRICM[[i]], worldgrids.spc)
        varImpPlot2(tmp)
        imp.all   <- imp.all + tmp
    }
    imp.all <- imp.all/length(nRuns)
    rownames(imp.all) <-  rownames(tmp)
    varImpPlot2(imp.all)
    dev.copy(png,paste0("./pics/imp_BDRLOG_", PC.flag,"_all.png"))
    dev.off()
}
