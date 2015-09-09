rm(list = ls(all = TRUE))
#pkgs = names(sessionInfo()$otherPkgs)
#pkgs = paste('package:', pkgs, sep = "")
#lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
library(sp)
library(randomForest)
library(randomForestSRC)
library(ggRandomForests)

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
arti.flag <- 1  #1: add artificial points; 0: not
fit.name <- "all" # c("eu", "as", "us", "ca", "all")
soil.flag <- 1 # 0: without soil profiles; 1: add soil profiles
en.num <- 1   #number of ensemble prediction

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

load(paste0("./model/m_BDRICM_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", 1,".rda"))
load(paste0("./model/m_BDRLOG_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", 1,".rda"))

gg_md <- gg_minimal_depth(vs_BDRICM)
# plot the object
plot(gg_md)
dev.copy(png,paste0("./pics/vs_p", PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name,".png"), width = 500, height = 800, units = "px")
dev.off()
plot(gg_minimal_vimp(gg_md))
dev.copy(png,paste0("./pics/vs_imp_p", PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name,".png"), width = 500, height = 800, units = "px")
dev.off()

plot(m_BDRICM)
par(mar = rep(2, 4))
varImpPlot(m_BDRICM, main = "Model of bedrock depth")
dev.copy(png,paste0("./pics/m_p", PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name,".png"), width = 800, height = 550, units = "px")
dev.off()

