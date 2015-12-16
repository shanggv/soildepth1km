# title         : cv.region.R
# purpose       : goodness of fitting and cross validation by regions;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : points with overlayed covariates 
# outputs       : compressed RDA files and png files ;
# remarks 1     : Takes several minites

rm(list = ls(all = TRUE))
########################################Goodness of fit & validation
library(hexbin)
library(gridExtra)
library(lattice)
library(randomForest)
library(sp)
data(R_pal, package = "plotKML")
log.flag <- 1
PC.flag <-0
a.dir <- "/home/shang009/big/"# dir of the project
m.dir <- paste0(a.dir, "/soildepth")
source(paste0(a.dir, "/soildepth/code/head/functions.r"))
setwd(m.dir)

#BDRICM, SAPICM2
t.lst <- c("BDRICM","SAPICM2")
m.lst <- paste("m_", t.lst, sep = "")
fit.names <- c("ca", "us", "eu", "as")
#load data
sub.sp <- list(NULL)
for ( i in 1:length(fit.names) )
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", fit.names[i], ".rda")) 
    sub.sp[[i]]<-suba.sp
}
names(sub.sp) <- c("ca", "us", "eu", "as")


arti.flag <- 1
soil.flag <- 0
#cross validation
fit.names <- c("ca", "us", "eu", "as", "all")
for ( i in 1:length(fit.names) )
{
    fit.name <- fit.names[i]
    source(paste0(a.dir, "/soildepth/code/1km/cv.region.r"))
}

fit.name <- "all"
soil.flag <- 1
source(paste0(a.dir, "/soildepth/code/1km/cv.region.r"))
