# title         : com.celln.n.r
# purpose       : compare different n in sample.grid;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Beijing.
# inputs        : prediction models
# outputs       : statistics ;
# remarks 1     : Takes several minutes;
rm(list = ls(all = TRUE))
library(sp)
library(randomForest)
library(randomForestSRC)
# directory
a.dir <- "/data/shang009/big"# dir of the project
w.dir <- paste0(a.dir, "/worldgrids")
m.dir <- paste0(a.dir, "/soildepth")
setwd(m.dir) 

####setting for cross validation by regions
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 0  #1: add artificial points; 0: not
soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
en.num <- 1   #number of ensemble prediction

fit.names <- c("us","usm", "na")
n.names <- c("")
source(paste0(a.dir, "/soildepth/code/1km/com.celln.r"))

fit.names <- c("ca","cam")
n.names <- c("")
source(paste0(a.dir, "/soildepth/code/1km/com.celln.r"))
fit.names <- c("ca")
n.names <- c(9)
source(paste0(a.dir, "/soildepth/code/1km/com.celln.r"))

fit.names <- c("eu","as","se","ie","us1", paste0("us", 3:17),"ca1", paste0("ca", 3:5))
n.names <- c("",9,18,27)
source(paste0(a.dir, "/soildepth/code/1km/com.celln.r"))

#
#with r2(n=3)<0.5
#because of small number:us1,us10
#other reason:us7,us9,us11,us12,ca1,ca4,ca5
#very bad r2(n.name==18)<0.4: us15,us16,
#very bad r2(n.name==27)<0.4:us17,ca3, may not use
