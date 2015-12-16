# title         : getcolinear.R
# purpose       : get colinearity and reduce the covarites and performs a principal components analysis;
# reference     :
# producer      : Prepared by W. Shangguan
# address       : In Wageningen, NL.
# inputs        : WorldGrids layers
# outputs       : compressed RDA files;
# remarks 1     : Takes ca 10 mins in overlay ;
rm(list = ls(all = TRUE))
#pkgs = names(sessionInfo()$otherPkgs)
#pkgs = paste('package:', pkgs, sep = "")
#lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
library(rgdal)
library(sp)
library(RSAGA)
library(raster)
library(plyr)
library(snowfall)
library(caret)

#global define
#wdir <- "D:/worldgrids/1km"
wdir <- "/home/shang009/big/worldgrids"
#wdir <- "/data/sheepfs/shang/worldgrids"
setwd(wdir)

  
# list:    
    g.lst <-c("DEMSRE3a", "SLPSRT3a", "TWISRE3a",  "TDMMOD3a",
    "TDSMOD3a", "TNMMOD3a", "TNSMOD3a", paste("TX", 1:6, "MOD3a", sep=""),
    "EVMMOD3a", "EVSMOD3a", "EVLMOD3a", "EVHMOD3a", "LAHMOD3a", "LALMOD3a",
    "LAMMOD3a", "LASMOD3a", "LITUSG3a",  "GCPIIA3a", "OPISRE3a",  "GTDHYS3a",
    "ELFUSG3a", paste("PX", 1:4, "WCL3a", sep=""), 
    paste("G0", 1:9, "ESA3a", sep=""),
    paste("G", 10:22, "ESA3a", sep=""), "L11IGB3a")


## download and unzip:
#for(j in 1:length(g.lst)){
#  outname = paste(g.lst[j], ".tif.gz", sep="")
#  outname.tif = paste(g.lst[j], ".tif", sep="")
#  if(is.na(file.info(outname.tif)$size)){
#    if(is.na(file.info(outname)$size)){
#      try(download.file(paste("http://worldgrids.org/lib/exe/fetch.php?media=", outname, sep=""), outname))
#    }
#    gunzip(paste0(wdir, "/", outname), remove = FALSE)
#    #system(paste("7za e", outname))
#    #unlink(outname)
#  }
#}

####fix the "GCPIIA3a"
#tmp <- readGDAL("GCPIIA3a.tif")
#tmp$band1[is.na(tmp$band1)] <- 0
#writeGDAL(tmp, fname = "GCPIIA3a.tif", drivername = "GTiff",  type = "Byte", mvFlag = 255)
#the following maybe a problem when sample from a coarse mask
#EVSMOD3a,GCPIIA3a, GCPIIA3a, GTDHYS3a: 0 value for non land
#DEMSRE3a: non-zero value for non land
#SLPSRT3a: non-zero value for non land


## covariate layers check if available:
tif.lst <- list.files(pattern=glob2rx("*3*.tif$"))
for(j in 1:length(g.lst)){
  if(is.na(file.info(paste(set.file.extension(g.lst[j], ".tif")))$size)){
    stop(paste("Layer missing:", g.lst[j]))
  }
}
tif.lst <- paste0(g.lst,".tif") # keep only selected covariates,  SMKISR3a is not included

## legends: GLTUHA3x vs LITUSG3a; LFOUSG3a, ELFUSG3a vs L3POBI3b
levs.LITUSG3a <- read.table("LITUSG3a.txt", sep="\t", header = TRUE)
levs.ELFUSG3a <- read.table("ELFUSG3a.txt", sep="\t", header = TRUE)

####get sample locations to represent the whole feature space
# mask to be update !!!!!!!!! 5km
set.seed(101)
LMTGSH1a <- readGDAL("LMTGSH1a.tif") ## HUGE DATA!!
LMTGSH1a$LMTGSH1a <- as.integer(LMTGSH1a$band1==1|LMTGSH1a$band1==2)
LMTGSH1a$LMTGSH1a[LMTGSH1a$LMTGSH1a == 0] <- NA
m5km <- as(LMTGSH1a["LMTGSH1a"], "SpatialPixelsDataFrame")
m5km <- subset(m5km, m5km@coords[,2] > -60)
#6,083,150 points!
#rpoints <- spsample(m5km, n=5e5, type="random")
rpoints <- as(m5km, "SpatialPoints")
#regular sampling will give much faster overlay
rpoints <- rpoints[(1:floor(length(rpoints)/10))*10]
proj4string(rpoints) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#608,315
save(rpoints, file="rpoints.rda")
rm(m5km, LMTGSH1a)
load("rpoints.rda")

wrapper.overWorldGrids <- function(i){
   r <- raster(paste0(wdir, "/", tif.lst[i]))
   ov <- raster::extract(r, rpoints.sub) # this take longest time
   ## reformat factor data:
   if(tif.lst[i]=="LITUSG3a.tif"){
     xf <- data.frame(Number=as.factor(ov))
     suppressWarnings( ov <- join(x=xf, y=levs.LITUSG3a[,c("Number","NAME")], type="left", match="first")$NAME )
   }
   if(tif.lst[i]=="ELFUSG3a.tif"){
     xf <- data.frame(Number=as.factor(ov))
     suppressWarnings( ov <- join(x=xf, y=levs.ELFUSG3a[,c("Number","NAME")], type="left", match="first")$NAME )
   }
   ov <- data.frame(ov)
   names(ov) <- strsplit(tif.lst[i], ".tif")[[1]][1]
   gc()#
   return(ov)
}

#############single process
####on PC
#25.62  with 10000 points
#47.02 with 20000
#103 with 50000
#181 with 100000
####data on big using donkey
#20 with 10000 points
#34  with 20000
#135 with 100000
#######data on sheepfs using donkey
#65 with 10000 points
#151 with 100000
#######data on sheepfs using one process of sheep1
#229 with 100000

nRuns <- 1:length(tif.lst)
#nRuns <- 1
nSlaves <- 10
ov.pca <- NULL

p.len <- 5e4
p.n <- round(length(rpoints)/5e4)

for(i in 1:p.n) ####split the rpoints will make the overlay run faster: 110sec vs 560sec
{
    if(i < p.n) rpoints.sub <- rpoints[((i-1)*p.len+1):(i*p.len)]
    else rpoints.sub <- rpoints[((i-1)*p.len+1): length(rpoints)]
    sfInit( parallel=TRUE, cpus = nSlaves)
    #sfInit( parallel=TRUE, type="SOCK", socketHosts = c(rep("localhost",nSlaves),rep("sheep1@sheep1", nSlaves),
    #    rep("sheep2@sheep2", nSlaves), rep("sheep3@sheep3", nSlaves)),
    #    slaveOutfile="~/errorwg.log")
    sfLibrary(plyr)
    sfLibrary(raster)
    sfExport("tif.lst", "rpoints.sub", "levs.LITUSG3a", "levs.ELFUSG3a", "wdir")
    ptm <- proc.time()
    tmp <- sfClusterApplyLB(nRuns, wrapper.overWorldGrids)
    print(i)
    print(proc.time() - ptm)
    sfStop()
    tmp <- do.call(cbind, tmp)
    ov.pca <- rbind(ov.pca, tmp)
}
save.image("1.RData")
#load("1.RData")
# for nRuns <- 1:10, nSlaves <- 10, length(rpoints) == 1e5
#overlay takes ca  254 secs on donkey with 10 processors & data on sheepfs
#overlay takes ca 845 secs on sheep1 with 10 processors & data on sheepfs
#overlay takes ca 2696 secs on sheep2 with 10 processors & data on sheepfs
#overlay takes ca 2717 secs on sheep3 with 10 processors & data on sheepfs

## convert every factor to indicators:
for(j in 1:length(g.lst)){
  if(is.factor(ov.pca[,g.lst[j]])){
    ln <- levels(ov.pca[,g.lst[j]])
    for(k in 1:length(ln)){
      vn <- paste(g.lst[j], k, sep="_")
      ov.pca[,vn] <- as.integer(ov.pca[,g.lst[j]]==ln[k])
      #ELFUSG3a: Undefined, Water body
      #LITUSG3a: Undefined, Water body
      if(sum(ov.pca[,vn], na.rm = T)<2)
      {
        print(g.lst[j])
        print(ln[k])
        ov.pca[,vn] <- NULL
      }
    }
  }else{# set as integer to save storage
    ov.pca[,g.lst[j]] <- as.integer(ov.pca[,g.lst[j]])
  }
}
varsn <- names(ov.pca)[which(!sapply(ov.pca, is.factor))]
ov.pca.f <- ov.pca[,varsn]

########### get rid of colinearity
descrCorr <- cor(ov.pca.f, use = "pairwise.complete.obs")
tmp <- findCorrelation(descrCorr, cutoff = .9, verbose = T, names = T, exact =T)
ov.pca.f <- ov.pca.f[, varsn[!varsn %in% tmp]]
#after colinearity
#[1] "TNMMOD3a"   "TX2MOD3a"   "TX6MOD3a"   "TDMMOD3a"   "TNSMOD3a"
# [6] "TX3MOD3a"   "TX5MOD3a"   "PX2WCL3a"   "LASMOD3a"   "ELFUSG3a_8"
###	LITUSG3a_12:"Basic Plutonics" and ELFUSG3a_8: "Undefined" are highly corelated (0.974 )
#cor(ov.pca$LITUSG3a_12,ov.pca$ELFUSG3a_8, use = "pairwise.complete.obs")
varsn <- names(ov.pca)[which(sapply(ov.pca, is.factor))]
ov.pca <- ov.pca[, c(names(ov.pca.f), varsn)]
##get the new g.lst
g.lst <- names(ov.pca)[names(ov.pca) %in% g.lst]

###get global means
varsn <- g.lst[!g.lst %in% varsn]
gMeans <- colMeans(ov.pca[,varsn], na.rm=TRUE)
varsn <- names(ov.pca)[which(sapply(ov.pca, is.factor))]
tmp <- apply(ov.pca[,varsn], 2, function(idx) {
    idx <-as.factor(idx)
    tmp <- which(tabulate(idx) == max(tabulate(idx)))
     list(factor(as.character(count(idx)[tmp,1]), levels = levels(idx)))
     })
tmp <- lapply(tmp, function(x)x[[1]] )
gMeans <- c(as.list(gMeans), tmp)


####get the pricipal componets worldgrids.spc
# two possible reasons for PCA to fail, missing value (not) and the coverage of feature space(not)
# but the indicator captured by PCA
#######the performace of m_BDRICM will decreace (49.83)and still different from original covariates if missing value records are ignored.
#round(apply(ov.pca.f, 2, function(x)sum(is.na(x))) /dim(ov.pca.f)[1],3) #number of null values
#tmp <- apply(ov.pca.s, 1, function(x)sum(is.na(x)))
#ov.pca.s <- ov.pca.s[tmp<1,]
ov.pca.s <- scale(ov.pca.f)
cMeans <- colMeans(ov.pca.f, na.rm=TRUE)
cStdev <- sapply(ov.pca.f, sd, na.rm=TRUE)
ov.pca.s[is.na(ov.pca.s)] <- 0
ov.pca.s <- as.data.frame(ov.pca.s)
formulaString.spc.f = as.formula(paste("~", paste(varsn, collapse="+")))
worldgrids.spc <- prcomp(formula=formulaString.spc.f, ov.pca.s)
worldgrids.spc$x <- worldgrids.spc$x[sample(1:dim(worldgrids.spc$x)[1], 10000),]
save(g.lst, gMeans, cMeans, cStdev, file="g.rda")
save(worldgrids.spc, file="worldgrids.spc.rda")
