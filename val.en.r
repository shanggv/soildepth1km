# title         : val.en.R
# purpose       : validate that ensemble will improve;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : prediction models, water well points
# outputs       : rda files, png ;
# remarks 1     : Takes several minutes;
rm(list = ls(all = TRUE))
library(randomForest)
a.dir <- "/data/shang009/big"# dir of the project
m.dir <- paste0(a.dir, "/soildepth")
source(paste0(a.dir, "/soildepth/code/head/functions.r"))
setwd(m.dir) 
log.flag <- 1
PC.flag <- 0 # 0: not use the PC as predictors; 1: not use
arti.flag <- 1  #1: add artificial points; 0: not
fit.name <- "us" # c("eu", "as", "us", "ca", "all")
soil.flag <- 0 # 0: without soil profiles; 1: add soil profiles
en.num <- 1:10 #number of ensemble prediction

#load data
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", fit.name, ".rda")) 
load(paste0("./profs/subs/subs.sp_", PC.flag, "_", fit.name, ".rda"))
tmp <- NULL
for(i in en.num)
{
    tmp <- c(tmp,subs.sp[[1]]$SOURCEID)
}
tmp <- unique(tmp)
val.lst <- subset( suba.sp, !(suba.sp$SOURCEID %in% tmp))@data
rm(subs.sp,suba.sp)  


val.d <- data.frame(val.lst$BDRICM) 
names(val.d) <- "y"
val.d$ylog <- log1p(val.d$y)
val.p <- matrix(NA, nrow= length(val.d$y), ncol = length(en.num))
val.plog <- matrix(NA, nrow= length(val.d$y), ncol = length(en.num))
val.en <- matrix(NA, nrow= length(val.d$y), ncol = length(en.num))
val.enlog <- matrix(NA, nrow= length(val.d$y), ncol = length(en.num))
for(i in en.num){

    load(paste0("./model/rf/m_BDRICM_p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", i, ".rda"))   
    m <- m_BDRICM
    rm(m_BDRICM)
    dm <- predict(m, val.lst, na.action=na.pass)
    if(log.flag == 1){
        val.p[ ,i] <- as.integer(expm1(dm))
        val.plog[ ,i] <- dm
    }else{
        val.p[ ,i] <- as.integer(dm)
        val.plog[ ,i] <- as.integer(log1p(dm))
    } 
    if(i == 1){
    val.en[ ,i] <- val.p[, 1]
    val.enlog[ ,i] <- val.plog[, 1]
    }else{
        val.en[ ,i] <- rowMeans(val.p[, 1:i])
        val.enlog[ ,i] <- rowMeans(val.plog[, 1:i]) 
    }   
  
}
rm(val.lst,m, dm)

r2.v <- NULL
me.v <- NULL
rmse.v <- NULL
r2.log.v <- NULL
me.log.v <- NULL
rmse.log.v <- NULL
for(i in en.num)
{

    r2.v   <- c(r2.v, signif(cor(val.en[,i], val.d$y) ^ 2, 3))
    me.v   <- c(me.v, signif(mean(val.en[,i] - val.d$y), 3))
    rmse.v <- c(rmse.v, signif(sqrt(mean((val.en[,i] - val.d$y) ^ 2)), 3))
    r2.log.v   <- c(r2.log.v, signif(cor(val.enlog[,i], val.d$ylog) ^ 2, 3))
    me.log.v   <- c(me.log.v, signif(mean(val.enlog[,i] - val.d$ylog), 3))
    rmse.log.v <- c( rmse.log.v, signif(sqrt(mean((val.enlog[,i] - val.d$ylog) ^ 2)), 3))
}
rm( val.p, val.plog)
save.image("./cv/val.en.rdata")

plot(r2.v)
par(new = T)
plot(me.v,yaxt="n",ylab="")
axis(4)
plot(rmse.v)
plot(r2.log.v)
plot(me.log.v)
plot(rmse.log.v)

load(("./cv/val.en.rdata"))
#plot Number of new points for subsets
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "all", ".rda")) 
tmp <- splitsp(suba.sp, cellsize = 0.1, n = 3, sn = 15)

par.old <- par()
par( mar = c(5.1, 4.1, 4.1, 2.1))
plot(tmp$new.n[1:10], ylab = "Number of new points", xlab = "Subset number")
dev.copy(png,paste0("./pics/subsnew.png"), width = 580, height = 550, units = "px")
dev.off()
plot(r2.log.v*100, ylab="variace Explained(%)", pch = 2, xlab = "Number of Subset")
dev.copy(png,paste0("./pics/subsr2.png"), width = 580, height = 550, units = "px")
dev.off()
par(par.old)
