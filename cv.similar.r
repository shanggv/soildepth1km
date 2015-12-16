# title         : cv.similar.R
# purpose       : calculate the similarity between extraplotion area and interpolation area;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Wageningen, NL.
# inputs        : points with overlayed covariates 
# outputs       :;
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
arti.flag <- 0
soil.flag <- 0
a.dir <- "/home/shang009/big/"# dir of the project
m.dir <- paste0(a.dir, "/soildepth")
source(paste0(a.dir, "/soildepth/code/head/functions.r"))
setwd(m.dir)




fit.names <- c("na",  "eu", "as")
n.name <- ""
#load data
sub.sp <- list(NULL)
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "ca", ".rda"))
tmp <- suba.sp
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "us", ".rda"))
sub.sp[[1]]<- rbind(tmp, suba.sp)
for ( i in 2:length(fit.names) )
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", fit.names[i], ".rda")) 
    suba.sp <- subset(suba.sp,  suba.sp$type == 2)
    sub.sp[[i]]<-suba.sp
}
names(sub.sp) <- fit.names


fit.name =  fit.names[1]
load(paste0("./model/m_", "BDRICM", "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_1.rda")) 
val.lst <- sub.sp
load(paste0("./profs/subs/subs.sp_", n.name, "p", PC.flag, "_a",arti.flag, "_", fit.name, ".rda")) 
val.lst[[fit.name]] <- subset( val.lst[[fit.name]], !(val.lst[[fit.name]]$SOURCEID %in% subs.sp$SOURCEID))

vars <- vs_BDRICM$topvars
vars <- subset(vars, substr(vars,1,3) !="LIT" &  substr(vars,1,3) !="ELF" &   substr(vars,4,6) !="ESA")

donor <- subs.sp@data[vars]
covd<- cov(donor)
Dc <- mahalanobis(donor, colMeans(donor), covd)^0.5
hist(Dc)
plot(density(Dc, bw = 0.5),
     main=" Mahalanobis distances")
thshold <- quantile(Dc, probs = 0.975)
tmp <- get_h_max(donor, vars)
hmax <- tmp$h_max
hmax
#0.02007895 max
mn_XXinv <- tmp$mn_XXinv



sim <- NULL
sims <- NULL
sim2 <- NULL
for(i in 1:length(val.lst))
{
    recip <- val.lst[[i]]@data[vars]
    Dr <- mahalanobis(recip, colMeans(donor), covd)^0.5
    sim[[i]] <- sum(Dr<thshold)/length(Dr)
    sims[[i]] <- Dr<thshold
    tmp <- check_feature_space(recip, vars, hmax, mn_XXinv)
    sim2[[i]] <- 1-tmp$frac
}
sim
sim2
plot(st.rg$r2.log.v,sim)
cor(st.rg$r2.log.v[4:16],sim[4:16], use ="complete.obs")

