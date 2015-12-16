# title         : cv.region2.R
# purpose       : goodness of fitting and cross validation by regions from continent to states;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Beijing
# inputs        : points with overlayed covariates 
# outputs       : compressed RDA files and tiff files ;
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
t.lst <- c("BDRICM")
m.lst <- paste("m_", t.lst, sep = "")



#####continnents
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

#cross validation
for ( i in 1:length(fit.names) )
{
    fit.name <- fit.names[i]
    source(paste0(a.dir, "/soildepth/code/1km/cv.region2.r"))
}

#######countries
fit.names <- c("ca", "us")
n.name <- ""
#load data
sub.sp <- list(NULL)
for ( i in 1:length(fit.names) )
{
    load(paste0("./profs/subs/suba.sp_", PC.flag, "_", fit.names[i], ".rda")) 
    suba.sp <- subset(suba.sp,  suba.sp$type == 2)
    sub.sp[[i]]<-suba.sp
}
names(sub.sp) <- fit.names

#cross validation
for ( i in 1:length(fit.names) )
{
    fit.name <- fit.names[i]
    source(paste0(a.dir, "/soildepth/code/1km/cv.region2.r"))
}

fit.names <- c("se", "ie")
n.name <- ""
#load data
sub.sp <- list(NULL)
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "eu", ".rda"))
suba.sp <- subset(suba.sp,  suba.sp$type == 2) 
sub.sp[[1]] <- subset(suba.sp,  suba.sp$LONWGS84 > 0)
sub.sp[[2]] <- subset(suba.sp,  suba.sp$LONWGS84 < 0)
names(sub.sp) <- fit.names

#cross validation
for ( i in 1:length(fit.names) )
{
    fit.name <- fit.names[i]
    source(paste0(a.dir, "/soildepth/code/1km/cv.region2.r"))
}

######states in US
#fit.names <- c("us1", paste0("us", 3:17))
#n.name <- ""
##load data
#sub.sp <- list(NULL)
#load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "us", ".rda")) 
#
#for ( i in 1:length(fit.names) )
#{
#     sub.sp[[i]] <- subset(suba.sp, suba.sp$source==as.integer(substr(fit.names[i], 3,5))) 
#}
#names(sub.sp) <- fit.names
#
##cross validation
#for ( i in 1:length(fit.names) )
#{
#    fit.name <- fit.names[i]
#    source(paste0(a.dir, "/soildepth/code/1km/cv.region2.r"))
#}
#####province in Canada
#fit.names <- c("ca1", paste0("ca", 3:5))
#n.name <- ""
##load data
#sub.sp <- list(NULL)
#load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "ca", ".rda")) 
#
#for ( i in 1:length(fit.names) )
#{
#     sub.sp[[i]] <- subset(suba.sp, suba.sp$source==as.integer(substr(fit.names[i], 3,5))) 
#}
#names(sub.sp) <- fit.names
#
##cross validation
#for ( i in 1:length(fit.names) )
#{
#    fit.name <- fit.names[i]
#    source(paste0(a.dir, "/soildepth/code/1km/cv.region2.r"))
#}



#####states in US, n.name <- 27
fit.names <- c("us1", paste0("us", 3:17))
n.name <- 27
#load data
sub.sp <- list(NULL)
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "us", ".rda")) 
suba.sp <- subset(suba.sp,  suba.sp$type == 2)
for ( i in 1:length(fit.names) )
{
     sub.sp[[i]] <- subset(suba.sp, suba.sp$source==as.integer(substr(fit.names[i], 3,5))) 
}
names(sub.sp) <- fit.names

#cross validation
for ( i in 1:length(fit.names) )
#for(i in c(1:4,6,7,9:13,16))
{
    fit.name <- fit.names[i]
    source(paste0(a.dir, "/soildepth/code/1km/cv.region2.r"))
}
#us6,us9,us15,16 bugs

####province in Canada, n.name <- 27
fit.names <- c("ca1", paste0("ca", 3:5))
n.name <- 27
#load data
sub.sp <- list(NULL)
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "ca", ".rda")) 
suba.sp <- subset(suba.sp,  suba.sp$type == 2)
for ( i in 1:length(fit.names) )
{
     sub.sp[[i]] <- subset(suba.sp, suba.sp$source==as.integer(substr(fit.names[i], 3,5))) 
}
names(sub.sp) <- fit.names

#cross validation
for ( i in 1:length(fit.names) )
{
    fit.name <- fit.names[i]
    source(paste0(a.dir, "/soildepth/code/1km/cv.region2.r"))
}

###############leave one out
#####states in US
fit.names <- c("mus1", paste0("mus", 3:17))
n.name <- "m3"
#load data
sub.sp <- list(NULL)
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "us", ".rda")) 
suba.sp <- subset(suba.sp,  suba.sp$type == 2)
for ( i in 1:length(fit.names) )
{
     sub.sp[[i]] <- subset(suba.sp, suba.sp$source==as.integer(substr(fit.names[i], 4,6))) 
}
names(sub.sp) <- fit.names

#cross validation
for ( i in 1:length(fit.names) )
{
    fit.name <- fit.names[i]
    source(paste0(a.dir, "/soildepth/code/1km/cv.region2.r"))
}


####province in Canada
fit.names <- c("mca1", paste0("mca", 3:5))
n.name <- "m3"
#load data
sub.sp <- list(NULL)
load(paste0("./profs/subs/suba.sp_", PC.flag, "_", "ca", ".rda")) 
suba.sp <- subset(suba.sp,  suba.sp$type == 2)
for ( i in 1:length(fit.names) )
{
     sub.sp[[i]] <- subset(suba.sp, suba.sp$source==as.integer(substr(fit.names[i], 4,6))) 
}
names(sub.sp) <- fit.names

#cross validation
for ( i in 1:length(fit.names) )
{
    fit.name <- fit.names[i]
    source(paste0(a.dir, "/soildepth/code/1km/cv.region2.r"))
}


#similarity
tr2.log <- NULL
tme.log <- NULL
trmse.log <-NULL
tacc <- NULL
tacc2 <- NULL
tsim <- NULL
tsim2 <- NULL
fit.names <- c("na",  "eu", "as")
n.name <- ""
for(i in 1:length(fit.names))
{
    load(paste0("./model/st_", "BDRICM", "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.names[i], ".rda"))
    tr2.log <- c(tr2.log,st.rg$r2.log.v[-i])
    tme.log <- c(tme.log,st.rg$me.log.v[-i])
    trmse.log <- c(trmse.log,st.rg$rmse.log.v[-i])
    tsim <- c(tsim,st.rg$sim[-i])
    tsim2 <- c(tsim2,st.rg$sim2[-i])
    tmp <- unlist(lapply(st.rg$val.c, function(x)x$sump1))*100
    tacc <-  c(tacc, tmp[-i])
    tmp <- unlist(lapply(st.rg$val.c, function(x)x$sump2))*100
    tacc2 <- c(tacc2, tmp[-i])
}
fit.names <- c("ca",  "us")
for(i in 1:length(fit.names))
{
    load(paste0("./model/st_", "BDRICM", "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.names[i], ".rda"))
    tr2.log <- c(tr2.log,st.rg$r2.log.v[-i])
    tme.log <- c(tme.log,st.rg$me.log.v[-i])
    trmse.log <- c(trmse.log,st.rg$rmse.log.v[-i])
    tsim <- c(tsim,st.rg$sim[-i])
    tsim2 <- c(tsim2,st.rg$sim2[-i])
    tmp <- unlist(lapply(st.rg$val.c, function(x)x$sump1))*100
    tacc <-  c(tacc, tmp[-i])
    tmp <- unlist(lapply(st.rg$val.c, function(x)x$sump2))*100
    tacc2 <- c(tacc2, tmp[-i])
}
fit.names <- c("se",  "ie")
for(i in 1:length(fit.names))
{
    load(paste0("./model/st_", "BDRICM", "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.names[i], ".rda"))
    tr2.log <- c(tr2.log,st.rg$r2.log.v[-i])
    tme.log <- c(tme.log,st.rg$me.log.v[-i])
    trmse.log <- c(trmse.log,st.rg$rmse.log.v[-i])
    tsim <- c(tsim,st.rg$sim[-i])
    tsim2 <- c(tsim2,st.rg$sim2[-i])
    tmp <- unlist(lapply(st.rg$val.c, function(x)x$sump1))*100
    tacc <-  c(tacc, tmp[-i])
    tmp <- unlist(lapply(st.rg$val.c, function(x)x$sump2))*100
    tacc2 <- c(tacc2, tmp[-i])
}

tsim <- tsim*100
tsim2 <- tsim2*100



bitmap(paste0("./pics/", "CCsim.tiff"), width = 7.48, height = 6, units = "in", res =1000, type = "tiffcrle", pointsize =11)
par(mfrow=c(2,2),mar=c(4, 4, 1, 0.1))
plot(tsim,tr2.log, xlab = "Similarity (%)", ylab=expression(R^2), main = paste0("R=",signif(cor(tr2.log,tsim),2)))
summary(lm(tr2.log~tsim))
cor.test(tr2.log,tsim)
plot(tsim,abs(tme.log), xlab = "Similarity (%)", ylab="Absoluse mean error", main = paste0("R=",signif(cor(abs(tme.log),tsim),2)))
summary(lm(abs(tme.log)~tsim))
plot(tsim,trmse.log, xlab = "Similarity (%)", ylab="RMSE", main = paste0("R=",signif(cor(trmse.log,tsim),2)))
summary(lm(trmse.log~tsim))
plot(tsim,tacc, xlab = "Similarity (%)", ylab="Overall accuracy (%)", main = paste0("R=",signif(cor(tacc,tsim),2)))
#text(50,10,expression(y==10.59+0.045*x(R^2==0.27, p==0.35)))
summary(lm(tacc~tsim))
#dev.copy(tiff,paste0("./pics/", "CCsim.tiff"),   width = 7.48, height = 3, units = "in",res =300)     
dev.off()  
par(mfcol=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1)) 



cor(tsim,tr2.log, use ="complete.obs")
cor(tsim,tacc, use ="complete.obs")



fit.names <- c("us1", paste0("us", 3:17))
n.name <- 27
####get coordinates of centroid of each state
library(maptools)
xx <- readShapePoly("./profs/USA_adm1.shp",
 IDvar="ID_1", proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
xx <- spTransform(xx,
 CRS=CRS("+proj=laea +lat_0=50 +lon_0=-100 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs ")) 

cxy <- getSpPPolygonsLabptSlots(xx)[c(2,15,16,17,18,20,24,26,29,30,33,36,28,39,43,46),]
cxy[4,] <- c(-98.4,37.88)
cxy[13,] <- c(-100.8,41.1)
tr2.log <- NULL
tme.log <- NULL
trmse.log <-NULL
tacc <- NULL
tacc2 <- NULL
tsim <- NULL
tsim2 <- NULL
tdis <- NULL
tlon <- NULL
tlat <- NULL
for(i in 1:length(fit.names))
{
if(i !=9 && i !=1)
{

    load(paste0("./model/st_", "BDRICM", "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.names[i], ".rda"))
    tmp1 <- abs(cxy[,1]-cxy[i,1])
    tlon <- c(tlon, tmp1[-i])
    tmp2 <- abs(cxy[,2]-cxy[i,2])
    tlat <- c(tlat, tmp2[-i])    
    tmp3 <- (tmp1^2 +tmp2^2)^0.5
    tdis <- c(tdis, tmp3[-i])   
    tr2.log <- c(tr2.log,st.rg$r2.log.v[-i])
    tme.log <- c(tme.log,st.rg$me.log.v[-i])
    trmse.log <- c(trmse.log,st.rg$rmse.log.v[-i])
    tsim <- c(tsim,st.rg$sim[-i])
    tsim2 <- c(tsim2,st.rg$sim2[-i])
    
    tmp <- unlist(lapply(st.rg$val.c, function(x){
        if(is.na(x)) y <- NA else y <- x$sump1
        return(y)
    }))*100
    tacc <- c(tacc,tmp[-i]) 
    
#    tr2.log <- st.rg$r2.log.v[-i]
#    tsim <- st.rg$sim[-i] 
#    tsim2 <- st.rg$sim2[-i]
#    tlon <- tmp1[-i]
#    tlat <- tmp2[-i]
#    tdis <- tmp3[-i] 
#    tacc <- tmp[-i]
    
    tmp <-  unlist(lapply(st.rg$val.c, function(x){
        if(is.na(x)) y <- NA else y <- x$sump2
        return(y)*100
    }))
    tacc2 <-   c(tacc2,tmp[-i]) 
    print(fit.names[i])#
    #plot(tsim[which(tsim>0)],tr2.log[which(tsim>0)], main = fit.names[i])
#    print(cor(tlon,tacc, use ="complete.obs"))
#    print(cor(tlat,tacc, use ="complete.obs"))
#    print(cor(tdis,tacc, use ="complete.obs"))
#    print(cor(tsim,tr2.log, use ="complete.obs"))
#    print(cor(tsim,tacc, use ="complete.obs"))
    
}    
}
tsim <- tsim*100
tsim2 <- tsim2*100
tdis <- tdis/1000

cor(tsim,tr2.log, use ="complete.obs")
cor(tsim,abs(tme.log), use ="complete.obs")
cor(tsim,trmse.log, use ="complete.obs")
cor(tsim,tacc, use ="complete.obs")
cor(tsim2,tr2.log, use ="complete.obs")
cor(tsim2,tacc, use ="complete.obs")
cor(tacc,tdis, use ="complete.obs")
cor(tacc,tlon, use ="complete.obs")
cor(tacc,tlat, use ="complete.obs")
cor(tr2.log,tdis, use ="complete.obs")
cor(trmse.log,tdis, use ="complete.obs")

summary(lm(tr2.log~tsim))
summary(lm(abs(tme.log)~tsim))
summary(lm(trmse.log~tsim))
summary(lm(tacc~tsim))

summary(lm(tr2.log~tdis))
summary(lm(abs(tme.log)~tdis))
summary(lm(trmse.log~tdis))

bitmap(paste0("./pics/", "USdis.tiff"), width = 7.48, height = 6, units = "in", res =1000, type = "tiffcrle", pointsize =11)

par(mar=c(4, 4, 2, 1.1),mfrow=c(2,2))
plot(tdis,abs(tr2.log), xlab = "Distance(km)", ylab=expression(R^2), xlim = c(0,2.5e3),
    main = expression(y==0.031-0.000054*x(R^2==0.006, p==0.29)));abline(lm(abs(tr2.log)~tdis))
plot(tdis,abs(tme.log), xlab = "Distance(km)", ylab="Absolute Mean Error", xlim = c(0,2.5e3),
    main = expression(y==0.24+0.00044*x(R^2==0.15, p<0.001)));abline(lm(abs(tme.log)~tdis))
plot(tdis,trmse.log, xlab = "Distance(km)", ylab="RMSE", xlim = c(0,2.5e3),
    main = expression(y==1.17+0.00027*x(R^2==0.11, p<0.001)));abline(lm(trmse.log~tdis))
plot(tdis,tacc, xlab = "Distance(km)", ylab="Overall accuracy (%)", xlim = c(0,2.5e3),
    main = expression(y==17.25-0.0034*x(R^2==0.15, p<0.001)));abline(lm(tacc~tdis))
summary(lm(tacc~tdis))
summary(lm(abs(tme.log)~tdis))
summary(lm(trmse.log~tdis) ) 
dev.off()  
par(mar=c(5.1, 4.1, 4.1, 2.1),mfcol=c(1,1)) 


bitmap(paste0("./pics/", "USsim.tiff"), width = 7.48, height = 6, units = "in", res =1000, type = "tiffcrle", pointsize =11)

par(mar=c(4, 4, 2, 1.1),mfrow=c(2,2))
plot(tsim,abs(tr2.log), xlab = "Distance(km)", ylab=expression(R^2), xlim = c(0,2.5e3),
    main = expression(y==0.031-0.000054*x(R^2==0.006, p==0.29)));abline(lm(abs(tr2.log)~tdis))
plot(tsim,abs(tme.log), xlab = "Distance(km)", ylab="Absolute Mean Error", xlim = c(0,2.5e3),
    main = expression(y==0.24+0.00044*x(R^2==0.15, p<0.001)));abline(lm(abs(tme.log)~tdis))
plot(tsim,trmse.log, xlab = "Distance(km)", ylab="RMSE", xlim = c(0,2.5e3),
    main = expression(y==1.17+0.00027*x(R^2==0.11, p<0.001)));abline(lm(trmse.log~tdis))
plot(tdis,tacc, xlab = "Distance(km)", ylab="Overall accuracy (%)", xlim = c(0,2.5e3),
    main = expression(y==17.25-0.0034*x(R^2==0.15, p<0.001)));abline(lm(tacc~tdis))
summary(lm(tacc~tdis))
summary(lm(abs(tme.log)~tdis))
summary(lm(trmse.log~tdis) ) 
dev.off()  
par(mar=c(5.1, 4.1, 4.1, 2.1),mfcol=c(1,1)) 


plot(tlon,tacc)
tsimc<-NULL
for(i in 1:length(tsim))
{
    if(is.na(tsim[i])) tsimc[[i]]=NA else
    if(tsim[i]==0) tsimc[[i]]=0 else
    if(tsim[i]<0.2) tsimc[[i]]=1 else
    if(tsim[i]<0.4) tsimc[[i]]=2 else
    if(tsim[i]<0.6) tsimc[[i]]=3 else
    if(tsim[i]<0.8) tsimc[[i]]=4 else tsimc[[i]]=5
}
bitmap(paste0("./pics/", "USsimaccbox.tiff"), width = 5.5, height = 5, units = "in", res =1000, type = "tiffcrle", pointsize =11)
par(mfcol=c(1,1),mar=c(4, 4, 0.1, 0.1))  
boxplot(tacc~tsimc, outline = F, xlab = "Similarity Class (%)", ylab = "Overall accuracy (%)",
 names=c("0","0~20","20~40","40~60","60~80","80~100"))
#dev.copy(tiff,paste0("./pics/", "USsimaccbox.tiff"),  width = 550, height = 500, units = "px")     
dev.off() 
boxplot(tr2.log~tsimc, outline = F, xlab = "Similarity Class (%)", ylab = "Overall accuracy (%)",
 names=c("0","0~20","20~40","40~60","60~80","80~100"))   
par(mfcol=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1))  

fit.names <- c("mus1", paste0("mus", 3:17))
n.name <- "m3"
  tr2.log <- NULL
    tacc <- NULL
    tacc2 <- NULL
    tsim <- NULL
for(i in 1:length(fit.names))
{

  
    load(paste0("./model/st_", "BDRICM", "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.names[i], ".rda"))
    tr2.log <- c(tr2.log, st.rg$r2.log.v[i])
    tsim <- c(tsim,st.rg$sim[i])
    tmp <- unlist(lapply(st.rg$val.c, function(x){
        if(is.na(x)) y <- NA else y <- x$sump1
        return(y)
    }))*100
    tacc <- c(tacc,tmp[i]) 
    tmp <-  unlist(lapply(st.rg$val.c, function(x){
        if(is.na(x)) y <- NA else y <- x$sump2
        return(y)*100
    }))
    tacc2 <-   c(tacc2,tmp[i])
    print(fit.names[i])
   
  
}
 plot(tsim[which(tsim>0)],tr2.log[which(tsim>0)], main = fit.names[i])
 print(cor(tsim,tr2.log, use ="complete.obs"))
 print(cor(tsim,tacc, use ="complete.obs"))
 print(cor(tsim,tacc2, use ="complete.obs"))

cor(tsim,tr2.log, use ="complete.obs")
cor(tsim,tacc, use ="complete.obs")
cor(tsim,tacc2, use ="complete.obs")

#####collect the statistics
#us
fit.names <- c("us1", paste0("us", 3:17))
n.name <- 27

r2.oob <- NULL
r2.log <- NULL
me.log <- NULL
rmse.log <- NULL
fit.acc <- NULL
r2.log.v <- matrix(data = NA, nrow = length(fit.names), ncol = length(fit.names))
val.acc <- matrix(data = NA, nrow = length(fit.names), ncol = length(fit.names))
me.log.v <- matrix(data = NA, nrow = length(fit.names), ncol = length(fit.names))
rmse.log.v <- matrix(data = NA, nrow = length(fit.names), ncol = length(fit.names))


for ( i in 1:length(fit.names) )
{
    load(paste0("./model/st_", "BDRICM", "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.names[i], ".rda"))
    r2.oob[[i]] <- signif(st.rg$r2.oob,3)
    r2.log[[i]] <- signif(st.rg$r2.log,3)
    me.log[[i]] <- signif(st.rg$me.log,3)
    rmse.log[[i]] <- signif(st.rg$rmse.log,3)
    fit.acc[[i]] <- signif(st.rg$fit.c[[1]]$sump1, 3)*100
    r2.log.v[i,] <- sapply(st.rg$r2.log.v, FUN =signif, digits =3)
    me.log.v[i,] <- sapply(st.rg$me.log.v, FUN =signif, digits =3)
    rmse.log.v[i,] <- sapply(st.rg$rmse.log.v, FUN =signif, digits =3)
    for( j in 1:length(fit.names))
    {
        if(!is.na(st.rg$val.c[[fit.names[j]]])) val.acc[i,j] <- signif(st.rg$val.c[[fit.names[j]]]$sump1,3)*100
    }
}
signif(cbind(r2.log,r2.oob,fit.acc,r2.log.v,val.acc),3)
signif(cbind(me.log,rmse.log,me.log.v,rmse.log.v),3)
#mus
fit.names <- c("mus1", paste0("mus", 3:17))
n.name <- "m3"

r2.oob <- NULL
r2.log <- NULL
me.log <- NULL
rmse.log <- NULL
fit.acc <- NULL
r2.log.v <- matrix(data = NA, nrow = length(fit.names), ncol = length(fit.names))
val.acc <- matrix(data = NA, nrow = length(fit.names), ncol = length(fit.names))
me.log.v <- matrix(data = NA, nrow = length(fit.names), ncol = length(fit.names))
rmse.log.v <- matrix(data = NA, nrow = length(fit.names), ncol = length(fit.names))

for ( i in 1:length(fit.names) )
{
    load(paste0("./model/st_", "BDRICM", "_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.names[i], ".rda"))
    r2.oob[[i]] <- signif(st.rg$r2.oob,3)
    r2.log[[i]] <- signif(st.rg$r2.log,3)
    me.log[[i]] <- signif(st.rg$me.log,3)
    rmse.log[[i]] <- signif(st.rg$rmse.log,3)
    fit.acc[[i]] <- signif(st.rg$fit.c[[1]]$sump1, 3)*100
    r2.log.v[i,] <- sapply(st.rg$r2.log.v, FUN =signif, digits =3)
    me.log.v[i,] <- sapply(st.rg$me.log.v, FUN =signif, digits =3)
    rmse.log.v[i,] <- sapply(st.rg$rmse.log.v, FUN =signif, digits =3)
    for( j in 1:length(fit.names))
    {
        if(!is.na(st.rg$val.c[[fit.names[j]]])) val.acc[i,j] <- signif(st.rg$val.c[[fit.names[j]]]$sump1,3)*100
    }
}
signif(cbind(r2.log,r2.oob,fit.acc,r2.log.v,val.acc),3)
signif(cbind(me.log,rmse.log,me.log.v,rmse.log.v),3)

r2.log.vi <- NULL
r2.log.ve <- NULL
me.log.vi <- NULL
me.log.ve <- NULL
rmse.log.vi <- NULL
rmse.log.ve <- NULL
val.acci <- NULL
val.acce <- NULL
for ( i in 1:length(fit.names) )
{
    for( j in 1:length(fit.names))
    {
        if(i==j) 
        {
            r2.log.ve[[i]] <- r2.log.v[i,j]
            r2.log.v[i,j]<- NA
            me.log.ve[[i]] <- me.log.v[i,j]
            me.log.v[i,j]<- NA
            rmse.log.ve[[i]] <- rmse.log.v[i,j]
            rmse.log.v[i,j]<- NA
            val.acce[[i]] <- val.acc[i,j]
            val.acc[i,j]<- NA
        }
    }
}
r2.log.vi <-colMeans(r2.log.v, na.rm =T)
me.log.vi <-colMeans(me.log.v, na.rm =T)
rmse.log.vi <-colMeans(rmse.log.v, na.rm =T)
val.acci <- colMeans(val.acc, na.rm =T)
signif(cbind(r2.log,r2.oob,fit.acc,r2.log.vi,r2.log.ve,val.acci,val.acce),3)
signif(cbind(me.log,rmse.log,me.log.vi,me.log.ve,rmse.log.vi,rmse.log.ve),3)
