## Function to fit models using nfold CV:
cv_nfold <- function(nf, rmat, nfold.x, fm.g, tvar, sub.N, cell.size, n){
   set.seed(10002)
   #nf <-3
   t.rmat <- rmat[!(nfold.x==nf),]
   v.rmat <- rmat[nfold.x==nf,]
   coordinates(t.rmat) <- ~LONWGS84 +LATWGS84
   proj4string(t.rmat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
   #get rid of spatial clustering
   t.rmat <-  as.data.frame(GSIF::sample.grid(t.rmat, cell.size = c(cell.size, cell.size), n = n)$sub)
   ## To speed things up, select only fixed random sub-sample:
   if(sub.N > nrow(t.rmat)){ sub.N <- nrow(t.rmat) }
   sel.s  <- sample(1:nrow(t.rmat), sub.N)
   t.rmat <- t.rmat[sel.s, ]

     ####Random forests modelling: mtry, 0.8 hour for BDRICM

   options(rf.cores=2, mc.cores=2)
   vs.rfs <- var.select(fm.g, t.rmat, ntree =200, seed = -2212 , nsplit = 10)
   if(tvar == "BDRLOG"){
     fm.g2 <- as.formula(paste0(tvar," ~ ",  paste(vs.rfs$topvars, collapse="+")))
   }else fm.g2 <- as.formula(paste0("log1p(", tvar, ") ~ ",  paste(vs.rfs$topvars, collapse="+")))
   try( m.rf <- randomForest(fm.g2, t.rmat, na.action = na.omit, ntree =300) )
   if(!class(.Last.value)[1]=="try-error"&!is.null(m.rf)){
     save(vs.rfs, m.rf, file = paste0("./cv/", tvar, "m.rf_", nf, "_p", PC.flag, "_a", arti.flag, "_s", soil.flag,  fit.name, ".Rda"))
     rf.pred <- predict(m.rf, v.rmat)
   } else {
     rf.pred <- rep(NA, nrow(v.rmat))
   }

#   load(paste0("./cv/", tvar, "m.rf_", nf, "_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name, ".Rda"))
#   rf.pred <- predict(m.rf, v.rmat)
    rm(m.rf)
    gc()
 if(m.flag == 1)
 {   
    if(tvar == "BDRLOG")
    {
        rk.pred <- NA 
        gbm.pred <- NA 
    }else {
        # Fit linear regression model using stepwise selection
        m.rk <- lm(fm.g, t.rmat)
        m.rk <- step(m.rk)
        save(m.rk, file = paste0("./cv/",tvar, "m.rk_", nf, "_p", PC.flag, "_a", arti.flag, "_s", soil.flag,  fit.name, ".Rda"))
#        load( paste0("./cv/",tvar, "m.rk_", nf, "_p", PC.flag, "_a", arti.flag, "_s", soil.flag,  fit.name, ".Rda"))
#        try(rk.pred <- predict(m.rk, v.rmat))  
        rm(m.rk)      
        # Stochastic Gradient Boosting  trees: interacion.depth, n.trees, shrinkage, 2 mins
        ###data should be t.rmat[,c(tvar,pr.lst)], or error happens
        try(m.gbm <- gbm(formula = fm.g2, data = t.rmat[,c(tvar,vs.rfs$topvars)], distribution = "gaussian", 
                        n.trees = 1000,cv.folds = 3 , n.cores = 1))
        if(!class(.Last.value)[1]=="try-error"&!is.null(m.gbm)){
         best.iter <- gbm.perf(m.gbm, method = "cv", plot.it = F)
         save(m.gbm, best.iter, file = paste0("./cv/", tvar, "m.gbm_", nf, "_p", PC.flag, "_a", arti.flag, "_s", soil.flag,  fit.name, ".Rda"))
         gbm.pred <- predict(m.gbm, v.rmat[ ,vs.rfs$topvars], best.iter)         
       } else {
         gbm.pred <- rep(NA, nrow(v.rmat))
       }
#        load(paste0("./cv/", tvar, "m.gbm_", nf, "_p", PC.flag, "_a", arti.flag, "_s", soil.flag, fit.name,  ".Rda"))
#        gbm.pred <- predict(m.gbm, v.rmat, best.iter) 
        rm(m.gbm)  
        gc()
        }
    #####randomForestSRC
    ####145 mins for fitting with 6G memorey, 100 mins for refitting with 4G,  for BDRICM  
    # some  serious bugs in predict.rfsrc(), but can still use var.select() to choose variables
    # Support Vector Machines with Radial Basis Function Kernel :  sigma, C
    #more than 5 hour for BDRICM, too much time
    ###caret::bagEarth: Bagged MARS(Muktivariate adaptive regression splines): degree, nprune
    #requires very big meomory, more than 24G for 30000 points with three predictors 
    ####party::cforest : Conditional Inference Random Forest: mtry
    #takes about 4.5 hours to predict 243,000 points with model fitted with 1000 points
 } 
    out.df <- data.frame(SOURCEID=v.rmat$SOURCEID,   rf.pred = rf.pred,
    meas=eval(fm.g[[2]], v.rmat), longitude=v.rmat$LONWGS84, latitude=v.rmat$LATWGS84)
    if(m.flag == 1)
    {
        out.df$rk.pred  = rk.pred
        out.df$gbm.pred = gbm.pred
    } 
   return(out.df)
}
