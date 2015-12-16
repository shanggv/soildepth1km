# title         : com.celln.r
# purpose       : compare different n in sample.grid, called by com.celln.n.r;
# reference     :
# producer      : Prepared by W. Shangguan 
# address       : In Beijing.
# inputs        : prediction models
# outputs       : statistics ;
# remarks 1     : Takes several minutes;
for(fit.name in fit.names)
{
    n<- NULL
    r2.oob <- NULL
    r2 <- NULL
    me <- NULL
    rmse <- NULL
    r2.log <- NULL
    me.log <- NULL
    rmse.log <- NULL  
    imp.cov <- NULL
    
    for(i in 1:length(n.names))
    {
        n.name <- n.names[i]
        load( paste0("./model/m_BDRICM_", n.name, "p",PC.flag, "_a", arti.flag, "_s", soil.flag, "_", fit.name, "_", 1, ".rda")) 
        m <- m_BDRICM
        rm(m_BDRICM)
        m$prelog <- m$predicted
        m$ylog <- m$y
        m$predicted <- expm1(m$prelog)
        m$y <- expm1(m$ylog)
        n[[i]] <- length(m$ predicted)
        r2.oob[[i]] <- round(m$rsq[length(m$rsq)], digits=3)
        # orignal scale
        r2[[i]] <- cor(m$predicted, m$y)
        me[[i]] <- signif(mean(m$predicted - m$y), 3)
        rmse[[i]] <- signif(sqrt(mean((m$predicted - m$y) ^ 2)), 3)
        # log scale
        r2.log[[i]] <- cor(m$prelog, m$ylog)
        me.log[[i]] <- signif(mean(m$prelog - m$ylog), 3)
        rmse.log[[i]] <- signif(sqrt(mean((m$prelog - m$ylog) ^ 2)), 3)
        imp.cov[[i]] <- rownames(m$importance)[order(m$importance, decreasing = T)][1:10]
    }
    print(paste0(fit.name,":------------------"))
    print(n)
    print("r2.oob:")
    print(r2.oob)
    print("r2:")
    print(r2)
    print("importance of cov:")
    print(imp.cov)    
}
