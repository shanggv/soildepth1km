do_unzip <- function()#unzip zip files
{
    zfiles <- shell("dir *.zip /B", intern = T)
    for(i in 1 :length(zfiles))
    {
        unzip(zfiles[i], exdir = "tmp")
    }
}

del_unzip <- function()#delete unzipped files
{
    cmd <- paste("rmdir /S /Q ", "tmp", sep = "")
    shell(cmd)
}

form_rec <- function(rec, n) #get the universay format of rec
{
    if(!is.data.frame(rec)) rec <- as.data.frame(rec)
    if(sum(as.numeric(row.names(rec)), na.rm = TRUE)>0) row.names(rec) <- NULL 
    i <- sapply(rec, is.factor)
    rec[i] <- lapply(rec[i], as.character)
    i <- sapply(rec, is.character)
    i[length(i)] <- FALSE
    rec[i] <- lapply(rec[i], as.numeric)
    rec <- cbind(n, rec)
    colnames(rec) <- c_names
    rec
}

getkml<-function(sd, sc, anames)
{
    out <- anames
    sd  <- as.character(sd)
    tmp <- str_sub(sd, start=325, end=-200)
    tmp <- unlist(str_split(tmp, "</td> <td width=\"205\">"))
    tmp <- unlist(str_split(tmp, "</td> </tr> <tr> <td width=\"210\">"))
    tmp <- unlist(str_split(tmp, 
        "</td> </tr> <tr bgcolor=\"#EBEBEB\"> <td width=\"210\">"))
    len <- length(tmp)
    j <- 1
    for(i in 1:length(anames))
    {
        while(j < len)
        {
            #if(str_detect(tmp[j], anames[i]))
            if(tmp[j] == anames[i])
            {
                out[i] <- tmp[j+1]
                j <- j + 2
                break
            }
            j <- j + 2
        }
    }
    c(out,sc[1:2])
}

print_0 <- function(rec)
{
    print(paste(sum(rec == 0), "out of", length(rec), "is 0", sep = " "))
}

getkml2 <- function(sd, sc, anames)
{
    out <- anames
    sd  <- as.character(sd)
    tmp <- str_sub(sd, start=103, end = -24)
    tmp <- unlist(str_split(tmp, " </tr><tr bgcolor=\"\"> "))
    tmp <- unlist(str_split(tmp, " </tr><tr bgcolor=\"#E3E3F3\"> "))
    tmp <- unlist(str_split(tmp, "</th> <td>"))
    len <- length(tmp)
    seqt <- seq(1, len, by = 2)
    tmp[seqt] <- str_sub(tmp[seqt], start = 5)
    tmp[seqt + 1] <- str_sub(tmp[seqt+1], end = -6)    
    j <- 1
    for(i in 1:length(anames))
    {
        while(j < len)
        {
            if(anames[i] == tmp[j])
            {
                out[i] <- tmp[j + 1]
                j <- j + 2
                break
            }
            j <- j + 2
        }
    }
    c(out, sc[1:2])   
}

getcoords <- function(coords)
{
    xmax <- max(coords[, 1])
    xmin <- min(coords[, 1])
    ymax <- max(coords[, 2])
    ymin <- min(coords[, 2])
    c(xmax,xmin, ymax, ymin)
}

get_coords2 <- function(no)
{
    xpath <- c("//a[@id='ctl00_ContentPlaceHolder1_hlMapit']")
    url <- "http://www2.des.state.nh.us/DESOnestop/WLSDetail.aspx?ID="
    url <- paste0(url, no) 
    map <- crawler1(url,xpath, content = "href")
    map <- as.character(map)
    ll  <- as.numeric(unlist(str_split(str_sub(map, str_locate(map, 
        "q=")[, 2]+1), ",")))
    data.frame(WRB_NUMBER = no, lon = ll[2], lat = ll[1]) 
}

qsec_box <- function(box, qsec)
{
    xdis <- box$xmax - box$xmin
    ydis <- box$ymax - box$ymin
    box2 <- box
    if(qsec == "NW")
    {
        box2$xmax <- box$xmin + xdis/2
        box2$ymin <- box$ymax - ydis/2
    }else if(qsec == "NE")
    {
        box2$xmin <- box$xmax - xdis/2
        box2$ymin <- box$ymax - ydis/2
    }else if(qsec == "SW")
    {
        box2$xmax <- box$xmin + xdis/2
        box2$ymax <- box$ymin + ydis/2   
    }else if(qsec == "SE")
    {
        box2$xmin <- box$xmax - xdis/2
        box2$ymax <- box$ymin + ydis/2
    }else if(qsec == "N")
    {
        box2$ymin <- box$ymax - ydis/2    
    }else if (qsec == "S")
    {
        box2$ymax <- box$ymin + ydis/2
    }else if(qsec == "W")
    {
        box2$xmax <- box$xmin + xdis/2    
    }else if (qsec == "E")
    {
        box2$xmin <- box$xmax - xdis/2
    }
    box2
} 


EDA <- function(x, qqplot = TRUE, main = NULL)
{
    library(e1071)
    par(mar = rep(2, 4))
    par(mfrow = c(2,2))
   
    hist(x, breaks = 50)
    hist(x, xlim =  quantile(x, probs = c(0.05, 0.95)), main = main,
        breaks = (max(x)-min(x))/(quantile(x, probs = c(0.95))-quantile(x, probs = c(0.05)))*50)
    if(qqplot){
     qqnorm(x)
     qqline(x)  
    }     
    print("summary statistics:")
    print(summary(x))
    print("The skewness statistics:")
    print(skewness(x))  
    plot(1, col = "white")
    text(0.6, 1.3, paste0("Total number: ",length(x)), pos = 4)
    text(0.6, 1.2, "summary statistics:", pos = 4) 
    text(0.6, 1.1, paste0(names(summary(x)), collapse = ";   "), pos = 4)
    text(0.6, 1.0, paste0(summary(x), collapse = ";   "), pos = 4)  
    text(0.6, 0.9, "The skewness statistics:", pos = 4)
    text(0.6, 0.8,paste0(round(skewness(x)),3), pos = 4)
    par(mfrow = c(1,1))    
}



subsp <- function(obj, cellsize, n) # get subset of spatial points with at least n points in each cell
{

    obj$gid <- 1:length(obj)
    bbox <- obj@bbox
    x <- GridTopology(cellcentre.offset=bbox[,1],
            cellsize=c(cellsize,cellsize),
            cells.dim=c(ceiling(abs(diff(bbox[1,])/cellsize)),
            ncols=ceiling(abs(diff(bbox[2,])/cellsize))))
    r.sp <- SpatialGrid(x, proj4string = obj@proj4string)
    r <- raster(r.sp)
    in.r <- rasterize(obj, r, field = "gid")
    grd <- as(in.r, "SpatialGridDataFrame")
    ov <- over(obj, grd)
    pnts <- tapply(obj$gid, ov, function(x, n){
        if(length(x) <= n) x
        else sample(x,n)
        }, n=n)
    pnts <- unlist(pnts, use.names = FALSE)
    sub <- obj[obj$gid %in% pnts, ]
    sub$gid <- NULL
    return(sub)
}
#####test
#obj <- wells.depth, length(obj) == 1247976
#cellsize <- 0.1,n <- 5 : 97493 points and 35s on PC
#cellsize <- 0.5, n <- 5 : 10030 points and 28s on PC



splitsp <- function(obj,        #SpatialPointDataframe
                    cellsize,   #cellsize
                    n ,         #the minmun number of samples to take in each cell
                    sn = 10          #number of subsets in spliting
                    )
{

    obj$gid <- 1:length(obj)
    bbox <- obj@bbox
    x <- GridTopology(cellcentre.offset=bbox[,1],
            cellsize=c(cellsize,cellsize),
            cells.dim=c(ceiling(abs(diff(bbox[1,])/cellsize)),
            ncols=ceiling(abs(diff(bbox[2,])/cellsize))))
    r.sp <- SpatialGrid(x, proj4string = obj@proj4string)
    r <- raster(r.sp)
    in.r <- rasterize(obj, r, field = "gid")
    grd <- as(in.r, "SpatialGridDataFrame")
    ov <- over(obj, grd)
    p1 <- tapply(obj$gid, ov, function(x, n){ #get the first subset and the rest points by cell
            if(length(x) <= n)
            {
                sub <- x
                left <- NULL
            }
            else
            {
                xsample <- sample(x)
                sub  <- xsample[1:n]
                left <- xsample[-1:-n]
            }
            return(list(sub = sub, left = left))
        }, n = n)
    p2 <- lapply(p1,function(x, sn){ #get subsets by keeping the minum points and subsetting randomly
            subs <- matrix(-1, nrow=sn, ncol=n)
            indicator <- 0
            new.n <- rep(0,sn)
            for(i in 1:sn)
            {

                if(is.null(x$left))
                {
                    sublen <- length(x$sub)
                    if(sublen <= n)
                    {
                        subs[i, 1:sublen] <- x$sub

                    }else{
                        subs[i,] <- sample(x$sub, n)
                    }
                    if (indicator == 0)indicator <- i   #no samples left anymore
                    new.n[i] <- 0
                }else{
                    leftlen <- length(x$left)
                    if(leftlen <= n)
                    {
                        subs[i,] <- c(x$left, sample(x$sub, n-leftlen))
                        x$sub  <- c(x$sub, x$left)
                        x$left <- NULL
                        indicator <- i #no samples left anymore
                        new.n[i] <- leftlen
                    }else{
                        leftsample <- sample(x$left)
                        subs[i,] <- leftsample[1:n]
                        x$sub  <- c(x$sub, leftsample[1:n])
                        x$left <- leftsample[-1:-n]
                        new.n[i] <- n
                    }
                }
            }
            res <- list(subs = subs, new.n = new.n, left = x$left, indicator = indicator)
            return(res)
        }, sn =sn)


    subsplit <- NULL
    for(i in 1:sn)
    {
        p3 <- lapply (p2, function(x, n) x$subs[n,], n=i)
        p3 <- unlist(p3, use.names = F)
        p3 <- p3[p3>0]
        subsplit <- rbind(subsplit,p3)
    }

    new.n <- rep(0,sn)
    new.n[1] <- ncol(subsplit)
    message(paste0("The size of each subsets is ", new.n[1]))
    for(i in 2:sn)
    {
        new.nt <- lapply(p2,function(x,i)x$new.n[i], i =i )
        new.nt <- unlist(new.nt, use.names = F)
        new.n[i] <- sum(new.nt)
        message (paste0("Subset", i, " has ", new.n[i], " new samples compared to previous subset."))
    }
    pleft <- lapply(p2,function(x,i) x$left)
    pleft <- unlist(pleft, use.names = F)
    indicator <- lapply(p2, function(x)x$indicator)
    indicator <- unlist(indicator, use.names = F)
    if(min(indicator) == 0)
    {
         left.n <- length(pleft)
        message(paste0("There are ", length(obj) - left.n, " different samples in subsets."))
        message(paste0("There are ", left.n, " samples which are not in any subsets."))
        guess.sn <- ceiling(left.n/new.n[sn])+sn
        warning(paste0("To include all the samples in subsets, the next trying sn is ", guess.sn,
            "\nIf the points are clustered, the sn tends to be much bigger."))
    }


    max.in <- max(indicator)
    if(max.in < sn)
    {
        message ("There are 0 samples which are not in any subsets")
        warning(paste0("The number of subsets is too bigger.", max.in, "subsets are enough for spliting" ))
    }
    res.subs <- as.list(rep(NA, sn))
    for (i in 1:sn)
    {
        res.subs[[i]] <- obj[obj$gid %in% subsplit[i,], ]
        res.subs[[i]]$gid <- NULL
    }
    res.left <- obj[obj$gid %in% pleft, ]
    return(list(sub = res.subs, left = res.left, new.n = new.n))
}


#round(exp(1:20))
#taken from CLM layer scheme
getclass <- function(nl){
    z <- 0.025*expm1(0.5*(1:nl-0.5))
    d <- NULL
    for(i in 1:nl)
    {
        if(i == 1){
            d[i] <- (z[1]+z[2])/2
        }else if(i == nl){
            d[i] <- z[i] - z[i-1]
        }else{
            d[i] <- (z[i+1] - z[i-1])/2
        }
    }
    bot <- NULL
    for(i in 1: nl)
    {
        if(i == nl){
            bot[i] <- z[i] + 0.5*d[i]
        }else bot[i] <- 0.5*(z[i] + z[i+1])
    }
    bot <- c(round(bot[c(4, 6, 8, 9, 10, 11, 12, 13, 14, 15, (8:(nl/2))*2)]*100),999999)
    up  <- c(0,bot[1:(length(bot)-1)])
    class <- 1:(nl/2+4)
    return(data.frame(class, up, bot))
}
dclass <- getclass(19)
#the first three and the last two were binned
dclass$up <- c(0, 20, 50, 140, 230, 380, 600, 1000, 1700, 3000, 4500, 7500, 20000)
dclass$bot <- c(20, 50, 140, 230, 380, 600, 1000, 1700, 3000, 4500, 7500, 20000, 999999)

toclass <- function(x, c){
    ret <- x
    for(i in 1:dim(c)[1])
    {
        ret[x < c$bot[i] & x >= c$up[i]] <- c$class[i]
    }
    ret
}


getcor <- function(y, pre, n)
{
    require(plyr)
    tmp <- tapply(pre,y,count)
    m <- matrix(0, nrow = n, ncol =n)
    for(i in 1 : length(tmp))
    {
        tmp2 <- tmp[[i]]
        for(j in 1:dim(tmp2)[1])
        {
            m[i,tmp2$x[j]] <- tmp2$freq[j]
        }
    }
    m2 <- m
    for(i in 1:n)
    {
        if(i == 1){
            m2[i,i] <- m[i,i] + m[i, i+1]
            m2[i,i+1] <-0
        }else if (i ==n){
            m2[i,i] <- m[i,i]+ m[i, i-1]
            m2[i,i-1] <-0
        }else{
            m2[i,i] <- m[i,i]+ m[i, i-1] + m[i, i+1]
            m2[i,i+1] <-0
            m2[i,i-1] <-0
        }
    }
    c1 <- rep(0,n)
    c2 <- rep(0,n)
    sumc1 <- 0
    sumc2 <- 0
    for(i in 1:n)
    {
        c1[i] <- round(m[i,i]/sum(m[i,]),3)
        c2[i] <- round(m2[i,i]/sum(m2[i,]),3)
        sumc1 <- sumc1 + m[i,i]
        sumc2 <- sumc2 + m2[i,i]
    }
    sumc1 <- sumc1/sum(m)
    sumc2 <- sumc2/sum(m2)
    return(list(m = m, m2 = m2, c1 = c1, c2 = c2, sumc1 = sumc1, sumc2 = sumc2))
}


pfun <- function(x,y, ...){
         panel.hexbinplot(x,y, ...)
         panel.abline(0,1,lty=1,lw=2,col="black")
}

getconfusion <- function(x,y)
{
    err.rate  <- signif(1 - sum(x==y)/length(x), 3)
    tmp <- matrix(0, nrow = 2, ncol =3 )
    tmp[1,1] <-  sum((x == F) & (y == F))
    tmp[1,2] <-  sum((x == F) & (y == T))
    tmp[2,1] <-  sum((x == T) & (y == F))
    tmp[2,2] <-  sum((x == T) & (y == T))
    tmp[1,3] <-  round(tmp[1,2]/(tmp[1,1]+tmp[1,2]),3)
    tmp[2,3] <-  round(tmp[2,1]/(tmp[2,1]+tmp[2,2]),3)
    colnames(tmp) <- c("FALSE", "TRUE", "class.error")
    rownames(tmp) <- c("FALSE", "TRUE")
    return(list( err.rate = err.rate,  confusion = tmp))
}
