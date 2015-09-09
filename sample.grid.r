obj = profs
cell.size= c(0.1,0.1)
n=1
sn=2
bbox <- obj@bbox

sample.grid2 <- function(obj, cell.size, n, bbox, ...){
    #to avoid the gid in obj
    if ("gid" %in% names(obj)){
      gid <- obj$gid
    }else{
      gid <- NULL
    }
    obj$gid <- 1:length(obj)
    if(missing(bbox)) { bbox <- obj@bbox }
    if(missing(cell.size)){
        ## automatically determine width:
        cell.size <- bbox[1,]/400
        message("Assigning 'cell.size'", immediate. = TRUE)
    }
    x <- GridTopology(cellcentre.offset=bbox[,1],
            cellsize=cell.size,
            cells.dim=c(floor(abs(diff(bbox[1,])/cell.size[1])),
            ncols=floor(abs(diff(bbox[2,])/cell.size[2]))))
    r.sp <- SpatialGrid(x, proj4string = obj@proj4string)
    r <- raster(r.sp)
    grd <- as(rasterize(obj, r, field = "gid"), "SpatialGridDataFrame")
    ov <- as.integer(over(obj, grd)$layer)
    #ov <- as.integer(over(obj, grd, ...)$layer)
    pnts <- tapply(obj$gid, ov, function(x, n){
        if(length(x) <= n){ x
        } else { sample(x,n) }
        }, n=n)
    pnts <- unlist(pnts, use.names = FALSE)
    ret <- list(obj[obj$gid %in% pnts, ], grd)
    grd <- as(grd, "SpatialPixelsDataFrame")
    # some problem here
    grdn <- tapply(obj$gid, ov, function(x){ length(x)})
    grd$layer <- grdn
    names(ret) <- c("subset", "grid")
    if(is.null(gid)){
      ret$subset$gid <- NULL
    }else{
      ret$subset$gid <- gid
    }
    return(ret)
}

data(isis)
profs <- isis[["sites"]]
coordinates(profs) <- ~  LONWGS84 + LATWGS84
proj4string(profs) <- CRS("+proj=longlat +datum=WGS84")
## sample SpatialPointsDataFrame:
#bbox <- matrix(c(-180, -90, 180, 90), nrow=2)
prof1 <- sample.grid2(profs, cell.size = c(5,5), n = 1)
l0 <- list("sp.points", profs, pch=1, col="red")
l1 <- list("sp.points", prof1$subset, pch="+", col="black", cex=1.2)
spplot(prof1$grid, scales=list(draw=TRUE),
   col.regions="grey", sp.layout=list(l0, l1))


split.grid <- function(obj,  cell.size, n, sn, bbox, ...){
    #to avoid the gid in obj
    if ("gid" %in% names(obj)){
      gid <- obj$gid
    }else{
      gid <- NULL
    }
    obj$gid <- 1:length(obj)
    if(missing(bbox)) { bbox <- obj@bbox }
    if(missing(cell.size)){
        ## automatically determine width:
        cell.size <- bbox[1,]/400
        message("Assigning 'cell.size'", immediate. = TRUE)
    }
    x <- GridTopology(cellcentre.offset=bbox[,1],
            cellsize=cell.size,
            cells.dim=c(floor(abs(diff(bbox[1,])/cell.size[1])),
            ncols=floor(abs(diff(bbox[2,])/cell.size[2]))))
    r.sp <- SpatialGrid(x, proj4string = obj@proj4string)
    r <- raster(r.sp)
    grd <- as(rasterize(obj, r, field = "gid"), "SpatialGridDataFrame")
    ov <- as.integer(over(obj, grd, ...)$layer)
    ret <- NULL
    pnts0 <- obj$gid
    for(i in 1:sn)
    {
      if(length(pnts0)>0)
      {
        pnts <- tapply(pnts0, ov, function(x, n){
            if(length(x) <= n){ x
            } else { sample(x,n) }
            }, n=n)
        pnts <- unlist(pnts, use.names = FALSE)
        ret1 <- list(obj[pnts0 %in% pnts, ], grd)
        names(ret1) <- c("subset", "grid")
        if(is.null(gid)){
          ret1$subset$gid <- NULL
        }else{
          ret1$subset$gid <- gid
        }
        ov <- ov[!(pnts0 %in% pnts)]
        pnts0 <- pnts0[!(pnts0 %in% pnts)]
        ret[[i]] <- ret1
      }
    }
    return(ret)
}

tmp <- split.grid (sub.sp,  cell.size= c(0.1,0.1) , 5, 20)
lapply(tmp, function(x) dim(x$sub@data))
