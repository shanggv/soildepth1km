
extract.equi7t3 <- function(x, y, path=".", equi7t3, cpus){
  ## list all tiles:
  tiles.lst <- list.dirs()[-1]
  tiles.lst <- substr(tiles.lst, 3, 12)
  tiles.lst <- tiles.lst[-which(tiles.lst=="KML")]
  ## get continents: 
  ov.c <- lapply(equi7t3, function(t){over(x, spTransform(t, CRS(proj4string(x))))})
  ov.t <- lapply(ov.c, function(t){which(!is.na(t$TILE))})
  ## get tiles:
  ov.c <- lapply(1:7, function(i){cbind(ov.c[[i]][ov.t[[i]],c("SHORTNAME","TILE")], row.index=ov.t[[i]])})
  ov.c <- do.call(rbind, ov.c)
  ov.c$TILENAME <- paste0(sapply(ov.c$SHORTNAME, function(i){strsplit("E7G ", i)[[1]][2]}), "_", ov.c$TILE)
  tmp <- sort(unique(ov.c$TILENAME[ !(ov.c$TILENAME %in% tiles.lst) ] ))
  ov.c <- ov.c[ov.c$TILENAME %in% tiles.lst, ]
  row.in <- unique(ov.c$row.index) 
  if(length(tmp) > 0){
    print("The following tiles do not exist but have points:")
    print(tmp)
  }
  ## list all available tifs:
  cov.lst <- as.vector(unlist(lapply(y, function(i){list.files(path=path, pattern=glob2rx(paste0(i, "_*_*_*.tif$")), recursive=TRUE, full.names=TRUE)})))
  ov.c <- ov.c[!duplicated(ov.c$row.index),]
  tiles <- levels(as.factor(ov.c$TILENAME))    
  cov.c <- lapply(tiles, function(i){grep(i, cov.lst)})
  names(cov.c) <- tiles 
  cov.c <- cov.c[lapply(cov.c,length)>0]
  ## extract using snowfall
  snowfall::sfInit(parallel=TRUE, cpus=cpus)
  snowfall::sfExport("x", "ov.c", "cov.c", "cov.lst", ".extract.tile", "equi7t3")
  snowfall::sfLibrary(package="raster", character.only=TRUE)
  snowfall::sfLibrary(package="rgdal", character.only=TRUE)
  ov.lst <- snowfall::sfClusterApplyLB(1:length(cov.c), .extract.tile) 
  names(ov.lst)<- names(cov.c)
  snowfall::sfStop()
  ## bind:
  #out <- dplyr::rbind_all(ov.lst)
  notcom <- NULL
  for(i in 1:length(ov.lst))
  {
    if(length(y) > length(ov.lst[[i]])-4)
    {
        notcom <- c(notcom,names(cov.c)[i])
       
    }   
  } 
  if(length(notcom)> 0){
    for(i in 1:length(notcom)) ov.lst[[notcom[i]]] <- NULL 
    print("The following tiles have some missing covriates:")
    print(notcom)
  }
  out <- do.call(rbind, ov.lst)
  x$row.index <- 1:nrow(x)
  out <- plyr::join(x@data, as.data.frame(out), type="left", by="row.index")
  out$row.index <- NULL
  return(out)
}

.extract.tile <- function(i){
  s <- stack(cov.lst[cov.c[[i]]])
  row.index <- ov.c$row.index[ov.c$TILENAME==names(cov.c)[i]]
  pnts <- x[row.index,]
  prj <- CRS(proj4string(equi7t3[[strsplit(names(cov.c)[i], "_")[[1]][1]]]))
  pnts <- spTransform(pnts, prj)
  out <- data.frame(extract(s, pnts))
  out$row.index <- row.index
  out$Equi7t3 <- names(cov.c)[i]
  names(out) <- sapply(names(out), function(i){strsplit(i, "_")[[1]][1]})
  xy <- data.frame(pnts@coords)
  names(xy) <- c("X","Y")
  out <- cbind(out, xy)
  save(out, file = paste0("../ov/",names(cov.c)[i], ".rda"))
  return(out)
}
