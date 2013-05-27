## Emilio Torres-Manzanera 
## University of Oviedo    
## Time-stamp: "2013-05-25 Sat 09:45 emilio on emilio-laptop2"

library(ggplot2)
require(splancs)
require(gpclib)
require(reshape)



calculatearea <- function( mymap ) {
  require(gpclib)
  calculateareasingle <- function( mymap ) {
    mapgpcpoly <-  as(mymap[,c("long","lat")], "gpc.poly")
    area.poly(mapgpcpoly)
  }
  mymap$group <- droplevels(mymap$group)
  sum(by(mymap, mymap$group, calculateareasingle), na.rm =TRUE)
}

calculatearearectangle <- function( mymap ) {
  calculatearearectanglesingle <- function( mymap ) {
    longrange <- range(mymap$long)
    latrange <- range(mymap$lat)
    diff(longrange)*diff(latrange)
  }
  sum(by(mymap, mymap$group, calculatearearectanglesingle), na.rm =TRUE)
}


pointsinmap <- function( pts, mymap, inside = TRUE, ... ) {
  require(splancs)
  checkpoint <- function(mymap, pts, ...) {
    names(pts) <- c("x","y","group")
    names(mymap) <- c("x","y","group")
    inout(pts[,c("x","y")], mymap[,c("x","y")], ...)
  }
  ptsinside <- NULL
  ## Now, we check the bigger group in first place.
  mymap$group <- droplevels(mymap$group)
  kk <- by(mymap, mymap$group, calculatearearectangle)
  jj <- do.call("cbind",list(kk))
  oo <- order(jj[,1], decreasing =TRUE)
  groupsorderedbyarea <- rownames(jj)[oo]
  for( i in groupsorderedbyarea) {
    ##print(paste("pointsinmap: group",i))
    filterin <- checkpoint(  mymap[ mymap$group == i, ], pts, ...)
    if( length(filterin) ) {
      pts[filterin, "group" ] <- i
      ptsinside <- rbind( ptsinside, pts[filterin, ])
      pts <- pts[!filterin, ]
      }
  }
  if(inside) ptsinside
  else pts
}


## Count the number of points that are in/out a map
mycountpoints <-  function(mymap, pts, ...) {
  pointsin <-  pointsinmap(pts, mymap,...)
  dim(pointsin)[1]
  }


## Create a grip of points that are inside a map
## mymap with at least 3 variables ('long', 'lat', 'group', ...)
gridpoints <- function( mymap, npoints = 200, ...) {
  longrange <- range(mymap$long)
  latrange <- range(mymap$lat)
  pts <-  expand.grid(
                      long = longrange[1] + (c(0:npoints) *  (longrange[2] - longrange[1])/ npoints ),
                      lat = latrange[1] + (c(0:npoints) *  (latrange[2] - latrange[1])/ npoints )
                      )
  pts$group <- factor(NA, levels=levels(mymap$group))
  pointsinmap( pts, mymap, ...)
}



######################################################################

zoommap <- function(mymap, myscale = 1) {
  require(gpclib)
  oldmap <- mymap[,c("long","lat")]
  oldcenter <- sapply(oldmap,mean)
  mapgpcpoly <-  as(oldmap, "gpc.poly")
  kk <- scale.poly(mapgpcpoly, xscale= 1/myscale, yscale =1/myscale)
  jj <- get.pts(kk)
  newmap <- data.frame(long=jj[[1]]$x, lat=jj[[1]]$y)
  newcenter <- sapply(newmap, mean)
  newmap$long <- newmap$long + oldcenter[1] - newcenter[1]
  newmap$lat <- newmap$lat + oldcenter[2] - newcenter[2]
  newmap[, c("longoriginal", "latoriginal")] <- mymap[,c("long","lat")]
  listnames <- names(mymap)[!( names(mymap) %in% c("long","lat"))]
  newmap[, listnames] <- mymap[, listnames]
  newmap
}

####################################################################

zoommaptogetanvalue <-  function(mymap, pts, valuetoget ,  ntrialsmax =10, calculatemapstatistic = mycountpoints, ...){
  if( dim(pts)[1] == 0) return(mymap)
  valuetoget <- round(valuetoget)
  newvalue <- calculatemapstatistic(mymap, pts, ...)
  newepsilon <- abs(newvalue -valuetoget)
  epsilon <- newepsilon
  zoommax <-  NULL
  zoomin <- NULL
  zoom <- 1
finalmap <- zoommap(mymap, zoom)
  if (newvalue > valuetoget ) {
      zoommax <- zoom
      zoommin <- 0
      }
  else if (newvalue < valuetoget) {
    zoommax <- 50
    zoommin <- zoom
    }

  for( i in 1:ntrialsmax ) {
    if( epsilon < 0.02 * valuetoget ) return(finalmap)
    newmymap <- zoommap(mymap, zoom)
    newvalue <- calculatemapstatistic(newmymap, pts, ...)
    newepsilon <- abs(newvalue -valuetoget)
    if (newvalue > valuetoget ) {
      zoommax <- zoom
    }
    else if (newvalue < valuetoget) {
      zoommin <- zoom
    } else return(newmymap)
    zoom <- mean(c(zoommax,zoommin))
    ## print(paste("zoommaptogetanvalue:","trial",i,"points",newvalue,"zoom",round(zoom,1)))
    if( newepsilon < epsilon  ) {
      epsilon <- newepsilon
      finalmap <- newmymap
    }
  }
  finalmap
}

######################################################################

colorisepointsforasubjetivemap <- function(mymap, mydata, varname, namesvariables, pts, ...){
  require(reshape)
  ## Create a grid of points. All points are inside the map
  ## You can change npoints
  ##print("It spends a lot time. Creating a grid...")
  ##  pts <- gridpoints( mymap, ...)
  ## Put data in long format
  mymap <- droplevels(mymap)
  mydata <- droplevels(mydata[ mydata[,varname] %in% levels(mymap[,varname]), ])
  mydatalong <- melt(mydata, id=varname, measure.vars= namesvariables)
  ## Order by number of points to plot
  ntotalpoints <- dim(pts)[1]
  mydatalong$points <- mydatalong$value * ntotalpoints / sum(mydatalong$value,na.rm = TRUE)
  oo <- order(mydatalong$points, decreasing = TRUE)
  mydatalong <- mydatalong[oo, ]
  ## Original map is very detailed.
  ## It spends a lot of time to compute
  ## We simplified the shape line: one point of each three
  npointshp <- dim(mymap)[1]
  if( npointshp > 10000 ) {
    simplificar <- max(trunc(npointshp/10000) - 1,0)
    mymapsimplified <- mymap[c(TRUE,rep(FALSE, simplificar)),] ## We simplify the shape
    mymaplistname <- by(mymapsimplified, mymapsimplified[,varname], function(x)x) 
  }  else {
    mymaplistname <- by(mymap, mymap[,varname], function(x)x) 
  }
  calculatemapstatistic <- mycountpoints
  ptscolor <- NULL
  ptsi <- pts
  for( i in 1:dim(mydatalong)[1] ) {
    name <- mydatalong[i, varname]
    color <- mydatalong[i, 'variable']
    valuetoget <- mydatalong[i, 'points']
    mymapi <-  mymaplistname[[name]]
    ## 
    ##
    ##print(paste("Position", dim(mydatalong)[1]-i,":", name, color,"points", "target", round(valuetoget), mydatalong$value[i]))
    ##print(paste("check", dim(ptsi)[1]))
    ##
    if( !is.null(ptsi) ) {
      mymapi <- zoommaptogetanvalue(mymap =mymapi,
                                    pts = ptsi,
                                    valuetoget=valuetoget,
                                    ntrialsmax = 15,
                                    calculatemapstatistic= mycountpoints)
    }
    ##print("pasado mymapi")
    ptscolori <-  pointsinmap( ptsi, mymapi)
    ##print(paste("pasado ptscolori",dim(ptscolori)[1]," valuetoget",round(valuetoget)))
    if(dim(ptscolori)[1]) {
      ptscolori$color <- color
      ptscolor <- rbind(ptscolor, ptscolori)
    }
    ptsi <- pointsinmap( ptsi, mymapi, inside=FALSE)
    ##mymaplistname[[name]] <- mymapi
  }
  ## ## The rest of points, we assign to the last category
  ## ptscolori <- ptsi
  ##  if(dim(ptscolori)[1]) {
  ##     ptscolori$color <- color
  ##     ptscolor <- rbind(ptscolor, ptscolori)
  ##   }
  ptscolor
}

######################################################################




