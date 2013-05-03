# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-01 Wed 14:06 emilio on emilio-laptop2>
# =====================================================================


source("utils.r")

interpolate <- function(x, y, xend, yend, npoints = 10, xjitteramount= 0, yjitteramount=0, bezier = TRUE,...) {
  require(Hmisc) # bezier
  ##require(R.utils)
  if(npoints < 2 )
    stop("npoints must be greater than 1")
  xbegin <- x
  ybegin <- y
  ## If there are no jitter, do not interpolate
  if( xjitteramount == 0 & yjitteramount == 0) npoints <- 2 
  x <- seq(xbegin,xend,length.out = npoints)
  if( (xend - xbegin) != 0 ) {
    y <- (yend - ybegin) * ( x - xbegin ) / (xend - xbegin) + ybegin
  } else {
    y <-  seq(ybegin, yend, length.out = npoints)
  }
  if(xjitteramount !=0) x <- jitter(x, amount=xjitteramount)
  if(yjitteramount !=0) y <- jitter(y, amount=yjitteramount)
  x[1] <- xbegin
  y[1] <- ybegin
  x[length(x)] <- xend
  y[length(y)] <- yend
  if(bezier & length(x)>2 & (xjitteramount != 0 | yjitteramount != 0)) {
    data <- data.frame(bezier(x=x, y=y, evaluation=30))
  }
  else data <- data.frame(x=x,y=y)   
  data
}


## ============================================================
##
## ============================================================

interpolatecircle <- function(x =0,y=0, diameter = 1, ratioxy=1, npoints = 16, alpha=  runif(1, 0, pi/2), ...){
    require(Hmisc) # bezier
    require(R.utils) # doCall
    center <- c(x,y)
    r <- rep( diameter / 2, npoints )
    tt <- seq(alpha,2*pi + alpha,length.out = npoints)
    r <- doCall("jitter", x=r, ...)
    sector <-  tt > alpha & tt <= ( pi/ 2 + alpha)
    r[ sector ] <- r[sector] * 1.05
    sector <-  tt > ( 2 * pi/2 + alpha)  & tt < (3* pi/ 2 +alpha)
    r[ sector ] <- r[sector] * 0.95    
    xx <- center[1] + r * cos(tt) * ratioxy
    yy <- center[2] + r * sin(tt) 
    ##return(data.frame(x = xx, y = yy))
    return(data.frame(doCall("bezier",x = xx, y =yy, ...)))
}


## ============================================================
##
## ============================================================

xkcdline <- function(mapping, data, typexkcdline="segment", mask = TRUE, ...) {

  if(typexkcdline == "segment" ){
    fun <- "interpolate"
    ## Required variable in the aesthetics function for segment
    requiredaesthetics <-  c("x","y","xend","yend")
    if( any(!  requiredaesthetics %in% names(mapping)))
      stop("mapping=aes(x= , y= , xend=, yend= ) must contain x, y, xend, and yend variables")
  } else if(typexkcdline == "circle" ) {
    fun <- "interpolatecircle"
    requiredaesthetics <-  c("x","y","diameter")
    if( any(!  requiredaesthetics %in% names(mapping)))
      stop("mapping=aes(x= , y= , diameter=, yend= ) must contain x, y, and diameter variables")
  } else stop("typexkcdline must be segment or circle")


  ## We transform the data to get a default mapping
  segementmapdat <- createdefaultmappinganddata(mapping, data) 
  data <- segementmapdat$data
  mapping <- segementmapdat$mapping
 
  nsegments <- dim(data)[1]

  ## Are arguments of fun in the ellipsis?
  ## Yes, try to add to the data base
  datafun <- data
  argList<-list(...)
  fcn <- get(fun, mode = "function")
  argsfcntt <-  names(formals(fcn))
  argsfcn <- argsfcntt[ argsfcntt != "..."]
  
  for( i in intersect(argsfcn, names(argList))) {
    if(!(is.null(argList[i])==TRUE)){
      if(length(argList[[i]]) == 1 ) datafun[, i] <- unlist(rep(argList[[i]],nsegments))
      if(length(argList[[i]]) == nsegments ) datafun[, i] <- argList[[i]]
     }
   }

  ## Now, calculate the interpolates for each segment
  listofinterpolates <- doforeachrow(datafun, fun,...)
  listofinterpolateswithillustrativedata <- lapply(1:nsegments,
                                                   function(i) {
                                                     dti <- listofinterpolates[[i]]
                                                     illustrativevariables <- names(datafun)[ ! names(datafun) %in% names(dti) ]
                                                    dti[, illustrativevariables] <- datafun[i, illustrativevariables]
                                                   dti}
                                                   )

  ##print(listofinterpolateswithillustrativedata)
  
  listofpaths <- lapply(listofinterpolateswithillustrativedata,
                        function(x, mapping, mask, ...) {
                          pathmask <- NULL
                          if(mask) {
                            ## Plot a white line widther that the original line
                            ## We must check the color, colour or size
                            ## and change them to white and a greater width
                            argList<-list(...)
                
                            for(i in intersect(c("color","colour"), names(argList)))
                              argList[i] <- NULL
                            argList$mapping <- mapping
                            argList$data <- x
                            argList$colour <- "white"
                            if(is.null(argList$size)==TRUE) argList$size <- 3
                            if(argList$size <= 3 ) argList$size <- 3
                            else  argList$size <- argList$size *2
                            ##print(argList)
                            pathmask <- do.call("geom_path",argList)
                            ##pathmask <- geom_path(mapping = mapping, data = x, colour="white",size=8)
                            }
                          c(pathmask,
                            geom_path(mapping = mapping, data = x, ...))
                       },
                        mapping = mapping,
                        mask = mask,
                        ... = ...
                       )
  listofpaths  
}

