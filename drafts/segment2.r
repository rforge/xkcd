# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-04-30 Tue 23:50 emilio on emilio-laptop2>
# =====================================================================

library(ggplot2)
datascaled <- data.frame(x=c(-3,3),y=c(-30,30))
dibujobase <- ggplot(data=datascaled, aes(x=x,y=y)) + geom_point()
xrange <- 6
yrange <- 60
ratioxy <- xrange / yrange
xlim <- c(-3,3)
ylim <- c(-30,30)
xjitteramount <- diff(xlim) / 50
yjitteramount <- diff(ylim) / 50
ratioxy

data <- data.frame(x1=c(1,2), y1=c(10,20), xend1=c(1.5,2.5), yend1=c(15,25), color=c("red","green"), car=c("bmw","seat"))
data

dibujobase + geom_segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1) , data = data)

interpolate <- function(xbegin, ybegin, xend, yend, npoints = 10, xjitteramount= 0, yjitteramount=0, bezier = TRUE,...) {
  require(Hmisc) # bezier
  ##require(R.utils)
  if(npoints < 2 )
    stop("npoints must be greater than 1")
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

segment <- function(mapping, data, ...) {
  requiredaesthetics <-  c("x","y","xend","yend")
  if( any(!  requiredaesthetics %in% names(mapping)))
    stop("mapping=aes(x= , y= , xend=, yend= ) must contain x, y, xend, and yend variables")
  namesofdataaesthetics <- NULL
  for( variable in requiredaesthetics) {
    if( !as.character(mapping[variable]) %in% names(data) )
      stop(paste("mapping",variable,"=", as.character(mapping[variable]),"not included in the names of the data base" ))
    namesofdataaesthetics <- c(namesofdataaesthetics, as.character(mapping[variable]))  
  }
  ## Add the rest of variables
  othervariablenames <- names(data)[!names(data) %in% namesofdataaesthetics]
 
  nsegments <- dim(data)[1]

  ##                            tempdata <- data.frame(x=1:nsegments) # for the interpolate function
  ## argList<-list(...)
  ## listofarguments <- c("xjitteramount","yjitteramount","npoints")
  ## for( arg in listofarguments) {
  ##   if(is.null(argList[arg])==TRUE){
  ##   tempdata[,arg] <- 0 }
  ## else tempdata[,arg] <- argList[arg]
  ## }

  ##print(tempdata)
 

  
  listofpaths <- lapply(1:nsegments,
                        function(i,mapping,data, namesofdataaesthetics, othervariablenames,...) {
                          newdata <- interpolate(xbegin = data[i, namesofdataaesthetics[1]],
                                                 ybegin = data[i, namesofdataaesthetics[2]],
                                                 xend = data[i, namesofdataaesthetics[3]],
                                                 yend = data[i, namesofdataaesthetics[4]],
                                                 ...)
                          ## mapping will call x= and y=
                          names(newdata) <- namesofdataaesthetics[1:2]
                          
                          for( namex in othervariablenames )
                            newdata[,namex] <- data[i,namex]
                          ## Other variables that appears in the mapping, but they do not appear
                          newdata[, unique(namesofdataaesthetics[ !namesofdataaesthetics %in% names(newdata)])] <- runif(1,0,1)
                          geom_path(mapping = mapping, data = newdata, ...)
                       },
                        mapping = mapping,
                        data = data,
                        namesofdataaesthetics = namesofdataaesthetics,
                        othervariablenames = othervariablenames,
                        ... = ...
                       )
  listofpaths  
}
segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1) , data = data, xjitteramount=c(1,2))



dibujobase + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour = color) , data = data, xjitteramount=1)

dibujobase + geom_segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour=color) , data = data)  + facet_grid(. ~ color)

ggplot() + geom_segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour=color) , data = data) +  facet_grid(. ~ color)

ggplot() + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1) , data = data) +  facet_grid(. ~ color)

dibujobase  + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1) , data = data) +  facet_grid(. ~ color)
