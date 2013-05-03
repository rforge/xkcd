# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-01 Wed 12:39 emilio on emilio-laptop2>
# =====================================================================


### ESTE ES EL BUENO

source("utils.r")

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

segment <- function(mapping, data, ...) {
  ## Required variable in the aesthetics function for segment
  requiredaesthetics <-  c("x","y","xend","yend")
  if( any(!  requiredaesthetics %in% names(mapping)))
    stop("mapping=aes(x= , y= , xend=, yend= ) must contain x, y, xend, and yend variables")

  ## We transform the data to get a default mapping
  segementmapdat <- createdefaultmappinganddata(mapping, data) 
  data <- segementmapdat$data
  mapping <- segementmapdat$mapping
 
  nsegments <- dim(data)[1]

  ## Are arguments of fun in the ellipsis?
  ## Yes, try to add to the data base
  datafun <- data
  argList<-list(...)
  fun <- "interpolate"
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
                        function(x, mapping,...) {
                          
                          geom_path(mapping = mapping, data = x, ...)
                       },
                        mapping = mapping
                       )
  listofpaths  
}
segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1) , data = data,xjitteramount=2)
segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1) , data = data)
segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1) , data = data,xjitteramount=c(2,3))

dibujobase + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour = color) , data = data)
dibujobase + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour = color) , data = data,xjitteramount=3)
dibujobase + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour = color) , data = data, xjitteramount=c(0.1,3))

dibujobase + geom_segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour=color) , data = data)  + facet_grid(. ~ color)

ggplot() + geom_segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour=color) , data = data) +  facet_grid(. ~ color)

ggplot() + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1) , data = data) +  facet_grid(. ~ color)

dibujobase  + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1) , data = data) +  facet_grid(. ~ color)


fcn <- get("interpolate", mode = "function")
names(formals(fcn))

match.call(definition=interpolate)
match.arg()


methods(doCall)

doCall.default
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

?assign

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
                        function(i,...) {
                          ...$xjitteramount <- 4
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
                        ... = ...
                       )
  listofpaths  
}
segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1) , data = data, xjitteramount=c(1,2))



dibujobase + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour = color) , data = data, xjitteramount=c(3,1))

dibujobase + geom_segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour=color) , data = data)

data
names(list(...))

replicatepoint(i=1, mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1), data=data, namesofdataaesthetics =  c("x1","y1","xend1","yend1"))


replicatepoint <- function(i, mapping, data, namesofdataaesthetics, ...) {
  print(i)
  newdata <- interpolate(xbegin = data[i, namesofdataaesthetics[1]],
                         ybegin = data[i, namesofdataaesthetics[2]],
                         xend = data[i, namesofdataaesthetics[3]],
                         yend = data[i, namesofdataaesthetics[4]],
                         ...)
  ## mapping will call x= and y=
  names(newdata) <- namesofdataaesthetics[1:2]
  ## Add the rest of variables
  othervariablenames <- names(data)[!names(data) %in% namesofdataaesthetics]
  for( namex in othervariablenames )
    newdata[,namex] <- data[i,namex]
  ## Other variables that appears in the mapping, but they do not appear
  newdata[, unique(namesofdataaesthetics[ !namesofdataaesthetics %in% names(newdata)])] <- runif(1,0,1)
  geom_path(mapping = mapping, data = newdata, ...)
  }


replicatepoint(i=1, mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1), data=data, namesofdataaesthetics =  c("x1","y1","xend1","yend1"))

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

interpolate(xbegin=0, ybegin=0, xend=1, yend=1,npoints=2,xjitteramount=9.1)
plot(interpolate(xbegin=0, ybegin=0, xend=1, yend=1,npoints=2,xjitteramount=9.1))

smooth.spline
2 && 4
?spline

c()
?jitter
?rug
names(data)
data$colour
bezier

       
library(grid) # needed for arrow function
p <- ggplot(seals, aes(x = long, y = lat))
q <- p + segment(aes(x = long, y = lat, xend = delta_lat, yend = lat ), data = seals, arrow = arrow(length = unit(0.1,"cm")))
p


path <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", 
  lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, arrow = NULL, 
      ...) {
  geom_path(mapping = mapping, data = data, stat = stat, position = position, 
            lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, arrow = arrow, 
      ...)
}


lapply(1, function)

data <- data.frame(x1=c(1,2), y1=c(10,20), xend1=c(1.5,2.5), yend1=c(15,25))
names(data) <- c("x","y","xend","yend")
data$colour <- c("red","green")
data$car <- c("bmw","audi")
data
npoints <- 7
data$x



interpolatedataframe <- function(data, ...){
  if( any(! c("x","y","xend","yend") %in% names(data)))
    stop("data must contain x, y, xend, yend variables")
  othervariablenames <- names(data)[!names(data) %in% c("x","y","xend","yend")]
  listofinterpolations <- lapply(1:(dim(data)[1]),
                                 function(i) {
                                 newdata <- interpolate(xbegin = data$x[i],
                                             ybegin = data$y[i],
                                             xend = data$xend[i],
                                             yend = data$yend[i],
                                             ...)
                                 for( namex in othervariablenames )
                                   newdata[,namex] <- data[i,namex]
                                 newdata
                                 }
                                 )
  do.call("rbind",listofinterpolations)
}



interpolatedataframe(data)

for( namei in othervariablenames ) newdatapath[,namei] <- data[1,namei]

newdatapath


lapply(, function(x) )

newdatapath
newdatapath <
, data[1,othervariablenames,drop=FALSE])
newdatapath

x
y
data

library(Hmisc)
?bezier
bezier


?segmentsGrob
library(grid)

assign


plot.default
Axis
