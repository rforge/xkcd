# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-04-30 Tue 16:54 emilio on emilio-laptop2>
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
  othervariablenames <- names(data)[!names(data) %in% namesofdataaesthetics]
  argList<-list(...);
  nsegments <- dim(data)[1]
  print("segmentos")
  print(nsegments)
  if(is.null(argList$xjitteramount)==TRUE){
    xjitteramount = rep(0, nsegments)
    } else if( length(xjitteramount) = 1 ) {
    xjitteramount <-  rep(xjitteramount, nsegments)
  }
  replicatepoint <- function(i, mapping, data, namesofdataaesthetics, othervariablenames, ...) {
      newdata <- interpolate(xbegin = data[i, namesofdataaesthetics[1]],
                                                          ybegin = data[i, namesofdataaesthetics[2]],
                                                          xend = data[i, namesofdataaesthetics[3]],
                                                          yend = data[i, namesofdataaesthetics[4]],
                                                          xjitteramount = xjitteramount[i],
                                                          yjitteramount = 0,
                                                          npoints = 10,
                                                          ...)
                                   ## mapping will call x= and y=
                                   names(newdata) <- namesofdataaesthetics[1:2]
                                   ## Add the rest of variables
                                   for( namex in othervariablenames )
                                     newdata[,namex] <- data[i,namex]
                                   ## Other variables that appears in the mapping, but they do not appear
                                   newdata[, unique(namesofdataaesthetics[ !namesofdataaesthetics %in% names(newdata)])] <- runif(1,0,1)
                                   geom_path(mapping = mapping, data = newdata, ...)
                                                                
  }
  ## listofpaths <- lapply(1:(dim(data)[1]),
  ##                                function(i, data=data, xjitteramount=xjitteramount, ...) {
                                 
  ##                                )
  ## listofpaths  
}
segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1) , data = data)


dibujobase + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1) , data = data)

dibujobase + geom_segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=xend1) , data = data)

names(list(...))


interpolate <- function(xbegin, ybegin, xend, yend, npoints = 10, xjitteramount= 0, yjitteramount=0,...) {
  if(npoints < 2 )
    stop("npoints must be greater than 1")
  x <- seq(xbegin,xend,length.out = npoints)
  if( (xend - xbegin) != 0 ) {
    y <- (yend - ybegin) * ( x - xbegin ) / (xend - xbegin) + ybegin
  } else {
    y <-  seq(ybegin, yend, length.out = npoints)
  }
  print(xjitteramount)
  if(xjitteramount !=0) x <- jitter(x, amount=xjitteramount)
  if(yjitteramount !=0) y <- jitter(y, amount=yjitteramount)
  x[1] <- xbegin
  y[1] <- ybegin
  x[length(x)] <- xend
  y[length(y)] <- yend
  data <- data.frame(x=x, y=y)
  data
}


interpolate(xbegin=0, ybegin=0, xend=1, yend=1,npoints=10)

?jitter
?rug
names(data)
data$colour

       
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
