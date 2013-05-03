# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-01 Wed 11:55 emilio on emilio-laptop2>
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

data <- data.frame(x1=c(1,2), y1=c(10,20), xend1=c(1.5,2.5), yend1=c(15,25), x=c(0,0), color=c("red","green"), car=c("bmw","seat"))

interpolate <- function(xbegin, ybegin, xend, yend, npoints = 10, xjitteramount= 0, yjitteramount=0, bezier = TRUE,...) {
   print(as.list(match.call()[-1]))
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


## Args of a function

 fcn <- get("interpolate", mode = "function")
 argsfcn<-  names(formals(fcn))
 argsnotdefined <- argsfcn[unlist(lapply(formals(fcn),class)) == "name"]
 argsmandatory <- argsnotdefined[ argsnotdefined != "..."]


 fcn <- get("aes", mode = "function")
 argsfcn<-  names(formals(fcn))
 argsnotdefined <- argsfcn[unlist(lapply(formals(fcn),class)) == "name"]
 argsmandatory <- argsnotdefined[ argsnotdefined != "..."]
argsmandatory
argsnotdefined


data
mapping <- aes(x= x1 +y1, y = y1)

## Create a aes with the default names. It changes the names of the data base
## 
## Therefore, we get a data frame with the default names of the mapping
## and a new mapping with the names by default
## For instance
## mapping <- aes(x= x1 +y1, y = y1) -> mapping <- aes(x= x, y = y)
## data[ , c("x1","y1","color")]  -> data[, c("x","y","x1","y1","color")]
createdefaultmappinganddata <- function(mapping, data) {
  ## For each name of the mapping, evaluate it and create a new data base
  ## with the names of the mapping.
  namesmapping <- names(mapping)
  dataaes <- as.data.frame(lapply(mapping, function(xnamedataxkcdveryrare.327) with(data, eval(xnamedataxkcdveryrare.327))))
  ## Add the rest of variables of the data base
  variablestocbind <- names(data)[!(names(data) %in% namesmapping)] 
  dataaes[, variablestocbind] <- data[,variablestocbind]
  ## Now, it creates a new mapping with the default variables x=x, y=x, yend=yend, and so on
  ## see aes_string
  parsed <- lapply(namesmapping, function(x) parse(text = x)[[1]])
  names(parsed) <- namesmapping
  newmapping <- structure(parsed, class = "uneval")
  list(mapping = newmapping, data = dataaes)
  }


defaultaesdata <- createdefaultmappinganddata(mapping= aes(x= x1 +y1, y = y1), data = data)

ggplot()+geom_point(mapping=defaultaesdata$mapping, data = defaultaesdata$data )


data <- defaultaesdata$data
mapping <- defaultaesdata$mapping


data$xend <- 1:2
mapping

length(mapping)


doforeachrow <- function(data, fun, ...) {
  ## Do not pass the variables of the ELLIPSIS
  ## that are arguments of the FUN  and they are in the DATA
  fcn <- get(fun, mode = "function")
  argsfcntt <-  names(formals(fcn))
  argsfcn <- argsfcntt[ argsfcntt != "..."]
  argsnotdefined <- argsfcn[unlist(lapply(formals(fcn),class)) == "name"]
  argsindata <- argsfcn[( argsfcn %in% names(data)) ]
  largellipsiscleaned <- list(...)
  for( i in intersect(argsindata, names(largellipsiscleaned) ) )
    largellipsiscleaned[i] <- NULL

 lapply(1:(dim(data)[1]),
         function(i, data, fun, argsindata, largellipsiscleaned) {
           largstopass <- as.list(data[i, argsindata,])
           do.call(fun, c(largstopass, largellipsiscleaned))
         },
         data = data,
         fun = fun,
         argsindata = argsindata,
         largellipsiscleaned = unlist(largellipsiscleaned)
         )
  
}
doforeachrow(data=data.frame(xbegin=c(1,2), ybegin=c(10,20), xend=c(1.5,2.5), yend=c(15,25), color=c("red","green"), car=c("bmw","seat")), fun="interpolate", ruido = 3,npoints=7,xjitteramount=0.1)




pp <- function(x=3,...)
  { print(as.list(match.call()[-1]))}

pp(x=7, y=2)

















pp

kk <- 

data

print(data)

  fcn <- get("interpolate", mode = "function")
  argsfcntt <-  names(formals(fcn))
  argsfcn <- argsfcntt[ argsfcntt != "..."]
  argsnotdefined <- argsfcn[unlist(lapply(formals(fcn),class)) == "name"]
argsindata <- argsfcn[( argsfcn %in% names(data)) ]

argList<-list(...)

argsnotindata


data
argsindata
class(parsed)

fun
get()
newmappingstring <- list()
names(newmapping) <- namesmapping
newmapping[sapply(newmapping, is.null)] <- "NULL"

names(newmapping)
kk <- newmapping$x

aes_string

names(kk)
class(kk)
(kk)
is.call(kk)
class(newmapping$x)

  mode(call("x"))


newmapping$x
newmapping
aes

pp <- function(...) {
  print(as.list(match.call()[-1]))
}

expression(x)

pp(x="aa",y=z)
match.call(call="aes")

aes_string

match.call

argsmandatory

aes
argsnotdefined

argsfcn
pairlistofargs <- formals(fcn)
mandatoryargs <- lapply(argsfcn, function(x) {if(is.null(formals(fcn)[[x]])) x})
mandatoryargs
is.null(formals(fcn))

formals(fcn)[["xjitteramount"]]
class(formals(fcn)[["xjitteramount"]])


args("interpolate")
class(formals(fcn)[["xend"]])
a

?hasArg

class(pairlistofargs[[9]])


lapply(pairlistofargs,class)
myArgs <- match.call()
  zInArgs <- ("z" %in% names(myArgs))

match.call(call="interpolate")

?match.call
