# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-01 Wed 12:03 emilio on emilio-laptop2>
# =====================================================================


## Utils.r

library(ggplot2)

## createdefaultmappinganddata <- function(mapping, data)
## doforeachrow <- function(data, fun, ...)
  

## ============================================================
## createdefaultmappinganddata 
## ============================================================

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


## ============================================================
## doforeachrow
## ============================================================

## Apply a FUN to each row of the DATA
## If the arguments of the FUN are in the DATA and in the ELLIPSIS
## only use the variable of the DATA
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
  ## Now, apply for each row the FUN
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

