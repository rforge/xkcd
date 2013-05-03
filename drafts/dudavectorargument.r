# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-01 Wed 00:04 emilio on emilio-laptop2>
# =====================================================================
quit()
Trabajar un argumento opcional como escalar o vector.

Estimada Lista,

Deseo aplicar una función a cada fila de un data frame con un argumento opcional.
En la función orginal, el argumento opcional debería poder ser escalar o vector.  

# Se aplica a cada fila. 
atomica <- function(xbegin, xend, npoints = 5, ...) { # los argumentos opcionales ... son necesarios ya que hay más funciones dentro
  seq(xbegin,xend,length.out = npoints) # la función seq no admite vectores
}


aplicaratodaslasfilas <- function(data, ...) {
  lapply(1:dim(data)[1], function(i,...) {
    atomica(xbegin=data[i,1], xend=data[i,2], ...)
  }, ... = ...
         )
}

data <- data.frame(xbegin=c(1:2),xend=c(11:12))
aplicaratodaslasfilas(data=data) # FUNCIONA
## [[1]]
## [1]  1.0  3.5  6.0  8.5 11.0

## [[2]]
## [1]  2.0  4.5  7.0  9.5 12.0

aplicaratodaslasfilas(data=data, npoints = 3) # FUNCIONA
## [[1]]
## [1]  1  6 11

## [[2]]
## [1]  2  7 12

aplicaratodaslasfilas(data=data, npoints = c(3,5)) # NO FUNCIONA
# Me gustaría obtener
## [[1]]
## [1]  1  6 11

## [[2]]
## [1]  2.0  4.5  7.0  9.5 12.0

¿Alguien sabría definir la función aplicaracadafila de tal forma que funcione en estos casos?
aplicaratodaslasfilas(data=data)
aplicaratodaslasfilas(data=data, npoints = 3)
aplicaratodaslasfilas(data=data, npoints = c(3,5))

Gracias
Emilio



aplicaratodaslasfilas <- function(data, ...) {
  arglist <- list(...)
 fcn <- get("atomica", mode = "function")
  argsfcn <-  names(formals(fcn))
  for( args in intersect(argsfcn,names(arglist)))
    data[,args] <- arglist[[args]]
  print(data)
  lapply(1:dim(data)[1], function(i,...) {
    
    atomica(xbegin=data[i,1], xend=data[i,2], ...)
  }, ... = ...
         )
}


