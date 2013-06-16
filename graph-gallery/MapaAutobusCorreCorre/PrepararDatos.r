## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-06-16 Sun 14:50 emilio on emilio-laptop2>
## ============================================================

dir()
datos <- read.csv(file="autobuses.csv",header=FALSE)
head(datos)
names(datos) <- c("idautobus","linea","recorrido","utmx","utmy","lon","lat","hora","dia")
head(datos)
summary(datos)


datos$hora <- paste("2013-06-15",datos$hora)
datos$hora <- strptime(datos$hora,"%Y-%m-%d %H:%M:%S", tz="EST5EDT")


datos$idautobus <- factor(datos$idautobus)
datos$linea <- factor(datos$linea)
datos$recorrido <- factor(datos$recorrido)

datos <- unique(datos)

dim(datos)



## library(PBSmapping)
## pp <- datos[,c("utmx","utmy")]
## names(pp) <- c("X","Y")
##  attr(pp, "projection") <-"UTM"
##  attr(pp, "zone") <-30
## coords <- convUL(pp, km=FALSE)
## head(coords)




save(datos,file="autobus.rda")


