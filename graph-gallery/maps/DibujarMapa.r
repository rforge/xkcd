## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-05-26 Sun 09:52 emilio on emilio-laptop2>
## ============================================================


rm(list=ls())
library(sp)
## sudo apt get install gdal proj-bin proj-data libproj-dev
##install.packages("rgdal")
library(rgdal)
##install.packages("maptools")
library(maptools)
gpclibPermit()
library(ggplot2)
library(xkcd)
source("FunSubjMap.r") ## Mapas subjetivos




## Data from INE
## http://www.ine.es/ss/Satellite?c=Page&p=1254735116596&pagename=ProductosYServicios%2FPYSLayout&cid=1254735116596&L=1
mapaes <- readShapeLines('spain_regions_ind')
Encoding(levels(mapaes$NOMBRE99)) <- "latin1"
mymap <- fortify(mapaes) ## to plot maps with ggplot2
idx <-  match( mymap$id, rownames(mapaes@data)) # We need the PROVMUN code
mymap$COM <- factor(as.character(mapaes@data$COM[idx]))
mymap <- mymap[!is.na(mymap$COM),]

ggplot(aes(x= long, y =lat), data = mymap) +
  geom_path( aes( group = group))


names(mapaes)
unique(mapaes$NOMBRE99)
unique(mapaes$COM)
comprov <- unique(mapaes@data[,c("COM","NOMBRE99")])
comprov
## Coordenadas
kk <- (by(mymap,mymap$COM, function(x)c(median(x$long),median(x$lat)) ))
coordenadas <- as.data.frame(do.call("rbind",as.list(kk)))
names(coordenadas) <- c("long","lat")
coordenadas$COM <-  row.names(coordenadas)
coordenadas




datosparo <- read.table(header=FALSE,sep=",",text="
Aragón,22
Cantabria,20
Castilla y León,22
Rioja (La),18
Navarra (Comunidad Foral de),19
Galicia,22
Madrid (Comunidad de),20
Balears (Illes),28
Comunidad Valenciana,29
Murcia (Región de),30
Canarias,34
Ceuta,38
Melilla,38
Asturias (Principado de),25
Cataluña,24
Castilla-La Mancha,31
Extremadura,35
Andalucía,38
País Vasco,16
")
names(datosparo) <- c("nombre","parados")
datosparo$ocupados <- 100-datosparo$parados



matchnames <- function(name, listofnames,...) {
  require(stringr)
  name <- str_trim(name)
  listofnames <-str_trim(listofnames)
  for( i in c(1:100)/100 ) {
    position <- agrep(pattern=name, x=listofnames, ignore.case = TRUE, max.distance=i, ...)
    if(length(position)) return( position[1] )
  }
}
mapaes$NOMBRE99[!mapaes$NOMBRE99 %in% datosparo$nombre]
namesregions <- unique(mapaes$NOMBRE99)
tablamap <- data.frame(NOMBRE99 = as.character(mapaes$"NOMBRE99"),
                       COM = as.character(mapaes$"COM"))
tablamap <- unique(na.omit(tablamap))
namesregions <- tablamap$NOMBRE99
idx <- sapply(datosparo$nombre, matchnames, namesregions, value = FALSE)
datosparo$COM <- as.character(tablamap$COM[idx])

## We select only the Asturian municipalities

datosparo$COM


mydata <- merge(datosparo,coordenadas)
varname <- 'COM'
namesvariables <- c("ocupados", "parados")



varnameregion <- "NOMBRE99"
mydata <- datosparo
namesvariables <- c("ocupados","parados")
mymap <- mymap



matchnames <- function(name, listofnames,...) {
  require(stringr)
  name <- str_trim(name)
  listofnames <-str_trim(listofnames)
  for( i in c(1:100)/100 ) {
    position <- agrep(pattern=name, x=listofnames, ignore.case = TRUE, max.distance=i, ...)
    if(length(position)) return( position[1] )
  }
}
mapaes$NOMBRE99[!mapaes$NOMBRE99 %in% datosparo$nombre]
namesregions <- unique(mapaes$NOMBRE99)
tablamap <- data.frame(NOMBRE99 = as.character(mapaes$"NOMBRE99"),
                       COM = as.character(mapaes$"COM"))
tablamap <- unique(na.omit(tablamap))
namesregions <- tablamap$NOMBRE99
idx <- sapply(datosparo$nombre, matchnames, namesregions, value = FALSE)
datosparo$COM <- as.character(tablamap$COM[idx])

## We select only the Asturian municipalities

datosparo$COM


mydata <- datosparo # merge(datosparo,coordenadas)
varname <- 'COM'
namesvariables <- c("ocupados", "parados")


mydata


varnameregion <- "COM"




pts <- gridpoints(mymap[,c("long","lat","group")],  400)


colorizepointsinaregion <- function(coderegion, varnameregion, mydata, namesvariables, mymap, pts, reverse = FALSE ){
  names(pts) <- c("x","y","group")
  dataregion <- mydata[ mydata[,varnameregion] == coderegion, namesvariables]
  dataregion <- t(dataregion)
  oo <- order(dataregion,decreasing = TRUE)
  dataregion <- dataregion[oo,]
  dataregion
  ## Distribution of points among political parties
  mypolygon <-  mymap[mymap[,varnameregion] == coderegion,c("long","lat","group")]
  names(mypolygon) <- c("x","y","group")
  ptsmun <- NULL
  for( i in unique( mypolygon$group)) {
    mypolygoni <- mypolygon[ mypolygon$group == i, c("x","y")]
    ##print(head(pts))
    ptsmun <-   rbind(ptsmun, pts[inout(pts, mypolygoni), ])
    }
  nptsmun <- dim(ptsmun)[1]
  npuntos <- round(dataregion * nptsmun/sum(dataregion),0)
  if(sum(npuntos) != nptsmun) {
    epsilon <- nptsmun - sum(npuntos)
    if( epsilon < 0 ) {
      place <- max( which( npuntos >0 ) )
      npuntos[place] <-  npuntos[place ] + epsilon
      }
    else npuntos[1] <- npuntos[1] + epsilon
      }
  pointmean <- c(mean(ptsmun$x), mean(ptsmun$y))
  if( reverse ) {
    oo <- order( (ptsmun$x - pointmean[1])**2 + (ptsmun$y- pointmean[2])**2, decreasing =TRUE )
  }
  else {
    oo <- order( (ptsmun$x - pointmean[1])**2 + (ptsmun$y- pointmean[2])**2, decreasing =FALSE )
  }
  ptsmun <- ptsmun[oo,]
  if(dim(ptsmun)[1]>0 ) {
    ptsmun$variable <- NA
    to <- cumsum(npuntos)
    from <- c(1, to + 1)
    for( i in 1:length(to)) {
      if(npuntos[i]) {ptsmun$variable[ to[i] : from[i ]] <- names(to)[i]}
    }
    ptsmun[,varnameregion] <- coderegion
  }
  ptsmun
}





ptsvariable <- NULL
for( coderegion in unique(mymap[,varname]) ) {
  print(coderegion)
  ptsvariable <- rbind(ptsvariable, colorizepointsinaregion(coderegion, varname, mydata, namesvariables, mymap, pts, reverse=TRUE ) )
}

cbPallette <- c(  "red","yellow")
tt <- ggplot(aes(x= long, y =lat), data = mymap) +
  geom_point( aes(x = x, y = y, colour = variable ), data = ptsvariable) +
  geom_path( aes( group = group), data = mymap) +
  scale_colour_manual(name="Unemployment Rate", labels=c("Employed","Unemployed"),values=cbPallette) +
  annotate("text",x=800000, y=4000000, label="We are all Marca España...",family="xkcd",size=8 ) +
  theme(panel.grid.major = element_blank(), axis.ticks =  element_blank(), 
            panel.background = element_blank(), panel.grid.minor = element_blank(), 
            legend.key = element_blank(), strip.background = element_blank(),
        axis.line = element_blank(),
       axis.text.x = element_blank(), axis.text.y = element_blank(),
       axis.ticks = element_blank(), 
       axis.title.x = element_blank(), axis.title.y = element_blank(),
            text = element_text(size = 16, family = "xkcd"))
##tt


p <- tt + theme(
     line =               element_blank(),
     rect =               element_blank(),
     ##text =               element_blank(),
           ##text = element_text(size = 16, family = "xkcd"),
     axis.ticks.length =  unit(0, "cm"),
     axis.ticks.margin =  unit(0.01, "cm"), # change back to 0 when grid is fixed
    # legend.position =    "none",
           legend.position=c(0.15,0.55),
     panel.margin =       unit(0, "lines"),
     plot.margin =        unit(c(0, 0, -.5, -.5), "lines"),
     complete = TRUE   ) +  theme(panel.grid.major = element_blank(), axis.ticks =  element_blank(), 
            panel.background = element_blank(), panel.grid.minor = element_blank(), 
            legend.key = element_blank(), strip.background = element_blank(),
        axis.line = element_blank(),
       axis.text.x = element_blank(), axis.text.y = element_blank(),
       axis.ticks = element_blank(), 
       axis.title.x = element_blank(), axis.title.y = element_blank(),
            text = element_text(size = 16, family = "xkcd"))



p





ggsave("MarcaEspanaEN.png")

