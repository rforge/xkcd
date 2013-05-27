## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-05-25 Sat 14:21 emilio on emilio-laptop2>
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


datosparo <- read.table(header=FALSE,sep=",",text="
Andalucía,2522,1473
Aragón,500,144
Asturias (Principado de),361,122
Balears (Illes),422,169
Canarias,739,385
Cantabria,215,56
Castilla y León,893,262
Castilla-La Mancha,676,311
Cataluña,2775,902
Comunidad Valenciana,1764,727
Extremadura,329,181
Galicia,998,287
Madrid (Comunidad de),2669,682
Murcia (Región de),507,221
Navarra (Comunidad Foral de),247,58
País Vasco,846,164
Rioja (La),119,27
Ceuta,22,14
Melilla,22,10
")
names(datosparo) <- c("nombre","ocupados","parados")
datosparo


datosparo$total <- datosparo$ocupados + datosparo$parados

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



mydata <- datosparo ##merge(datosparo,coordenadas)
varname <- 'COM'
namesvariables <- c("ocupados", "parados")

mydata



areas <- by(mymap, mymap$COM, calculatearea)
datosespana <- data.frame(do.call("cbind",list(areas)))
names(datosespana) <- c("areareal")
datosespana$COM <- rownames(datosespana)
idx <- match( datosespana$COM, mydata$COM)
datosespana$poblacion <- mydata$total[idx]
sumas <- colSums(datosespana[,c("areareal","poblacion")])
datosespana$areasubjetiva <- datosespana$poblacion * sumas["areareal"]/ sumas["poblacion"]

head(datosespana)


head(mydata)
varname
namesvariables

pts <- gridpoints( mymap, 400)

dim(pts)

## Adapatams la funcion ptscolor <- colorisepointsforasubjetivemap(mymap, mydata, varname, namesvariables, pts)


## Esto lleva tiempo...
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
  oo <- order(mydatalong$variable, mydatalong$points, decreasing = TRUE)
  mydatalong <- mydatalong[oo, ]

## OJO. Simplifico aquí, ya que sólo nos interesan los parados
 mydatalong <- mydatalong[mydatalong$variable=="parados",]

## Original map is very detailed.
  ## It spends a lot of time to compute
  ## We simplified the shape line: one point of each three
  npointshp <- dim(mymap)[1]
  if( npointshp > 10000) {
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
    print(paste("Position", dim(mydatalong)[1]-i,":", name, color,"points", "target", round(valuetoget), mydatalong$value[i]))
    print(paste("check", dim(ptsi)[1]))
    ##
    if( !is.null(ptsi) ) {
      mymapi <- zoommaptogetanvalue(mymap =mymapi,
                                    pts = ptsi,
                                    valuetoget=valuetoget,
                                    ntrialsmax = 20,
                                    calculatemapstatistic= mycountpoints)
    }
    ##print("pasado mymapi")
    ptscolori <-  pointsinmap( ptsi, mymapi)
    print(paste("pasado ptscolori",dim(ptscolori)[1]," valuetoget",round(valuetoget)))
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


grmap6 <- ggplot() +
  geom_point( aes( x = long, y = lat, color = color), data = ptscolor,size=0.1)
grmap6

grmap6 <- ggplot() +
  geom_point( aes( x = long, y = lat, color = color), data = ptscolor,size=0.4)
grmap6

summary(ptscolor)

ptsparo <- ptscolor[ptscolor$color=="parados",]

grmap6 <- ggplot() +
  geom_point( aes( x = long, y = lat), data = ptsparo,size=0.9,color="grey")
grmap6


grmap6 +  geom_path(aes(x=long, y=lat, group=group), mymap,color="yellow") 

xrange <- range(ptsparo$long)
yrange <- range(ptsparo$lat)
ratioxy <-  diff(xrange) / diff(yrange)


x <- -220000
y <-  4400000
scale <- 100000



mapman  <- aes(x,  y,
               scale,
               ratioxy,
               angleofspine ,
               anglerighthumerus,
               anglelefthumerus,
               anglerightradius,
               angleleftradius,
               anglerightleg,
               angleleftleg,
               angleofneck)
n <- 1
set.seed(321)
dataman <- data.frame( x= x,
                       y= y,
                      scale = scale ,
                      ratioxy = ratioxy,
angleofspine = runif(n, -pi/2 - pi/10,-pi/2 + pi/10),
                      anglerighthumerus = runif(n, -pi/6-pi/10, -pi/6+pi/10),
                      anglelefthumerus = runif(n, -pi/2 - pi/10, -pi/2 + pi/10),
                      anglerightradius = runif(n, -pi/5 - pi/10, -pi/5 + pi/10),
                      angleleftradius = runif(n, -pi/5 - pi/10, -pi/5 + pi/10),
                      angleleftleg = runif(n, 3*pi/2  + pi / 12  -pi/20,  3*pi/2  + pi / 12  +pi/20) ,
                      anglerightleg =  runif(n, 3*pi/2  - pi / 12  -pi/20,  3*pi/2  - pi / 12  +pi/20) ,
                      angleofneck = runif(n, 3*pi/2-pi/10, 3*pi/2+pi/10))
p1 <- grmap6 + xkcdman(mapman , dataman) 


p2 <- p1 + annotate("text",x=0,y=4500000,label="Where do you live?", family="xkcd") +
   xkcdline(aes(xbegin=xbegin,ybegin=ybegin,xend=xend,yend=yend),
            data=data.frame( xbegin=0, ybegin=4480000, xend=-145000, yend=4420000  )
            , xjitteramount = 50000) +
 annotate("text",x=600000,y=3875000,label="The geography of unemployment", family="xkcd",size=8) +
 annotate("text",x=600000,y=3820000,label="Thousands of unemployed persons, by region", family="xkcd")

p2


p3 <- p2 + theme(
     line =               element_blank(),
     rect =               element_blank(),
     text =               element_blank(),
     axis.ticks.length =  unit(0, "cm"),
     axis.ticks.margin =  unit(0.01, "cm"), # change back to 0 when grid is fixed
     legend.position =    "none",
     panel.margin =       unit(0, "lines"),
     plot.margin =        unit(c(0, 0, -.5, -.5), "lines"),
     complete = TRUE
   ) ## theme_nothing()


p3



comgroup <- unique(mymap[,c("COM","group")])
comgroup
ptscolor <- merge(ptscolor,comgroup)
kk <- (by(ptscolor,ptscolor$COM, function(x)c(median(x$long),median(x$lat)) ))
coordenadas <- as.data.frame(do.call("rbind",as.list(kk)))
names(coordenadas) <- c("long","lat")
coordenadas$COM <-  row.names(coordenadas)
coordenadas
mydata <- merge(mydata,coordenadas)

p3 + annotate("text",x=mydata$long, y=mydata$lat,label=mydata$parados,color="black",family="xkcd")



ggsave("GeographyUnemployment.png")

