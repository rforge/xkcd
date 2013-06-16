## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-06-16 Sun 20:09 emilio on emilio-laptop2>
## ============================================================

library(mapproj)
library(ggmap) #install.packages("ggmap")
library(ggplot2) #install.packages("ggplot2")
library(grid)
load("autobus.rda")

horainicial <- strptime("2013-06-15 09:00:00","%Y-%m-%d %H:%M:%S", tz="EST5EDT")
horafinal <- strptime("2013-06-15 09:30:00","%Y-%m-%d %H:%M:%S", tz="EST5EDT")
hora <- seq(horainicial,horafinal,20)

hora
summary(datos)


#for( i in 1:nlength(hora))

## Caso uno
i <- 21
tiempoi <- hora[i]
distanciaminutos <- 5
tiempoihacenminutos <- tiempoi - 60 * distanciaminutos
tiempoi
tiempoihacenminutos
datosintervalo <- datos[ datos$hora > tiempoihacenminutos & datos$hora <= tiempoi, ] 
datoscerca <- datos[datos$hora > tiempoi - 20  & datos$hora <= tiempoi, ]
datoscerca$cercax <- round(datoscerca$utmx/10,0)
datoscerca$cercay <- round(datoscerca$utmy/10,0)

head(datos)
oo <- order(datoscerca$idautobus)
datoscerca <- datoscerca[oo,]
datoscerca <- datoscerca[ !duplicated(datoscerca[,c("idautobus")]), ]
datoscerca <- datoscerca[ duplicated(datoscerca[,c("cercax","cercay")]),]
 datoscerca<- datoscerca[ datoscerca$lon > range(GijonMap$data$lat)[1] &  # OJO que van al reves
                                   datoscerca$lon < range(GijonMap$data$lat)[2] &
                                   datoscerca$lat > range(GijonMap$data$lon)[1] &
                                   datoscerca$lat < range(GijonMap$data$lon)[2],  ]


datoscerca
ggplot()+geom_text(aes(x=lat, y=lon, label="¡Te pillé!"), hjust=0, vjust=0,data= datoscerca, family="xkcd") 
GijonMap +geom_text(aes(x=lat, y=lon, label="¡Te pillé!"), hjust=0, vjust=0,data= datoscerca, family="xkcd")

str(GijonMap)
range(GijonMap$data$lon)
range(GijonMap$data$lat)

datoscerca

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
library(RColorBrewer)

lineas <- geom_path(aes(x=lat, y=lon, group=idautobus ,size = as.numeric(hora), color=as.factor(linea)), data = datosintervalo )
ggplot()+lineas  +  scale_fill_brewer(palette="Set3") + geom_point(aes(x=lat, y=lon, group=idautobus ,size = as.numeric(hora), color=as.factor(linea)),  data = datosintervalo) +
        scale_size_continuous(range = c(0, 2.5))


#### Mapa de Gijón
##  al1 = get_map(location = c(lon = -5.664, lat = 43.531), zoom = 14, maptype = 'roadmap',color='bw')
## GijonMap = ggmap(al1)

##save(GijonMap,file="GijonMap.rda")
## GijonMap <- ggmap(al1,darken=c(0.4,"grey"))
## ## GijonMap
## save(al1, GijonMap,file="GijonMap.rda")

load("GijonMap.rda")
library(mapproj)


GijonMap +  annotate("text", x = -5.667, y=43.547, label="Corre, corre, que te pillo",size=12,family="xkcd")

datosintervalo2 <- datosintervalo
datosintervalo2$lon <- datosintervalo$lon - 0.0019
datosintervalo2$lat <- datosintervalo$lat - 0.001
dib1 <- GijonMap +  geom_path(aes(x=lat, y=lon, group=idautobus ,size = as.numeric(hora), color=as.factor(linea)), data = datosintervalo2,alpha=0.4 )  +  scale_fill_brewer(palette="Set3") + geom_point(aes(x=lat, y=lon, group=idautobus ,size = as.numeric(hora),  color=as.factor(linea)),  data = datosintervalo2, alpha=0.2) +
        scale_size_continuous(guide = 'none',range = c(0, 2.9))+    theme(
     line =               element_blank(),
     rect =               element_blank(),
     ##text =               element_blank(),
     axis.ticks.length =  unit(0, "cm"),
     axis.ticks.margin =  unit(0.01, "cm"), # change back to 0 when grid is fixed
     legend.position =    "none",
                        legend.position=c(0.15,0.87655),
     panel.margin =       unit(0, "lines"),
     plot.margin =        unit(c(0, 0, -.5, -.5), "lines"),
     complete = TRUE
   ) + theme(panel.grid.major = element_blank(), axis.ticks =  element_blank(), 
            panel.background = element_blank(), panel.grid.minor = element_blank(), 
            legend.key = element_blank(), strip.background = element_blank(),
        axis.line = element_blank(),
       axis.text.x = element_blank(), axis.text.y = element_blank(),
       axis.ticks = element_blank(), 
       axis.title.x = element_blank(), axis.title.y = element_blank(),
            text = element_text(size = 16, family = "xkcd")) +
  annotate("text", x = -5.667, y=43.547, label="Corre, corre, que te pillo",size=12,family="xkcd")
## theme_nothing() 

dib1

dib1 + geom_text(aes(x=lat, y=lon, label="¡Te pillé!"), hjust=0, vjust=0,data= datoscerca, family="xkcd") 


datoscerca

 for( i in 1:length(hora)) {
## for( i in 1:3) {
 tiempoi <- hora[i]
distanciaminutos <- 5
tiempoihacenminutos <- tiempoi - 60 * distanciaminutos
datosintervalo <- datos[ datos$hora > tiempoihacenminutos & datos$hora <= tiempoi, ]
datoscerca <- datos[datos$hora > tiempoi - 19  & datos$hora <= tiempoi, ]
datoscerca$cercax <- round(datoscerca$utmx/10,0)
datoscerca$cercay <- round(datoscerca$utmy/10,0)
datoscerca <- datoscerca[ !duplicated(datoscerca[,c("idautobus")]), ]
datoscerca <- datoscerca[ duplicated(datoscerca[,c("cercax","cercay")]),]
 datoscerca<- datoscerca[ datoscerca$lon > range(GijonMap$data$lat)[1] &  # OJO que van al reves
                                   datoscerca$lon < range(GijonMap$data$lat)[2] &
                                   datoscerca$lat > range(GijonMap$data$lon)[1] &
                                   datoscerca$lat < range(GijonMap$data$lon)[2],  ]
 ##
datosintervalo2 <- datosintervalo
datosintervalo2$lon <- datosintervalo$lon - 0.0019
datosintervalo2$lat <- datosintervalo$lat - 0.001
dibujito <- GijonMap +  geom_path(aes(x=lat, y=lon, group=idautobus ,size = as.numeric(hora), color=as.factor(linea)), data = datosintervalo2, alpha=0.4 )  +  scale_fill_brewer(palette="Set3") + geom_point(aes(x=lat, y=lon, group=idautobus ,size = as.numeric(hora), color=as.factor(linea), alpha=0.2),  data = datosintervalo2) +
        scale_size_continuous(guide = 'none',range = c(0, 2.9))+    theme(
     line =               element_blank(),
     rect =               element_blank(),
     ##text =               element_blank(),
     axis.ticks.length =  unit(0, "cm"),
     axis.ticks.margin =  unit(0.01, "cm"), # change back to 0 when grid is fixed
     legend.position =    "none",
                        legend.position=c(0.15,0.87655),
     panel.margin =       unit(0, "lines"),
     plot.margin =        unit(c(0, 0, -.5, -.5), "lines"),
     complete = TRUE
   ) + theme(panel.grid.major = element_blank(), axis.ticks =  element_blank(), 
            panel.background = element_blank(), panel.grid.minor = element_blank(), 
            legend.key = element_blank(), strip.background = element_blank(),
        axis.line = element_blank(),
       axis.text.x = element_blank(), axis.text.y = element_blank(),
       axis.ticks = element_blank(), 
       axis.title.x = element_blank(), axis.title.y = element_blank(),
            text = element_text(size = 16, family = "xkcd")) +
  annotate("text", x = -5.667, y=43.547, label="Corre, corre, que te pillo",size=12,family="xkcd")
## if(dim(datoscerca)[1]>0) {
##   print("te pille")
##   dibujito <- dibujito +
##      geom_text(aes(x=lat, y=lon, label="¡Te pillé!"), hjust=0, vjust=0,data= datoscerca, family="xkcd")
##   }
filename <- "Dibujos/GrAuto"
print(i)
  png(paste(filename,formatC(i, width = 4, format = "d", flag = "0"),".png",sep=""),res=72)
  print(dibujito)
  dev.off()
}


##  convert -delay 0 -loop 0 GrAuto*.png test.gif
