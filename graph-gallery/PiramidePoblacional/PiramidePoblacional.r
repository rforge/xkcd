## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-06-13 Thu 21:15 emilio on emilio-laptop2>
## ============================================================

##http://stackoverflow.com/questions/14680075/simpler-population-pyramid-in-ggplot2


library(xkcd)

## http://koenbro.blogspot.com.es/2013/03/population-pyramid.html


test <- data.frame(v=sample(1:20,1000,replace=T), g=c('M','F'))

head(test)

require(ggplot2)
require(plyr)    
ggplot(data=test,aes(x=as.factor(v),fill=g)) + 
  geom_bar(subset=.(g=="F")) + 
  geom_bar(subset=.(g=="M"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()


load("resumenmigraciones.rda")
resumen

resumenlargo <- melt(resumen[,c("tonombre","llegan","sevan")])
resumenlargo

resumenlargo$tonombre

oo <- order(resumen$llegan)
nombreordenados <- (resumen$tonombre)[oo]
nombreordenados

resumenlargo$tonombre <- factor( resumenlargo$tonombre, levels=nombreordenados, ordered=TRUE)

## ggplot(data=resumenlargo,aes(x=tonombre,fill=variable)) + 
##   geom_bar(subset=.(variable=="llegan"), aes(y= value ), stat="identity") + 
##   geom_bar(subset=.(variable=="sevan"),aes(y= value *(-1)), stat="identity") + 
##  scale_y_continuous(breaks=seq(-1.3,1.6,0.3),labels=abs(seq(-1.3,1.6,0.3))) +
##   ylab("Movilidad de los asalariados (% sobre asalariados residentes)") +
##   coord_flip() +
##   theme_xkcd() + xlab("") +  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

set.seed(130613)
kk <- ggplot() + 
  geom_bar( aes(y= value, x=tonombre,fill=variable ), data=resumenlargo[resumenlargo$variable=="llegan", ], stat="identity") + 
  geom_bar(aes(y= (-1)* value, x=tonombre,fill=variable ), data=resumenlargo[resumenlargo$variable=="sevan", ], stat="identity") + 
 scale_y_continuous(breaks=seq(-1.2,1.5,0.3),labels=abs(seq(-1.2,1.5,0.3))) +
  ylab("Movilidad de los asalariados (% sobre asalariados residentes)") +
  coord_flip() +
  theme_xkcd() + xlab("") +  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())



kk2 <- kk +
  geom_text(aes(x=(tonombre),y=0,label=tonombre), data=resumenlargo,family="xkcd")


kk2


resumen
lleganespana <- sum(resumen$topersons)*100/ sum(resumen$persons)
sevanespana <- sum(resumen$frompersons)*100/ sum(resumen$persons)

lleganespana
sevanespana


lineaespana1 <-   xkcdline(mapping=aes(xbegin=1-0.5,ybegin=lleganespana,xend=15+0.5, yend=lleganespana, yjitteramount=0.051), data= resumenlargo, linetype=2,mask=FALSE)
lineaespana2 <- xkcdline(mapping=aes(xbegin=1-0.5,ybegin=-lleganespana,xend=15+0.5, yend=-lleganespana), yjitteramount=0.051, data= resumenlargo,linetype=2,mask=FALSE)


kk3 <- kk2  +   xkcdline(mapping=aes(xbegin=as.numeric(tonombre)-0.5,ybegin=-1.24,xend=as.numeric(tonombre)-0.5, yend=1.52, xjitteramount=0.151), data= resumenlargo, size=3,color="white") + lineaespana1 + lineaespana2
 

kk4 <- kk3 + annotate("text",x=1, y=c(lleganespana,-lleganespana),label="Media de España", hjust=c(-0.11,-0.11), vjust=c(-0.1,0.1),family="xkcd",angle=90)


kk5 <- kk4 + scale_fill_discrete(name="",
                         breaks=c("llegan", "sevan"),
                         labels=c("Llegan", "Se van")) + theme(legend.justification=c(0,0), legend.position=c(0,0))



kk5

xrange <- c(1,15)
yrange <- c(-1.3,1.6)
ratioxy <- diff(xrange)/diff(yrange)
x <- 7
y <-  1.5
scale <- 0.35
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
set.seed(130613)
datamanflip <- data.frame( x= x,
                       y= y,
                      scale = scale ,
                      ratioxy = ratioxy,
                          angleofspine = runif(n, -pi/2 -pi/2 - pi/10,-pi/2 -pi/2 + pi/10),
                      ##angleofspine = runif(n, -0 - pi/10,-0 + pi/10),
                      anglerighthumerus = runif(n, -pi/2-pi/6-pi/10, -pi/2 -pi/6+pi/10),
                      anglelefthumerus = runif(n, -pi/2-pi/2 - pi/10, -pi/2 -pi/2 + pi/10),
                      anglerightradius = runif(n, -pi/2-pi/5 - pi/10, -pi/2-pi/5 + pi/10),
                      angleleftradius = runif(n, -pi/2-pi/5 - pi/10, -pi/2-pi/5 + pi/10),
                      angleleftleg = runif(n, -pi/2 + 3*pi/2  + pi / 12  -pi/20,-pi/2  +3*pi/2  + pi / 12  +pi/20) ,
                      anglerightleg =  runif(n, -pi/2 + 3*pi/2  - pi / 12  -pi/20, -pi/2+ 3*pi/2  - pi / 12  +pi/20) ,
                      angleofneck = runif(n, -pi/2+3*pi/2-pi/10, -pi/2+3*pi/2+pi/10))
p1 <-  xkcdman(mapman , datamanflip) 

kk6 <- kk5 + p1


kk7 <- kk6 + annotate("text", x=9.3, y = 1.3, label="Unos vienen, otros se van",family="xkcd" ) +
  xkcdline(aes(xbegin=xbegin,xend=xend,yend=yend,ybegin=ybegin), yjitteramount=0.135,data=data.frame(xbegin=9.0, xend=7.2, ybegin=1.2, yend=1.3))


kk7
ggsave(kk7,filename="GrUnosVienenOtrosSeVan.png")



