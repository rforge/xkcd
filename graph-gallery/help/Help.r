## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-05-27 Mon 09:52 emilio on emilio-despacho>
## ============================================================

# R CMD BATCH EvolucionParo.r
#install.packages("zoo")
library(zoo)
library(xkcd)
require(splancs) #install.packages("splancs", dependencies = TRUE, repos="http://cran.es.r-project.org/")

mydatar <- read.table(text="
6.202
5.965 
5.778 
5.693 
5.639 
5.273 
4.978 
4.833 
4.910 
4.696 
4.574 
4.645 
4.612
")

mydata1 <- mydatar[dim(mydatar)[1]:1,]
z <- zooreg(mydata1, end = as.yearqtr("2013-1"), frequency = 4)
z


mydata <- data.frame(parados=z)
mydata$year <- as.numeric(as.Date(as.yearqtr(rownames(mydata))))
mydata$label <- paste(substr(rownames(mydata),3,4),substr(rownames(mydata),6,7),sep="")

## mydata

## p1 <- ggplot() + geom_point(aes(year,parados), mydata) +  scale_x_date()
## lo <- loess(parados~as.numeric(year),mydata)
## predict(lo)




data <- mydata
data$xmin <- as.numeric(data$year) -1
data$xmax <- data$xmin + 90
data$ymin <- 4.5
data$ymax <- data$parados



n <- 3200
poligono <- mydata[,c("year","parados")]
names(poligono) <- c("x","y")
poligono <- rbind(poligono, c(max(poligono$x),4.4))
poligono <- rbind(poligono, c(min(poligono$x),4.4))
points <- data.frame(x=runif(n,range(poligono$x)[1],range(poligono$x)[2] ),
                     y=runif(n,range(poligono$y)[1],range(poligono$y)[2] ))
kk <- inout(points, poligono)
points <- points[kk, ]
points <- rbind(points,poligono)

x <- points$x
y <- points$y
nman <- length(x)
sizer <-runif(nman, 4, 6)
nman

xrange <- c(min(x),max(x))
yrange <- c(min(y),max(y))
ratioxy <- diff(xrange)/diff(yrange)

n <- 2
set.seed(123)
twomen <-  xkcdman(mapping= aes(x,  y,
                 scale,
                 ratioxy,
                 angleofspine ,
                 anglerighthumerus,
                 anglelefthumerus,
                 anglerightradius,
                 angleleftradius,
                 anglerightleg,
                 angleleftleg,
                 angleofneck),
          data.frame(x=c(15600, 14800) ,
                     y=c(5.3, 5.7),
                     scale = 0.2,
                     ratioxy = ratioxy,
                     angleofspine = runif(n, - pi/2 - pi/10, -pi/2 + pi/10),
                     anglerighthumerus = runif(n, -pi/6- pi/10, - pi/6 + pi/10),
                     anglelefthumerus = runif(n, pi + pi/6 -pi/10, pi + pi/6 + pi/10),
                     anglerightradius =  runif(n, -pi/4, pi/4),
                     angleleftradius =  runif(n, pi -pi/4, pi + pi/4),
                     anglerightleg = runif(n,  3* pi/2 + pi/12 , 3* pi/2  + pi/12 + pi/10),
                     angleleftleg = runif(n, 3* pi/2  - pi/12 - pi/10, 3* pi/2 - pi/12 ),
                     angleofneck = runif(n, -pi/2-pi/10, -pi/2 + pi/10)))

p1 <- ggplot() + geom_text(aes(x,y,label="0"), data.frame(x=x,y=y),family="xkcd",alpha=0.4,size=sizer) +  xkcdaxis(xrange,yrange) +
   ylab("Unemployed persons (millions)") + xlab("Date") +
 twomen +
  annotate("text", x= 15250, y=5.95,label="Help!", family="xkcd",size=7) +
   xkcdline(aes(xbegin=xbegin,ybegin=ybegin,xend=xend,yend=yend),
            data=data.frame( xbegin=15600, ybegin=5.42, xend=15250, yend=5.902  )
            , xjitteramount = 200) + theme(legend.position="none")
#p1
p2 <- p1 + scale_x_continuous(breaks=as.numeric(mydata$year),label=mydata$label)
p2

ggsave("Help.png")


