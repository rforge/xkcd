## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-05-26 Sun 13:57 emilio on emilio-laptop2>
## ============================================================


library(xkcd)
library(reshape)

mydata <- read.table(header=TRUE,sep=",",text="
year,tasa
1561,16
1888,18
2162,20
2424,21
2674,23
2924,25
3174,26
3415,28
4298,32
5182,35
13524,45
17528,47
")
mydata

mydatalong <- melt(mydata, id="year", measure.vars= names(mydata)[-1])
mydatalong

xrange <- c(1500,20000)#range(mydatalong$year)
yrange <- range(mydatalong$value)
set.seed(123)
##p <- ggplot() + geom_smooth(aes(x=year, y=value, group=variable,linetype=variable), data=mydatalong, position = position_jitter(h=0.0001),color="black") + theme(legend.position = "none") + xkcdaxis(xrange,yrange)
p <- ggplot() + geom_smooth(aes(x=year, y=value, group=variable,linetype=variable), data=mydatalong,color="black",se=FALSE) + theme(legend.position = "none") 




xkcdaxislog <- function (xrange, yrange) {
    if (is.null(xrange) | is.null(yrange)) 
        stop("Arguments are: xrange, yrange")
    xjitteramount <- diff(xrange)**(1/2)
    yjitteramount <- diff(yrange)/50
    dataaxex <- data.frame(xbegin = xrange[1] - xjitteramount, 
        ybegin = yrange[1] - yjitteramount, xend = xrange[2] + 
            xjitteramount, yend = yrange[1] - yjitteramount)
    mappingsegment <- with(dataaxex, aes(xbegin = xbegin, ybegin = ybegin, 
        xend = xend, yend = yend))
    axex <- xkcdline(mappingsegment, dataaxex, yjitteramount = yjitteramount, 
        mask = FALSE)
    dataaxey <- data.frame(xbegin = xrange[1] - xjitteramount, 
        ybegin = yrange[1] - yjitteramount, xend = xrange[1] - 
            xjitteramount, yend = yrange[2] + yjitteramount)
    mappingsegment <- with(dataaxey, aes(xbegin = xbegin, ybegin = ybegin, 
        xend = xend, yend = yend))
    axey <- xkcdline(mappingsegment, dataaxey, xjitteramount = xjitteramount, 
        mask = FALSE)
    coordcarte <- coord_cartesian(xlim = xrange + 1.5 * c(-xjitteramount, 
        xjitteramount), ylim = yrange + 1.5 * c(-yjitteramount, 
        yjitteramount))
    list(c(axex, axey), coordcarte, theme_xkcd())
}

n <- length(mydata$year)
p2 <- p + ylab("Spanish income tax rates (%)") + xlab("Monthly salary (€)") +  scale_x_log10(breaks=c(mydata$year),   labels=c(mydata$year)) + xkcdaxislog(xrange,yrange) +
  theme(    axis.text.x  = element_text(angle=30, vjust=0.5))
p2

n <- length(mydatalong$year)
p3 <- p2 + geom_text(aes(x=year, y=value, label=value), mydatalong, family="xkcd",size=3*(1:n), alpha=0.4+(1:n)/(n+20), hjust=1, vjust=1)

p3

ratioxy <-  diff(xrange) / diff(yrange)
x <- 12000
y <-  30
scale <- 5

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
set.seed(123)
dataman <- data.frame( x= x,
                       y= y,
                      scale = scale ,
                      ratioxy = ratioxy,
                      angleofspine = runif(n, -pi/2-pi/8 ,-pi/2),
                      anglerighthumerus = pi + pi/4,
                      anglelefthumerus = pi + pi/3,
                      anglerightradius = runif(n,pi+ -pi/5 - pi/10, pi + -pi/5 + pi/10),
                      angleleftradius = pi + pi/6,
                      angleleftleg = runif(n, 3*pi/2  + pi / 12  -pi/20,  3*pi/2  + pi / 12  +pi/20) ,
                      anglerightleg =  runif(n, 3*pi/2  - pi / 12  -pi/20,  3*pi/2  - pi / 12  +pi/20) ,
                      angleofneck = runif(n, 3*pi/2-pi/10, 3*pi/2+pi/10))
g1 <- p3 + xkcdman(mapman , dataman) 
g1




g2 <- g1 + annotate("text",x=x-3000,y=y+5,label="If I Were a Rich Man ", family="xkcd")
g2

g3 <- g2 +   xkcdline(aes(xbegin=xbegin,ybegin=ybegin,xend=xend,yend=yend),
            data=data.frame( xbegin=x-3000, ybegin=y+4.5, xend=x-1500, yend=y+1  )
            , xjitteramount = 300)



g3
ggsave("IfIWereRichMan.png")


element_text

theme
?axis.text.x
