# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-01 Wed 16:03 emilio on emilio-laptop2>
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

data <- data.frame(x1=c(1,2), y1=c(10,20), xend1=c(1.5,2.5), yend1=c(15,25), color=c("red","green"), car=c("bmw","seat"))
data


segment <- function(mapping, data, ...) {
  xkcdline(mapping=mapping, data=data, typexkcdline="segment", ...)
}


dibujobase + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour = color) , data = data)
dibujobase + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour = color) , data = data,xjitteramount=3)
dibujobase + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour = color) , data = data, xjitteramount=c(0.1,3))

dibujobase + segment(mapping=aes(x=x1, y=y1, xend=xend1, yend=yend1, colour=color) , data = data)  + facet_grid(. ~ color)


man <- function(mapping=aes(x=x,
                  y=y,
                  scale=scale,
                  ratioxy=ratioxy,
                  angleofspine = angleofspine,
                  anglerighthumerus = anglerighthumerus,
                  anglelefthumerus = anglelefthumerus,
                  anglerightradius = anglerightradius,
                  angleleftradius = angleleftradius,
                  angleleftleg = angleleftleg,
                  anglerightleg =  anglerightleg,
                  angleofneck = angleofneck ),
                data=data.frame( x= 0, y=0,
                  scale = 1,
                  ratioxy =1,
                  angleofspine =   - pi / 2 ,
                  anglerighthumerus = -pi/6,
                  anglelefthumerus = pi + pi/6,
                  anglerightradius = 0,
                  angleleftradius = - pi/4,
                  angleleftleg = 3*pi/2  + pi / 12 ,
                  anglerightleg = 3*pi/2  - pi / 12,
                  angleofneck = runif(1, min = 3 * pi / 2 - pi/10 , max = 3 * pi / 2 + pi/10)),
                ...) {

  requiredaesthetics <-  c("x","y",
                           "scale",
                           "ratioxy",
                           "angleofspine",
                           "anglerighthumerus",
                           "anglelefthumerus",
                           "anglerightradius",
                           "angleleftradius",
                           "angleleftleg", 
                           "anglerightleg",
                           "angleofneck")
  if( any(!  requiredaesthetics %in% names(mapping)))
    stop("mapping=aes(x= , y= , xend=, yend= ) must contain ... variables")
  
  ## We transform the data to get a default mapping
  defaultmapdat <- createdefaultmappinganddata(mapping, data) 
  data <-defaultmapdat$data
  mapping <- defaultmapdat$mapping
  
  centerofhead <- cbind(data$x,data$y)
  diameterofhead <-  data$scale
  lengthofspine <- diameterofhead 
  lengthofleg <- lengthofspine * 1.2
  lengthofhumerus <- lengthofspine * 0.6
  lengthofradius <- lengthofspine * 0.5
  beginspine <- centerofhead + (diameterofhead / 2) * cbind( cos(data$angleofneck) * ratioxy, sin( data$angleofneck))
  endspine <- beginspine + lengthofspine * cbind( cos( data$angleofspine) * ratioxy , sin(data$angleofspine))
  endrighthumerus <- beginspine + lengthofhumerus * cbind( cos( data$anglerighthumerus) *ratioxy, sin(data$anglerighthumerus))
  endlefthumerus <- beginspine + lengthofhumerus * cbind( cos( data$anglelefthumerus)*ratioxy, sin(data$anglelefthumerus))
  
  bone <- function(begin, distance, angle, ratioxy, mapping, data, ... ) {
    end <- cbind( begin[,1] + distance * cos( angle ) * ratioxy, begin[,2] + distance * sin(angle) )
    data$x <- begin[,1]
    data$y <- begin[,2]
    data$xend <- end[,1]
    data$yend <- end[,2]
    
    ttmapping <- unlist(mapping)
    ttmapping$xend <- parse(text = "xend")[[1]]
    ttmapping$yend <- parse(text = "yend")[[1]]
    newmapping <- structure(ttmapping, class = "uneval")
    newmapping
    
    segment(mapping=newmapping, data=data, ...)
  }
  
  head <- function(centerofhead, diameter, ratioxy , mapping, data,...) {
    data$diameter <- diameter
    
    ttmapping <- unlist(mapping)
    ttmapping$diameter <- parse(text = "diameter")[[1]]
    newmapping <- structure(ttmapping, class = "uneval")
    xkcdline(mapping = newmapping, data =data, typexkcdline="circle", ...) 
  }
  
  c(head(centerofhead=centerofhead, diameter = diameterofhead, ratioxy = data$ratioxy, mapping = mapping, data = data, ...),
    bone(begin = beginspine, distance = lengthofspine, angle = data$angleofspine, ratioxy = data$ratioxy, mapping =mapping, data = data, ...  ),
    bone(begin = beginspine, distance = lengthofhumerus, angle = data$anglerighthumerus, ratioxy = data$ratioxy, mapping =mapping, data = data, ...) , # right humerus
    bone(begin = endrighthumerus, distance = lengthofradius, angle = data$anglerightradius , ratioxy = data$ratioxy, mapping =mapping, data = data, ...),
    bone(begin = beginspine, distance = lengthofhumerus, angle = data$anglelefthumerus, ratioxy = data$ratioxy, mapping =mapping, data = data, ...),
    bone(begin = endlefthumerus, distance = lengthofradius, angle = data$angleleftradius, ratioxy = data$ratioxy, mapping =mapping, data = data, ...),
    bone(begin = endspine, distance = lengthofleg, angle = data$angleleftleg, ratioxy = data$ratioxy, mapping =mapping, data = data, ...), # Leg
    bone(begin = endspine, distance = lengthofleg, angle = data$anglerightleg, ratioxy= data$ratioxy, mapping =mapping, data = data, ...)
    ) #Leg
}

kk <- man(data =data.frame( x= rep(0,7), y=0,
                  scale = 5,
                  ratioxy = ratioxy,
                  angleofspine =   - pi / 2 ,
                  anglerighthumerus = -pi/6,
                  anglelefthumerus = pi + pi/6,
                  anglerightradius = 0,
                  angleleftradius = - pi/4,
                  angleleftleg = 3*pi/2  + pi / 12 ,
                  anglerightleg = 3*pi/2  - pi / 12,
                  angleofneck = runif(1, min = 3 * pi / 2 - pi/10 , max = 3 * pi / 2 + pi/10)))



dibujobase +kk
mapping <- aes(x=x,y=y, color=color)


mapping <- aes(x=x,
                  y=y,
                  scale=scale,
                  ratioxy=ratioxy,
                  angleofspine = angleofspine,
                  anglerighthumerus = anglerighthumerus,
                  anglelefthumerus = anglelefthumerus,
                  anglerightradius = anglerightradius,
                  angleleftradius = angleleftradius,
                  angleleftleg = angleleftleg,
                  anglerightleg =  anglerightleg,
                  angleofneck = angleofneck,
             color = color)

data <- data.frame( x= c(-1,0,1), y=c(-10,0,10),
                  scale = c(10,7,5),
                  ratioxy = ratioxy,
                  angleofspine =  seq(- pi / 2, -pi/2 + pi/8, l=3) ,
                  anglerighthumerus = -pi/6,
                  anglelefthumerus = pi + pi/6,
                  anglerightradius = 0,
                  angleleftradius = runif(3,- pi/4, pi/4),
                  angleleftleg = 3*pi/2  + pi / 12 ,
                  anglerightleg = 3*pi/2  - pi / 12,
                  angleofneck = runif(3, min = 3 * pi / 2 - pi/10 , max = 3 * pi / 2 + pi/10),
                   color=c(1,2,3))


kk <- man(mapping=mapping,data)
dibujobase + kk
dibujobase + kk +  facet_grid(. ~ color)

ttmapping
tt
aes_string
