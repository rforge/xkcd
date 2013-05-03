library(ggplot2)
library(grid)
library(Hmisc)
library(R.utils) # doCall
datascaled <- data.frame(x=c(-3,3),y=c(-30,30))
dibujobase <- ggplot(data=datascaled, aes(x=x,y=y)) + geom_point()
xrange <- 6
yrange <- 60
ratioxy <- xrange / yrange

ratioxy


circlehead <- function(center = c(0,0),diameter = 1, ratioxy=1, npoints = 16,     alpha=  runif(1, 0, pi/2), ...){
    r <- rep( diameter / 2, npoints )

    tt <- seq(alpha,2*pi + alpha,length.out = npoints)
    r <- jitter(r, factor=0.49, ...)
    sector <-  tt > alpha & tt <= ( pi/ 2 + alpha)
    r[ sector ] <- r[sector] * 1.05
    sector <-  tt > ( 2 * pi/2 + alpha)  & tt < (3* pi/ 2 +alpha)
    r[ sector ] <- r[sector] * 0.95    
    xx <- center[1] + r * cos(tt) * ratioxy
    yy <- center[2] + r * sin(tt) 
    ##return(data.frame(x = xx, y = yy))
    return(data.frame(bezier(x = xx, y =yy, ...)))
}


head <- function(center = c(0,0), diameter = 1, ratioxy=1, npoints = 100,... ) {
  data <- circlehead(center,diameter, ratioxy, npoints,... )
  geom_path(mapping=aes(x,y),data = data,...)
}


dibujobase + head(ratioxy=ratioxy)


dibujobase +cartooncharacterequalscaled(ratioxy=ratioxy,scale=10) +
  cartooncharacterequalscaled(center=c(-2,00),ratioxy=ratioxy,scale=10) +
  cartooncharacterequalscaled(center=c(2,20),ratioxy=ratioxy,scale=10) +
  cartooncharacterequalscaled(center=c(-2,20),ratioxy=ratioxy,scale=10)

## ============================================================
## Linea tronco, brazos, piernas
## ============================================================

library(grid)

?bezier

databone <- function( begin= c(0,0), distance = 1, angle = pi/2, ratioxy=1, npoints = 10, ... ) {
  require(Hmisc) # bezier
  end <- c( begin[1] + distance * cos( angle ) * ratioxy, begin[2] + distance * sin(angle) )
  middleoriginal <- (begin + end )/2
  middle <- jitter(middleoriginal, factor=0.1,...)
  ##middle <- middleoriginal
  data <- data.frame(bezier(c(begin[1], middle[1], end[1]),  # Generate
                            c(begin[2], middle[2], end[2]),  # X & y
                            ...))
  data
}

bone <- function( begin= c(0,0), distance = 1, angle = pi/2, ratioxy = 1, npoints = 10, ... ) {
  geom_path(mapping=aes(x,y),data = databone(begin, distance, angle, ratioxy,npoints, ...) , ...)
}





data


ggplot() + bone() +head(center=c(0,1),diameter=0.3)

dibujobase + bone(ratioxy=ratioxy) + head(ratioxy=ratioxy)

## ============================================================
## cartoon character
## ============================================================





cartooncharacterequalscaled <- function( centerofhead = c(0,0),
                             scale = 1,
                                        ratioxy =1,
                             angleofspine =   - pi / 2 ,
                             anglerighthumerus = -pi/6,
                             anglelefthumerus = pi + pi/6,
                             anglerightradius = 0,
                             angleleftradius = - pi/4,
                             angleleftleg = 3*pi/2  + pi / 12 ,
                             anglerightleg = 3*pi/2  - pi / 12,
                             angleofneck = runif(1, min = 3 * pi / 2 - pi/10 , max = 3 * pi / 2 + pi/10)) {

diameterofhead <- 1 * scale
lengthofspine <- diameterofhead * 1
lengthofleg <- lengthofspine * 1.2
lengthofhumerus <- lengthofspine * 0.6
lengthofradius <- lengthofspine * 0.5
beginspine <- centerofhead + (diameterofhead / 2) * c( cos(angleofneck) * ratioxy, sin( angleofneck))
endspine <- beginspine + lengthofspine * c( cos( angleofspine) * ratioxy , sin(angleofspine))
endrighthumerus <- beginspine + lengthofhumerus * c( cos( anglerighthumerus) *ratioxy, sin(anglerighthumerus))
endlefthumerus <- beginspine + lengthofhumerus * c( cos( anglelefthumerus)*ratioxy, sin(anglelefthumerus))

c(head(center=centerofhead,diameter=diameterofhead, ratioxy = ratioxy), # head
  bone(begin = beginspine, distance = lengthofspine , angle = angleofspine, ratioxy = ratioxy  ), # spine
  bone(begin = beginspine, distance = lengthofhumerus, angle = anglerighthumerus, ratioxy = ratioxy) , # right humerus
  bone(begin = endrighthumerus, distance = lengthofradius, angle = anglerightradius , ratioxy = ratioxy),
  bone(begin = beginspine, distance = lengthofhumerus, angle = anglelefthumerus,ratioxy = ratioxy),
  bone(begin = endlefthumerus, distance = lengthofradius, angle = angleleftradius, ratioxy = ratioxy),
  bone(begin = endspine, distance = lengthofleg, angle = angleleftleg, ratioxy = ratioxy), # Leg
  bone(begin = endspine, distance = lengthofleg, angle = anglerightleg, ratioxy = ratioxy)) #Leg
}
  


#ggplot() +cartooncharacterequalscaled()

dibujobase +cartooncharacterequalscaled(ratioxy=ratioxy,scale=10) +
  cartooncharacterequalscaled(center=c(-2,00),ratioxy=ratioxy,scale=10) +
  cartooncharacterequalscaled(center=c(2,20),ratioxy=ratioxy,scale=10) +
  cartooncharacterequalscaled(center=c(-2,20),ratioxy=ratioxy,scale=10)


+ coord_equal()




#####################################################################################
##

datasegment <- function( begin= c(0,0), end=c(1,1), npoints = 10, bezier= TRUE, ... ) {
  require(Hmisc) # bezier
  require(R.utils) # doCall
  x <- seq(begin[1],end[1],length.out = npoints)
  if( (end[1] - begin[1]) != 0 ) {
    y <- (end[2] - begin[2]) * ( x - begin[1] ) / (end[1] - begin[1]) + begin[2]
  } else {
    y <-  seq(begin[2], end[2], length.out = npoints)
  }
  xx <- doCall("jitter", x=x, ...)
  yy <- doCall("jitter", x=y, ...)
  xx[1] <- begin[1]
  yy[1] <- begin[2]
  xx[length(xx)] <-end[1]
  yy[length(yy)] <-end[2]
  data <- data.frame(x=xx,y=yy)
  if(bezier) data <- data.frame(doCall("bezier", x=xx, y=yy, ...))
  data
}

npoints <- 10
begin <- c(0,0)
end <- c(1,1)
  x <- seq(begin[1],end[1],length.out = npoints)
  print(x)
  y <- (end[2] - begin[2]) * ( x - begin[1] ) / (end[1] - begin[1]) + begin[2]
y

datasegment()

path <- function(data, mask = TRUE, ...) {
  if(mask) {
     argList<-list(...);
	 if(is.null(argList$size)==TRUE){
       mymask <- geom_path(mapping=aes(x,y),data = data , colour = "white", size = 3, ...)
     } else {
       if(argList$size < 3 ) size <- 3
       else size <- argList$size *2
       mymask <- geom_path(mapping=aes(x,y),data = data , colour = "white", size = size, ...)
     }    
    return(c(mymask,geom_path(mapping=aes(x,y),data = data , ...)))
  }
  else {
  return(geom_path(mapping=aes(x,y), data = data , ...))
  }
}


segment <- function(begin= c(0,0), end=c(1,1), ... ) {
  data <- datasegment(begin=begin, end=end, ...)
  ##doCall("path", data = data, ...))
  path(data,...)
}


dibujobase + segment(begin=c(0,0), end=c(10,10),npoints=10,factor=3)

dibujobase + segment(begin=c(0,0), end=c(0,10)) + segment(begin=c(0,0), end=c(0,-10),npoints=50,factor=10, colour="red") + segment(begin=c(-2,5), end=c(2,5), npoints=30,factor=10, size =4) 

dibujobase + segment(begin=c(0,0), end=c(0,10),npoints=10, factor=0.22,bezier =FALSE)
datasegment()

ggplot()+ segment(begin=c(0,0), end=c(0,10),npoints=10, factor=0.12)


ggplot()  +cartooncharacter() + coord_equal() 


cartooncharacter <- function( centerofhead = c(0,0),
                             scale = 1,
                             xrange = 10,
                             yrange = 100,
                             angleofspine =   - pi / 2 ,
                             anglerighthumerus = -pi/6,
                             anglelefthumerus = pi + pi/6,
                             anglerightradius = 0,
                             angleleftradius = - pi/4,
                             angleleftleg = 3*pi/2  + pi / 12 ,
                             anglerightleg = 3*pi/2  - pi / 12,
                             angleofneck = runif(1, min = 3 * pi / 2 - pi/10 , max = 3 * pi / 2 + pi/10)) {

diameterofhead <- 1 * scale
lengthofspine <- diameterofhead * 1
lengthofleg <- lengthofspine * 1.2
lengthofhumerus <- lengthofspine * 0.6
lengthofradius <- lengthofspine * 0.5
beginspine <- centerofhead + (diameterofhead / 2) * c( cos(angleofneck), sin( angleofneck))
endspine <- beginspine + lengthofspine * c( cos( angleofspine), sin(angleofspine))
endrighthumerus <- beginspine + lengthofhumerus * c( cos( anglerighthumerus), sin(anglerighthumerus))
endlefthumerus <- beginspine + lengthofhumerus * c( cos( anglelefthumerus), sin(anglelefthumerus))

c(head(center=centerofhead,diameter=diameterofhead), # head
  bone(begin = beginspine, distance = lengthofspine , angle = angleofspine, factor = 1.2  ), # spine
  bone(begin = beginspine, distance = lengthofhumerus, angle = anglerighthumerus, factor = 0.20) , # right humerus
  bone(begin = endrighthumerus, distance = lengthofradius, angle = anglerightradius , factor = 0.20),
  bone(begin = beginspine, distance = lengthofhumerus, angle = anglelefthumerus, factor = 0.20),
  bone(begin = endlefthumerus, distance = lengthofradius, angle = angleleftradius, factor = 0.20),
  bone(begin = endspine, distance = lengthofleg, angle = angleleftleg, factor = 0.20), # Leg
  bone(begin = endspine, distance = lengthofleg, angle = anglerightleg, factor = 0.20)) # Leg
}
  
 


+head(center=c(0,1),diameter=0.3)


?geom_path
?stat_smooth

jitter(r)
bezierGrob
grob
validGrob


##And a demonstration of its use:
dat <- circleFun(c(1,-1),2.3,npoints = 100, factor=0.5)
#geom_path will do open circles, geom_polygon will do filled circles
p1 <- ggplot(dat,aes(x,y)) + geom_path()
p1

dat2 <- circleFun(c(1,-1),2.4,npoints = 100)
p2 <- ggplot(dat2,aes(x,y)) + geom_path()

p1 + p2

ggplot() + geom_path(mapping=aes(x,y),data = dat) +  geom_path(mapping=aes(x,y),data = dat2)


cabeza()

ggplot() +  geom_path(mapping=aes(x,y),data = dat2) +cabeza()  

?jitter


data(iris)
iris

ggplot() + geom_path(mapping=aes(x=Sepal.Length,y=Sepal.Width), data = iris) + geom_path(mapping=aes(x=Petal.Length,y=Petal.Width), data = iris)

fun1 <- function(data) {
  names(data) <- c("x","y")
  geom_path(mapping=aes(x=x,y=y), data = data)
}

ggplot() + fun1(iris[,c(1:2)]) + fun1(iris[,c(3:4)])  # Funciona

fun2 <- function(data) {
  c(geom_path(mapping=aes(x=Sepal.Length,y=Sepal.Width), data = data), geom_path(mapping=aes(x=Petal.Length,y=Petal.Width), data = data))
}

ggplot() + fun2(iris)

geom_path(mapping=aes(x=Sepal.Length,y=Sepal.Width), data = iris) + geom_path(mapping=aes(x=Petal.Length,y=Petal.Width), data = iris)
