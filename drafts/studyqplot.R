library(ggplot2)
library(grid)

qplot
?rep

runif
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}


## ============================================================
## cabeza
## ============================================================

## http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circlehead <- function(center = c(0,0),diameter = 1, npoints = 100, ...){
    r <- rep( diameter / 2, npoints )
    alpha <-  runif(1, pi/6, pi/3)
    tt <- seq(alpha,2*pi + alpha,length.out = npoints)
    r <- jitter(r, factor=0.49, ...)
    sector <-  tt > alpha & tt <= ( pi/ 2 + alpha)
    r[ sector ] <- r[sector] * 1.05
    sector <-  tt > ( 2 * pi/2 + alpha)  & tt < (3* pi/ 2 +alpha)
    r[ sector ] <- r[sector] * 0.95    
    xx <- center[1] + r * cos(tt) 
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}

head <- function(center = c(0,0), diameter = 1, npoints = 100,... ) {
  data <- circlehead(center,diameter, npoints )
  geom_path(mapping=aes(x,y),data = data,...)
}


ggplot() + head() + geom_path(mapping=aes(x,y), data = circleFun() )



## ============================================================
## Linea tronco, brazos, piernas
## ============================================================

library(grid)

?bezier

databone <- function( begin= c(0,0), distance = 1, angle = pi/2, npoints = 10, ... ) {
  require(Hmisc) # bezier
  middleoriginal <- c(begin[1] + distance * cos( angle ) / 2  ,begin[2] +  distance * sin(angle) / 2)
  middle <- jitter(middleoriginal, ...)
  end <- c( begin[1] + distance * cos( angle ), begin[2] + distance * sin(angle) )
  data <- data.frame(bezier(c(begin[1], middle[1], end[1]),  # Generate
                            c(begin[2], middle[2], end[2]),  # X & y
                            evaluation = npoints))
  data
}

bone <- function( begin= c(0,0), distance = 1, angle = pi/2, npoints = 10, ... ) {
  geom_path(mapping=aes(x,y),data = databone(begin, distance, angle, npoints, ...) , ...)
}


data


ggplot() + bone() +head(center=c(0,1),diameter=0.3)

## ============================================================
## cartoon character
## ============================================================

centerofhead <- c(0,0)
scale <- 1
angleofspine <-   - pi / 2 # 
anglerighthumerus <- -pi/6
anglelefthumerus <- pi + pi/6
anglerightradius <- 0
angleleftradius <- - pi/4
angleleftleg <- 3*pi/2  + pi / 12 
anglerightleg <- 3*pi/2  - pi / 12
angleofneck <- runif(1, min = 3 * pi / 2 - pi/10 , max = 3 * pi / 2 + pi/10)




centerofhead <- c(0,0)
scale <- 1
angleofspine <-   - pi / 2 # 
anglerighthumerus <- -pi/6
anglelefthumerus <- pi + pi/6
anglerightradius <- 0
angleleftradius <- - pi/4
angleleftleg <- 3*pi/2  + pi / 12 
anglerightleg <- 3*pi/2  - pi / 12
angleofneck <- runif(1, min = 3 * pi / 2 - pi/10 , max = 3 * pi / 2 + pi/10)

diameterofhead <- 1 * scale
lengthofspine <- diameterofhead * 1
lengthofleg <- lengthofspine * 1.2
lengthofhumerus <- lengthofspine * 0.6
lengthofradius <- lengthofspine * 0.5
beginspine <- centerofhead + (diameterofhead / 2) * c( cos(angleofneck), sin( angleofneck))
endspine <- beginspine + lengthofspine * c( cos( angleofspine), sin(angleofspine))
endrighthumerus <- beginspine + lengthofhumerus * c( cos( anglerighthumerus), sin(anglerighthumerus))
endlefthumerus <- beginspine + lengthofhumerus * c( cos( anglelefthumerus), sin(anglelefthumerus))

p <- ggplot() + #coord_equal() +
 head(center=centerofhead,diameter=diameterofhead) + # head
  bone(begin = beginspine, distance = lengthofspine , angle = angleofspine, factor = 1.2  ) + # spine
  bone(begin = beginspine, distance = lengthofhumerus, angle = anglerighthumerus, factor = 0.20) + # right humerus
  bone(begin = endrighthumerus, distance = lengthofradius, angle = anglerightradius , factor = 0.20) +
  bone(begin = beginspine, distance = lengthofhumerus, angle = anglelefthumerus, factor = 0.20) +
  bone(begin = endlefthumerus, distance = lengthofradius, angle = angleleftradius, factor = 0.20) +
  bone(begin = endspine, distance = lengthofleg, angle = angleleftleg, factor = 0.20) + # Leg
  bone(begin = endspine, distance = lengthofleg, angle = anglerightleg, factor = 0.20) # Leg








cartooncharacterequalscaled <- function( centerofhead = c(0,0),
                             scale = 1,
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
  


ggplot() + geom_smooth(mapping=aes(x,y),data=data) +cartooncharacter() + coord_equal() 




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
