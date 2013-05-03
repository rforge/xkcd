library(ggplot2)
library(grid)
library(Hmisc) # bezier
library(R.utils) # doCall
datascaled <- data.frame(x=c(-3,3),y=c(-30,30))
dibujobase <- ggplot(data=datascaled, aes(x=x,y=y)) + geom_point()
xrange <- 6
yrange <- 60
ratioxy <- xrange / yrange

ratioxy



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
  data <- datasegment(begin=begin, end=end, ...)
  path(data,...)
}



dibujobase + segment(begin=c(0,0), end=c(0,10)) + segment(begin=c(0,0), end=c(0,-10),npoints=50,factor=10, colour="red") + segment(begin=c(-2,5), end=c(2,5), npoints=30,factor=10, size =4) 



circle <- function(center = c(0,0), diameter = 1, ... ) {
  datacircle <- function(center = c(0,0), diameter = 1, ratioxy=1, npoints = 16, alpha=  runif(1, 0, pi/2), ...){
    require(Hmisc) # bezier
    require(R.utils) # doCall
    r <- rep( diameter / 2, npoints )
    tt <- seq(alpha,2*pi + alpha,length.out = npoints)
    r <- doCall("jitter", x=r, ...)
    sector <-  tt > alpha & tt <= ( pi/ 2 + alpha)
    r[ sector ] <- r[sector] * 1.05
    sector <-  tt > ( 2 * pi/2 + alpha)  & tt < (3* pi/ 2 +alpha)
    r[ sector ] <- r[sector] * 0.95    
    xx <- center[1] + r * cos(tt) * ratioxy
    yy <- center[2] + r * sin(tt) 
    ##return(data.frame(x = xx, y = yy))
    return(data.frame(doCall("bezier",x = xx, y =yy, ...)))
  }
  data <- datacircle(center = center, diameter = diameter, ...)
  path(data = data, ...)
}


dibujobase + circle(diameter = 5, ratioxy=ratioxy)


## ============================================================
## man
## ============================================================



man <- function(centerofhead = c(0,0),
                             scale = 1,
                                        ratioxy =1,
                             angleofspine =   - pi / 2 ,
                             anglerighthumerus = -pi/6,
                             anglelefthumerus = pi + pi/6,
                             anglerightradius = 0,
                             angleleftradius = - pi/4,
                             angleleftleg = 3*pi/2  + pi / 12 ,
                             anglerightleg = 3*pi/2  - pi / 12,
                             angleofneck = runif(1, min = 3 * pi / 2 - pi/10 , max = 3 * pi / 2 + pi/10),
                ...) {
bone <- function( begin= c(0,0), distance = 1, angle = pi/2, ratioxy = 1, ... ) {
  end <- c( begin[1] + distance * cos( angle ) * ratioxy, begin[2] + distance * sin(angle) )
  segment(begin, end, ...)
}
diameterofhead <- 1 * scale
lengthofspine <- diameterofhead * 1
lengthofleg <- lengthofspine * 1.2
lengthofhumerus <- lengthofspine * 0.6
lengthofradius <- lengthofspine * 0.5
beginspine <- centerofhead + (diameterofhead / 2) * c( cos(angleofneck) * ratioxy, sin( angleofneck))
endspine <- beginspine + lengthofspine * c( cos( angleofspine) * ratioxy , sin(angleofspine))
endrighthumerus <- beginspine + lengthofhumerus * c( cos( anglerighthumerus) *ratioxy, sin(anglerighthumerus))
endlefthumerus <- beginspine + lengthofhumerus * c( cos( anglelefthumerus)*ratioxy, sin(anglelefthumerus))

return(c(circle(center=centerofhead,diameter=diameterofhead, ratioxy = ratioxy, ...), # head
  bone(begin = beginspine, distance = lengthofspine , angle = angleofspine, ratioxy = ratioxy, ...  ), # spine
  bone(begin = beginspine, distance = lengthofhumerus, angle = anglerighthumerus, ratioxy = ratioxy, ...) , # right humerus
  bone(begin = endrighthumerus, distance = lengthofradius, angle = anglerightradius , ratioxy = ratioxy, ...),
  bone(begin = beginspine, distance = lengthofhumerus, angle = anglelefthumerus,ratioxy = ratioxy, ...),
  bone(begin = endlefthumerus, distance = lengthofradius, angle = angleleftradius, ratioxy = ratioxy, ...),
  bone(begin = endspine, distance = lengthofleg, angle = angleleftleg, ratioxy = ratioxy, ...), # Leg
  bone(begin = endspine, distance = lengthofleg, angle = anglerightleg, ratioxy= ratioxy,  ...)) )#Leg
}


dibujobase + man(ratioxy=ratioxy,scale=10) +
  man(center=c(-2,00),ratioxy=ratioxy,scale=10) +
  man(center=c(2,20),ratioxy=ratioxy,scale=10) +
  man(center=c(-2,20),ratioxy=ratioxy,scale=10)
