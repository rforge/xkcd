library(ggplot2)
library(grid)
library(Hmisc) # bezier
library(R.utils) # doCall
require(extrafont)
 ##xkcdFontURL <- "http://simonsoftware.se/other/xkcd.ttf"
  ## download.file(xkcdFontURL,dest="xkcd.ttf")
##font_import(".")   ## because we downloaded to working directory
##font_import(system.file("extdata", "",package = "xcdb"))
loadfonts()
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


jitteramount <- function(x, amount = NULL) {
  if(is.null(amount)) return(x)
  else return(jitter(x, amount = amount))
}



segment <- function(begin= c(0,0), end=c(1,1), ... ) {
 datasegment <- function( begin= c(0,0), end=c(1,1), npoints = 10, bezier= TRUE, xjitteramount = NULL, yjitteramount = NULL, ... ) {
    require(Hmisc) # bezier
    require(R.utils) # doCall
    x <- seq(begin[1],end[1],length.out = npoints)
    if( (end[1] - begin[1]) != 0 ) {
      y <- (end[2] - begin[2]) * ( x - begin[1] ) / (end[1] - begin[1]) + begin[2]
    } else {
      y <-  seq(begin[2], end[2], length.out = npoints)
    }
    xx <- jitteramount(x=x, amount= xjitteramount)
    yy <- jitteramount(x=y, amount= yjitteramount)
    xx[1] <- begin[1]
    yy[1] <- begin[2]
    xx[length(xx)] <-end[1]
    yy[length(yy)] <-end[2]
    if(bezier) data <- data.frame(doCall("bezier", x=xx, y=yy, ...))
    else   data <- data.frame(x=xx,y=yy)
    data
  }
  data <- datasegment(begin=begin, end=end, ...)
  path(data=data,...)
}



## segment<- function(begin= c(0,0), end=c(1,1), ... ) {
##   if(length(begin) == 2 & length( end) ==2 )
##     return(segmentone( begin, end, ...))
##   else if (length(begin) > 2 & length(begin) == length(end) & !(length(begin)%%2) ) {
##     numberofpoints <- length(begin)/2
##     return( c(segmentone(begin = c(begin[1], begin[numberofpoints +1]), end= c(end[1], end[numberofpoints +1])), segment(begin=begin[c(-1,-(numberofpoints +1))], end=end[c(-1,-(numberofpoints +1))] )))
##   } 
##   }


dibujobase + segment(begin=c(0,0), end=c(0,10)) + segment(begin=c(0,0), end=c(0,-10),npoints=50,factor=10, colour="red") + segment(begin=c(-2,5), end=c(2,5), npoints=30,yjitteramount=1, size =4) 



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


dibujobase +
  man(ratioxy=ratioxy,scale=10) +
  man(center=c(-2,00),ratioxy=ratioxy,scale=10) +
  man(center=c(2,20),ratioxy=ratioxy,scale=10) +
  man(center=c(-2,20),ratioxy=ratioxy,scale=7,xjitteramount=0.05,yjitteramount=0.15) +
man(center=c(0,20),ratioxy=ratioxy,scale=7,angleofneck = 3 * pi/2 + pi /4, anglerighthumerus =  pi /4,  anglerightradius =  pi /3, angleleftleg= pi, anglerightleg= pi + pi/12, xjitteramount=0.05,yjitteramount=0.15) 
  

axis <- function(xlim, ylim, xjitteramount = 0, yjitteramount =0, ...) {
  xaxis <- segment(begin=c(xlim[1],ylim[1]),
                   end=c(xlim[2],ylim[1]),
                   xjitteramount=0, yjitteramount=yjitteramount, ...)
  yaxis <- segment(begin=c(xlim[1],ylim[1]),
                   end=c(xlim[1],ylim[2]),
                   xjitteramount=xjitteramount, yjitteramount=0,  ...)
return(c( xaxis, yaxis))
}

themexkcd <- function() {
  theme(panel.grid.major=element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor=element_blank(),
        text = element_text(size=16, family="xkcd"))
}


bar <- function(x,y,ymin,xwidth, ...) {
  xmin <- x-xwidth/2 #- (required) left edge of rectangle 
  xmax <-  x + xwidth/2  #(required) right edge of rectangle 
  ymin <- ymin #- (required) bottom edge of rectangle 
  ymax <- y # (required) top edge of rectangle
  data <- data.frame(xmin = xmin, xmax= xmax, ymin = ymin, ymax =ymax )
  p <- geom_rect(mapping=aes(xmin = xmin, xmax= xmax, ymin = ymin, ymax =ymax), data = data,...)
  ## AGGGG!!
  for( i in 1:length(x) ) {
    p <- c(p,
           segment(begin=c(data$xmin[i],data$ymin[i]), end=c(data$xmin[i],data$ymax[i]), ...),
           segment(begin=c(data$xmax[i],data$ymin[i]), end=c(data$xmax[i],data$ymax[i]), ...)
           )
  }
  p   
}
