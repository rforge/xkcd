# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-04-29 Mon 15:08 emilio on emilio-laptop2>
# =====================================================================

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


dibujobase + man(centerofhead=c(0,0), scale=10, ratioxy =ratioxy)



pointshair <- function(centerofhead = c(0,0), scale=1, ratioxy=1, npoints = 24, alpha=  pi/3, ...) {
  require(Hmisc) # bezier
  require(R.utils) # doCall
  center <- centerofhead
  diameter <- scale
  diam <- rep(diameter,npoints)
  pointxforehead <- center[1] + (diam/2 ) * cos(alpha) * ratioxy
  pointyforehead <- center[1] + (diam /2 ) * sin(alpha)
  tt <- seq( (diameter /2), (diameter/6 ),length.out = npoints)
  pointx2ofthehead <- center[1] + tt  * cos(alpha + pi/4) * ratioxy
  pointy2ofthehead <- center[1] + tt  * sin(alpha + pi/4)
  pointxtopofthehead <- center[1] + tt  * cos(alpha + pi/2) * ratioxy
  pointytopofthehead <- center[1] + tt  * sin(alpha + pi/2)
  tt <- seq( alpha + pi - pi/18 , alpha + pi ,length.out = npoints)
  pointxbackofthehead <- center[1] + (diameter/2)*0.85  * cos(tt) * ratioxy
  pointybackofthehead <- center[1] + (diameter/2)*0.85  * sin(tt)
  pointxpigtail <- center[1] + (diameter/2)*1.05  * cos(tt) * ratioxy
  pointypigtail <- center[1] + (diameter/2)*1.05  * sin(tt)
  dataguide <- data.frame(pointxforehead,pointx2ofthehead, pointxtopofthehead, pointxbackofthehead, pointxpigtail,
             pointyforehead, pointy2ofthehead,pointytopofthehead, pointybackofthehead, pointypigtail)
  kk <- apply(dataguide,1, function(x){bezier(x=x[1:5],y=(x[6:10]),evaluation=8)})
  ##jj <- lapply(kk,function(x){data <- data.frame(x=x$x,y=x$y); path(data, mask = FALSE)})
  ##kk <- apply(dataguide,1, function(x){data.frame(x=x[1:5],y=x[6:10])})
  jj <- lapply(kk,function(x){data <- data.frame(x=x$x,y=x$y)
                              ##geom_smooth(mapping=aes(x=x,y=y),data=data, method="loess")
                              path(data,mask=FALSE)
                            })
  jj
}

ggplot() +  pointshair(centerofhead=c(0,0), scale=1, ratioxy =ratioxy)

ggplot() + man(centerofhead=c(0,0), scale=1, ratioxy =ratioxy) + pointshair(centerofhead=c(0,0), scale=1, ratioxy =ratioxy)


dibujobase + man(centerofhead=c(0,0), scale=10, ratioxy =ratioxy) + pointshair(centerofhead=c(0,0), scale=10, ratioxy =ratioxy) 



kk

?apply

?rep
