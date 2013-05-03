## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-05-03 Fri 12:58 emilio on emilio-laptop2>
## ============================================================



volunteers <- data.frame(year=c(2007:2011), number=c(56470, 56998, 59686, 61783, 64251))
xrange <- range(volunteers$year)
yrange <- range(volunteers$number)
xjitteramount <- diff(xrange)/50
yjitteramount <- diff(yrange)/50
ratioxy <-  diff(xrange) / diff(yrange)

p <- ggplot() + geom_rect(aes(xmin = year, xmax= year +0.3, ymin=number, ymax = number + 3600),volunteers)
p


mapping <- aes(xmin = year, xmax= year +0.3, ymin=number, ymax = number + 3600)
kk <- createdefaultmappinganddata(mapping, volunteers, c("xmin","ymin","xmax","ymax"))
mapping <- kk$mapping
data <- kk$data
mapping


  upperline <- xkcdline(mappingjoin(aes(x=xmin,y=ymax, xend=xmax, yend=ymax), mapping), data, colour="white", xjitteramount=xjitteramount/2)
#
rightline <-  xkcdline(mappingjoin(aes(x=xmax,y=ymin, xend=xmax, yend=ymax), mapping), data, colour="white",xjitteramount = xjitteramount/2) 
leftline <-  xkcdline(mappingjoin(aes(x=xmin,y=ymin, xend=xmin, yend=ymax), mapping), data, colour="white", xjitteramount=xjitteramount/2)
bottomline <-  xkcdline(mappingjoin(aes(x=xmin,y=ymin, xend=xmax, yend=ymin),mapping), data, colour="white", xjitteramount=xjitteramount/2)

p +upperline +rightline + leftline + bottomline


xkcdrect <- function(mapping, data, ...) {

  requiredaesthetics <-  c("xmin","ymin",
                           "xmax","ymax")

  rect1 <-  geom_rect(mapping, data, ...)

  defaultmapdat <- createdefaultmappinganddata(mapping, data, requiredaesthetics) 
  data <-defaultmapdat$data
  mapping <- defaultmapdat$mapping
  

  xrange <- range(min(data$xmin, data$xmax), max(data$xmin, data$xmax))
yrange <- range(min(data$ymin, data$ymax), max(data$ymin, data$ymax))
xjitteramount <- diff(xrange)/100
yjitteramount <- diff(yrange)/100

  argList <- list(...)
  if( "colour" %in% names(argList))
  
   upperline <- xkcdline(mappingjoin(aes(x=xmin,y=ymax, xend=xmax, yend=ymax), mapping), data, colour="white", yjitteramount=yjitteramount, mask = FALSE, size=3)
#
rightline <-  xkcdline(mappingjoin(aes(x=xmax,y=ymin, xend=xmax, yend=ymax), mapping), data, colour="white",xjitteramount = xjitteramount, mask = FALSE, size=3) 
leftline <-  xkcdline(mappingjoin(aes(x=xmin,y=ymin, xend=xmin, yend=ymax), mapping), data, colour="white", xjitteramount=xjitteramount, mask = FALSE, size =3)
bottomline <-  xkcdline(mappingjoin(aes(x=xmin,y=ymin, xend=xmax, yend=ymin),mapping), data, colour="white", yjitteramount=yjitteramount, mask = FALSE, size=3)

  list(rect1, upperline , rightline, leftline, bottomline )
}


p <- ggplot() + xkcdrect(aes(xmin = year, xmax= year +0.3, ymin=number, ymax = number + 3600),volunteers, fill="red", colour="black")
p


q <- p <- ggplot() + geom_rect(aes(xmin = year, xmax= year +0.3, ymin=number, ymax = number + 3600),volunteers, fill="red", colour="green",size = 5)
q

xkcdrect

data
calculatecorners <- function(xmin, xmax, ymin, ymax ){
  ## upper line
  upperline <- xkcd(aes(x=xmin))
    data <- data.frame(x=xmin, y)
  list(c(xmin,ymin))
}

with(volunteers, xmin = year, xmax= year +0.3, ymin=number, ymax = number + 300

)


volunteers <- data.frame(year=c(2007:2011),
                         number=c(56470, 56998,59686, 61783, 64251))
p <- ggplot() + xkcdrect(aes(xmin = year,
                             xmax= year +0.3,
                             ymin=number,
                             ymax = number + 3600),
                         volunteers,
                         fill="red", colour="black")
p
