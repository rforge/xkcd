# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-04-28 Sun 21:28 emilio on emilio-laptop2>
# =====================================================================



dibujobase + segment(begin=c(0,-20), end=c(0,10), npoints = 50, xjitteramount = 0.1515)

volunteers <- data.frame(year=c(2007:2011), number=c(56470, 56998, 59686, 61783, 64251))
xlim <- range(volunteers$year)
ylim <- range(volunteers$number)
xjitteramount <- diff(xlim)/50
yjitteramount <- diff(ylim)/50
ratioxy <-  diff(xlim) / diff(ylim)


set.seed(123)
p <- ggplot() + geom_smooth(mapping=aes(x=year, y =number), data =volunteers,  position = position_jitter(height=0.01),method="loess")
p + themexkcd() + ylab("Volunteers at Cáritas Española") +
  segment(begin=c(xlim[1],ylim[1]),     # xaxis
                   end=c(xlim[2],ylim[1]),
                   xjitteramount=0, yjitteramount=yjitteramount, mask = FALSE) +
  segment(begin=c(xlim[1],ylim[1]), # yaxis
                   end=c(xlim[1],ylim[2]),
                   xjitteramount=xjitteramount, yjitteramount=0, mask = FALSE) +
  coord_cartesian(xlim = xlim + c(-xjitteramount, xjitteramount),
        ylim = ylim +  c(-yjitteramount, yjitteramount)) +
  man(centerofhead = c(2008, 63000), scale =1000, ratioxy = ratioxy) +
  annotate("text", x=2008.7, y = 63700, label = "We Need\nVolunteers!", family="xkcd" ) +
  segment(begin=c(2008.3, 63000), end=c(2008.5,63400), xjitteramount=0.12) +
  man(centerofhead = c(2010, 58850), scale =1000, ratioxy = ratioxy, angleofspine = -pi/2 - pi/8) +
  annotate("text", x=2010.5, y = 60000, label = "Sure\nI can!", family="xkcd" ) +
  segment(begin=c(2010.5, 59600), end=c(2010.3,59000), xjitteramount=0.12)
  



We Need Volunteers!

?position_jitter

require(extrafont)

 ##xkcdFontURL <- "http://simonsoftware.se/other/xkcd.ttf"
  ## download.file(xkcdFontURL,dest="xkcd.ttf")
##font_import(".")   ## because we downloaded to working directory
##font_import(system.file("extdata", "",package = "xcdb"))
loadfonts()

font_import
?system.file

## http://stackoverflow.com/questions/12675147/how-can-we-make-xkcd-style-graphs-in-r

xlim <- c(-3,3)
ylim <- c(-30,30)
xjitteramount <- diff(xlim) / 50
yjitteramount <- diff(ylim) / 50



axis <- function(xlim, ylim, xjitteramount = 0, yjitteramount =0, ...) {
  xaxis <- segment(begin=c(xlim[1],ylim[1]),
                   end=c(xlim[2],ylim[1]),
                   xjitteramount=0, yjitteramount=yjitteramount, ...)
  yaxis <- segment(begin=c(xlim[1],ylim[1]),
                   end=c(xlim[1],ylim[2]),
                   xjitteramount=xjitteramount, yjitteramount=0,  ...) 
  ## coord <-  doCall("coord_cartesian",
  ##                  xlim = xlim + c(-xjitteramount, xjitteramount),
  ##                  ylim = ylim +  c(-yjitteramount, yjitteramount), ...)
return(c( xaxis, yaxis))
}

themexkcd <- function() {
  
  theme(panel.grid.major=element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor=element_blank(),
        text = element_text(size=16, family="xkcd"))
}

k <- ggplot() + axis(xlim = xlim, ylim = ylim, xjitteramount =xjitteramount, yjitteramount = yjitteramount, mask = FALSE) +
  coord_cartesian(xlim = xlim + c(-xjitteramount, xjitteramount),
        ylim = ylim +  c(-yjitteramount, yjitteramount)) +
   themexkcd()
  
##+ coord_cartesian(xlim = xlim, ylim = ylim, wise = NULL)
k


dibujobase +
  man(ratioxy=ratioxy,scale=10) +
  man(center=c(-2,00),ratioxy=ratioxy,scale=10) + axis(xlim = xlim, ylim = ylim, xjitteramount =xjitteramount, yjitteramount = yjitteramount, mask = FALSE) +
  coord_cartesian(xlim = xlim + c(-xjitteramount, xjitteramount),
        ylim = ylim +  c(-yjitteramount, yjitteramount)) +
  theme(panel.grid.major=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(),panel.grid.minor=element_blank())


k


theme(
    panel.background = element_rect(fill="white"),
    axis.ticks = element_line(colour=NA),
    panel.grid = element_line(colour="white"),
    ##axis.text.y = element_text(colour=NA),
                     axis.text.y = element_text(colour="black"),
    axis.text.x = element_text(colour="black"),
    ##text = element_text(size=16, family="Humor Sans")
                     text = element_text(size=16, family="xkcd")
    )

?jitter
class(k)
names(k)
k$data
recta <- k$layers[[2]]
class(recta)
min(k$coordinates)

ggplot_build(k)
jj <- ggplot_build(k)
names(jj)
jj$panel$ranges[[1]]$x.range
jj$panel$ranges[[1]]$y.range
