# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-04-29 Mon 15:21 emilio on emilio-laptop2>
# =====================================================================



##dibujobase + segment(begin=c(0,-20), end=c(0,10), npoints = 50, xjitteramount = 0.1515)

# Number of volunteers at 
volunteers <- data.frame(year=c(2007:2011), number=c(56470, 56998, 59686, 61783, 64251))
xlim <- range(volunteers$year)
ylim <- range(volunteers$number)
xjitteramount <- diff(xlim)/50
yjitteramount <- diff(ylim)/50
ratioxy <-  diff(xlim) / diff(ylim)


set.seed(123)
p <- ggplot() + geom_smooth(mapping=aes(x=year, y =number), data =volunteers,  position = position_jitter(height=0.01),method="loess")
p1 <- p + themexkcd() + ylab("Volunteers at Spanish Caritas") +
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
  

png("caritas.png")
print(p1)
dev.off()
