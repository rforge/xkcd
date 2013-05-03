# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-04-29 Mon 15:08 emilio on emilio-laptop2>
# =====================================================================

datatourists <- data.frame(x=c(2007:2012),y=c(58.6,57.2, 52.2, 52.6,56.1, 57.7))
xlim<- range(datatourists$x) -c(0.3, -0.3)
ylim <- range(datatourists$y) -c(0.1,0)
ratioxy <- diff(xlim) / diff(ylim)
xjitteramount <- diff(xlim) / 50
yjitteramount <- diff(ylim) / 50
ratioxy


boys <- read.table(sep=";",text="
Alain Gonzalez
David Martin
Ibon Aranburu
Ion Zugasti
Mikel Zurutuza
Ibai Roman
Igor Ibarguren
Ion Gil
Ortzi Torices
Hugo Salas
Inigo Castander
Fidel Rebon
Joana Vidal
")

girls <- read.table(sep=";",text="
Maider Anso
Itziar Rincon
Izaskun Arenaza
Elisabet Ormazabal
Sofia Reino
Maria Peralta
Nagore Espinosa
Ariane Rubio
Joana Vidal
")


plotworkers <- function(x,y,...) {
  p <- NULL
  for( i in 1:length(x) ) {
    p <- c(p,
            man(centerofhead=c(x[i],y[i]),
                angleofspine = runif(1, - pi/2 - pi/3, - pi/3 ),
                anglerighthumerus = runif(1, -pi/6- pi/10, - pi/6 + pi/10),
                anglelefthumerus = runif(1, pi + pi/6 -pi/10, pi + pi/6 + pi/10),
                anglerightradius =  runif(1, -pi/4, pi/4),
                angleleftradius =  runif(1, pi -pi/4, pi + pi/4),
                angleleftleg = runif(1, 3* pi/2 + pi/12 - pi/180, 3* pi/2 + pi/12 - pi/180 ),
                anglerightleg = runif(1,  3* pi/2 - pi/12 - pi/180, 3* pi/2 - pi/12 + pi/180),
                ... )
## man(centerofhead=c(x[i],y[i]),...)
             )
  }
  p   
}

workers <- c(as.character(boys$V1),as.character(girls$V1),"Jon Kepa Gerrikagoitia","Carlos Lamsfus")
positionx <- runif(length(workers), 2007,2012)

set.seed(123)
positiony <- seq(52.5,58.6,length.out = length(workers))
p <- ggplot() +  themexkcd() + ylab("International Tourist Arrivals") +
  xlab("It took mommy and daddy five years to raise a large family") +
  bar(x=datatourists$x, y = datatourists$y, ymin = ylim[1], xwidth = 0.3, color="white", alpha=0.2, fill="blue", xjitteramount=0.1) +
  segment(begin=c(xlim[1],ylim[1]),     # xaxis
                   end=c(xlim[2],ylim[1]),
                   xjitteramount=0, yjitteramount=yjitteramount, mask = FALSE) +
  segment(begin=c(xlim[1],ylim[1]), # yaxis
                   end=c(xlim[1],ylim[2]),
                   xjitteramount=xjitteramount, yjitteramount=0, mask = FALSE) +
  coord_cartesian(xlim = xlim + c(-xjitteramount, xjitteramount),
        ylim = ylim +  c(-yjitteramount, yjitteramount)) +
   scale_x_continuous(breaks=c(2007:2012))
p



p2 <- p + man(centerofhead=c(2008.6,55),scale=1, ratioxy=ratioxy, angleofspine = runif(1, -pi/2 - pi/18, -pi/2 + pi/18 ), anglerighthumerus = pi/6, anglelefthumerus = pi/2 + pi/6, anglerightradius =  runif(1, pi/2 - pi/18, pi/2 + pi/18 ), angleleftradius =  runif(1, pi/2 - pi/18, pi/2 + pi/18 ))  +
   man(centerofhead=c(2010,55),scale=1, ratioxy=ratioxy, angleofspine = runif(1, -pi/2 - pi/18, -pi/2 + pi/18 ), anglerighthumerus = pi/6, anglelefthumerus = pi/2 + pi/6, anglerightradius =  runif(1, pi/2 - pi/18, pi/2 + pi/18 ), angleleftradius =  runif(1, pi/2 - pi/18, pi/2 + pi/18 )) +
   plotworkers(x=positionx, y = positiony, scale=0.2, ratioxy=ratioxy)

pdf("cic5ann.pdf")
print(p2)
dev.off()

p3 <- p2  +  annotate("text", x= positionx, y= positiony, label=workers, family="xkcd",size=3, alpha =0.7) + annotate(geom="text", x = 2009.5, y= 56.2, label="5th Anniversary of\nCICtourGUNE", family="xkcd",size=18, alpha=0.4, colour="red") +    annotate(geom="text", x = 2008.6, y= 55, label="Aurkene Alzua-Sorzabal Mommy", family="xkcd",size=3.5 , alpha =0.8) + 
annotate(geom="text", x = 2010, y= 55, label="Garikoitz Agote Daddy", family="xkcd",size=3.5, alpha =0.8 )

p3

pdf("cic5anniversary.pdf")
print(p3)
dev.off()


embed_fonts("cic5anniversary.pdf")
