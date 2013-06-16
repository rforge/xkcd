## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-06-16 Sun 21:19 emilio on emilio-laptop2>
## ============================================================

## http://cincodias.com/cincodias/2013/06/12/economia/1371050593_717680.html#?id_externo_display=03072012-cincodias-dsp-pie-001-cds

library(xkcd)
nombresfilas <- read.table(sep=" ", text="
año Alicante Baleares Barcelona Cadiz Gerona Granada LasPalmas Madrid Malaga Tarragona Tenerife Valencia Otras
")
nombrescolumnas <- as.character(t(nombresfilas[1,]))
compradores <- read.table(sep=" ", text="
2008 234 513 74 60 245 10 25 43 792 31 66 10 247
2009 202 485 103 51 176 24 19 65 558 13 39 16 107
2010 226 546 134 29 197 22 26 37 636 11 52 17 93
2011 205 643 166 56 242 22 25 45 674 10 59 10 60
2012 243 729 155 68 213 24 32 41 732 33 68 11 69
")
compradores
colnames(compradores) <- nombrescolumnas
precio <-   read.table(sep=" ", text="
2008 714417 1094974 866739 813501 859413 NA 1085933 1273803 727102 712839 637279 NA 659455
2009 665416 1102774 832431 874942 695767 570307 667624 909084 730174 548003 619293 892132 636841
2010 685149 951745 775375 855569 723474 629467 795179 1009611 816785 646705 669598 814325 743258
2011 671839 1009862 793121 990970 756297 674452 687413 799644 744354 NA 706554 NA 685438
2012 717941 1080361 693578 753738 771773 633005 724301 584433 770168 591207 610577 741019 676768
")
precio
colnames(precio) <- nombrescolumnas
library(reshape2)
preciolargo <- melt(precio,id="año",value.name="precio")
preciolargo
compradoreslargo <- melt(compradores,id="año",value.name="comprador") 
basedatos <- merge(compradoreslargo, preciolargo)


basedatos



ggplot(data=basedatos, aes(x=comprador, y=precio,
                      color=variable)) +
    geom_point() + geom_path() + geom_text(aes(label=año), size=4)+
   ## scale_color_manual(guide=FALSE) +
    xlab('Compradores') +
    ylab('Precio')


yrange <- range(basedatos$precio,na.rm=TRUE)
yrange
basedatos
base1 <- basedatos[basedatos$comprador<400 & basedatos$comprador>25,]
listaciudades <- levels(base1$variable)[table(base1$variable)==5]
listaciudades
base1 <- base1[base1$variable%in%listaciudades,]
base2 <- basedatos[basedatos$comprador>400,]
ejes <- function(xrange,yrange){
 if (is.null(xrange) | is.null(yrange)) 
        stop("Arguments are: xrange, yrange")
    xjitteramount <- diff(xrange)/50
    yjitteramount <- diff(yrange)/50
    dataaxex <- data.frame(xbegin = xrange[1] - xjitteramount, 
        ybegin = yrange[1] - yjitteramount, xend = xrange[2] + 
            xjitteramount, yend = yrange[1] - yjitteramount)
    mappingsegment <- with(dataaxex, aes(xbegin = xbegin, ybegin = ybegin, 
        xend = xend, yend = yend))
    axex <- xkcdline(mappingsegment, dataaxex, yjitteramount = yjitteramount, 
        mask = FALSE)
    dataaxey <- data.frame(xbegin = xrange[1] - xjitteramount, 
        ybegin = yrange[1] - yjitteramount, xend = xrange[1] - 
            xjitteramount, yend = yrange[2] + yjitteramount)
    mappingsegment <- with(dataaxey, aes(xbegin = xbegin, ybegin = ybegin, 
        xend = xend, yend = yend))
    axey <- xkcdline(mappingsegment, dataaxey, xjitteramount = xjitteramount, 
        mask = FALSE)
    coordcarte <- coord_cartesian(xlim = xrange + 1.5 * c(-xjitteramount, 
        xjitteramount), ylim = yrange + 1.5 * c(-yjitteramount, 
        yjitteramount))
    list(c(axex), coordcarte)##, theme_xkcd())
 }
## eje x base 2
kk <- c(6,7,8,9,10,11,12)*100000
set.seed(130616)
dib1 <- ggplot() +
    geom_point(data=base1, aes(x=comprador, y=precio, color=variable),alpha=0.6) +
  geom_path(data=base1, aes(x=comprador, y=precio,  color=variable),alpha=0.6) +
  geom_text(data=base1, aes(x=comprador, y=precio, color=variable, label=año), alpha=0.6,size=3,hjust=0, vjust=0,family="xkcd") +
   geom_text(aes(x=comprador,y=precio,label=variable,color=variable),data=base1[base1$año==2012,],hjust=1, vjust=1,family="xkcd",size=6 )+
    xlab('                                      Viviendas vendidas') +
    ylab('Miles de euros') +
  xkcdaxis(c(12,255),yrange) +
  theme(
     ## line =               element_blank(),
     ## rect =               element_blank(),
     ## ##text =               element_blank(),
     ## axis.ticks.length =  unit(0, "cm"),
     ## axis.ticks.margin =  unit(0.01, "cm"), # change back to 0 when grid is fixed
                           panel.grid.major = element_blank(), axis.ticks = element_line(colour = "black"), 
            panel.background = element_blank(), panel.grid.minor = element_blank(), 
            legend.key = element_blank(), strip.background = element_blank(), 
            text = element_text(size = 16, family = "xkcd"),
                           legend.position =    "none",
                           axis.text.y  = element_text(angle=90, vjust=0.5,hjust=0.5)) +
  scale_y_continuous(breaks=kk,   labels=kk/1000)
##
dib2 <-  ggplot() +
    geom_point(data=base2, aes(x=comprador, y=precio, color=variable),alpha=0.6) +
  geom_path(data=base2, aes(x=comprador, y=precio,  color=variable),alpha=0.6) +
  geom_text(data=base2, aes(x=comprador, y=precio, color=variable, label=año), size=3,alpha=0.6,hjust=0, vjust=0,family="xkcd") +
   geom_text(aes(x=comprador,y=precio,label=variable,color=variable),data=base2[base2$año==2012,],hjust=1, vjust=1,family="xkcd",size=6 )+
    xlab(' ') + ejes(range(base2$comprador),yrange) +
   theme(
     ## line =               element_blank(),
     ## rect =               element_blank(),
     ## ##text =               element_blank(),
     ## axis.ticks.length =  unit(0, "cm"),
     ## axis.ticks.margin =  unit(0.01, "cm"), # change back to 0 when grid is fixed
                           panel.grid.major = element_blank(), axis.ticks = element_line(colour = "black"), 
            panel.background = element_blank(), panel.grid.minor = element_blank(), 
            legend.key = element_blank(), strip.background = element_blank(), 
            text = element_text(size = 16, family = "xkcd"),
                           legend.position =    "none",axis.text.y = element_blank(),
           axis.title.y = element_blank(),
         axis.ticks.y = element_blank(), 
                           axis.text.y  = element_blank())
## grid.newpage()
## pushViewport( viewport( layout = grid.layout(1,2, widths = c(.7,.3))))
## vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
## print(dib1, vp = vplayout(1,1))
## print(dib2,   vp = vplayout(1,2))
library(gridExtra)
p <- arrangeGrob(dib1, dib2,widths=c(0.7,0.3),ncol=2)
grid.draw(p) # interactive device

ggsave("GrEvolucionVentaViviendas.png", p) # need to specify what to save explicitely


## pdf(file="GrEvolucionVentaViviendas.pdf")
## grid.newpage()
## pushViewport( viewport( layout = grid.layout(1,2, widths = c(.7,.3))))
## vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
## print(dib1, vp = vplayout(1,1))
## print(dib2,   vp = vplayout(1,2))
## dev.off()

## library(grDevices)
## dir()
## embedFonts("GrEvolucionVentaViviendas.pdf")



?arrangeGrob
