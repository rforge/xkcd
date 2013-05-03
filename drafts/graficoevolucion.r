# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-04-28 Sun 18:34 emilio on emilio-laptop2>
# =====================================================================

## http://stackoverflow.com/questions/12675147/how-can-we-make-xkcd-style-graphs-in-r
 ##xkcdFontURL <- "http://simonsoftware.se/other/xkcd.ttf"
  ## download.file(xkcdFontURL,dest="xkcd.ttf")
##install.packages("extrafont")
  library(extrafont)

##The most useful information about fonts was on the extrafont github site -- this is taken from there
##font_import(".")   ## because we downloaded to working directory
loadfonts()





library(ggplot2)
library(grid) # arrow


## ============================================================
## Datos
## ============================================================
lines <- "
ano,euros
2011,10000
2012,13000
2013,10000
2014,15000
"
con <- textConnection(lines)
data1 <- read.csv(con)
close(con)
data1

lines <- "
ano,euros
2011.0,-100
2011,300
2011,15100
"
con <- textConnection(lines)
data2 <- read.csv(con)
close(con)
data2

n <- 40
mineuros <- -100
maxeuros <-  15000 + 100
y <- -500 + maxeuros * ( c(1:n)) / (n-2)
kk <- (rnorm(n, 2010.9,0.015))
data2 <- data.frame(ano=kk, euros=y)
data2


dataejex <- data.frame(ano=c(2010.85,2011, 2012, 2013, 2014,2014.3), euros = 0)

n <- 40
mineuros <- -100
maxeuros <-  7000
y <- -100 + maxeuros * ( c(1:n)) / (n)
kk <- (rnorm(n, 2013,0.005))
datalinea1 <- data.frame(ano=kk, euros=y)
datalinea1

n <- 40
mineuros <- 10000
maxeuros <-  15000
y <- mineuros + (maxeuros -mineuros) * ( c(1:n)) / (n)
kk <- (rnorm(n, 2014,0.005))
datalinea2 <- data.frame(ano=kk, euros=y)
datalinea2 <- datalinea2[datalinea2$euros>7000,]
datalinea2

n <- 40
mineuros <- 7000
maxeuros <-  10000
y <- mineuros + (maxeuros -mineuros) * ( c(1:n)) / (n)
kk <- (rnorm(n, 2013,0.005))
datalinea3 <- data.frame(ano=kk, euros=y)
datalinea3 <- datalinea3[datalinea3$euros>7000 & datalinea3$euros<10000,]
datalinea3

## ============================================================
## Grafico
## ============================================================

theme_xkcd <- theme(
    panel.background = element_rect(fill="white"),
    axis.ticks = element_line(colour=NA),
    panel.grid = element_line(colour="white"),
    ##axis.text.y = element_text(colour=NA),
                     axis.text.y = element_text(colour="black"),
    axis.text.x = element_text(colour="black"),
    ##text = element_text(size=16, family="Humor Sans")
                     text = element_text(size=16, family="xkcd")
    )

p <- ggplot() + geom_smooth(data=data1, aes(x=ano, y=euros), position = position_jitter(h=0.51)) +
  geom_path(mapping=aes(x=ano, y=euros), data = data2) +
  geom_smooth(aes(x=ano,y=euros), data= dataejex, position = position_jitter(w=0,h = 50), colour="black") +
  ## Otra linea
  geom_path(mapping=aes(x=ano, y=euros), data = datalinea2, type="dotted", colour="red") +
  geom_segment(aes(x = 2014, y = 14920, xend = 2014, yend = 14955), arrow = arrow(length = unit(0.25, "cm")), colour = "red" ) +
  geom_segment(aes(x = 2014, y = 10050, xend = 2014, yend = 10025), arrow = arrow(length = unit(0.25, "cm")), colour = "red" ) +
 ## geom_text(data=pleb.clegg[22, ], family="Humor Sans", aes(x=Date), colour="dark blue", y=4, label="Searches for pleb")+
  geom_text(data=data.frame(ano=2014, euros =12300), aes(x=ano, y=euros), colour="white", label="I",size=18) +
  geom_text(data=data.frame(ano=2014, euros =12300), aes(x=ano, y=euros), colour="dark blue", label="¿SESPA?",size=10,family="xkcd") +
  ## Otra linea
   geom_path(mapping=aes(x=ano, y=euros), data = datalinea1, type="dotted", colour="green") +
  geom_segment(aes(x = 2013, y = 6920, xend = 2013, yend = 6965), arrow = arrow(length = unit(0.25, "cm")), colour = "green" ) +
  geom_segment(aes(x = 2013, y = 50, xend = 2013, yend = 25), arrow = arrow(length = unit(0.25, "cm")), colour = "green" ) +
 ## geom_text(data=pleb.clegg[22, ], family="Humor Sans", aes(x=Date), colour="dark blue", y=4, label="Searches for pleb")+
  geom_text(data=data.frame(ano=2013, euros =5000), aes(x=ano, y=euros), colour="white", label="I",size=18) +
  geom_text(data=data.frame(ano=2013, euros =5000), aes(x=ano, y=euros), colour="dark blue", label="1º Semestre",size=10,family="xkcd") +
  ##### Otra linea
  geom_path(mapping=aes(x=ano, y=euros), data = datalinea3, type="dotted", colour="red") +
  geom_segment(aes(x = 2013, y = 7050, xend = 2013, yend = 7025), arrow = arrow(length = unit(0.25, "cm")), colour = "red" ) +
  geom_segment(aes(x = 2013, y = 9950, xend = 2013, yend = 9975), arrow = arrow(length = unit(0.25, "cm")), colour = "red" ) +
 ## geom_text(data=pleb.clegg[22, ], family="Humor Sans", aes(x=Date), colour="dark blue", y=4, label="Searches for pleb")+
  geom_text(data=data.frame(ano=2013, euros =8600), aes(x=ano, y=euros), colour="white", label="I",size=16) +
  geom_text(data=data.frame(ano=2013, euros =8600), aes(x=ano, y=euros), colour="dark blue", label="¿2º semestre?",size=10, family="xkcd") +
  theme_xkcd +
  xlab("Año")
p

png("GrEvolucion.png")
print(p)
dev.off()





p <- ggplot() + geom_smooth(data=data1, aes(x=ano, y=euros), position = position_jitter(h=0.51))
p + segment(begin=c(2010.9,0), end=c(2014,31), factor = 500)


## No funcionan los acentos
ggsave("GrEvolucion.pdf", plot=p,  width=4, height=4)
## needed for Windows:
##   Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.05/bin/gswin32c.exe")
embed_fonts("GrEvolucion.pdf")
