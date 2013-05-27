## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-05-26 Sun 11:52 emilio on emilio-laptop2>
## ============================================================


library(xkcd)
library(reshape)

mydata <- read.table(header=TRUE,sep=",",text="
year,ministerio,banco,fmi,homo
2013,2,1.95,1.96,1.94
2014,2.1,1.97,1.93,1.88
2015,2.2,2.05,1.90,1.87
")
mydata

mydatalong <- melt(mydata, id="year", measure.vars= names(mydata)[-1])
mydatalong

xrange <- c(2013,2015)
yrange <- c(1.86,2.21)
set.seed(123)
##p <- ggplot() + geom_smooth(aes(x=year, y=value, group=variable,linetype=variable), data=mydatalong, position = position_jitter(h=0.0001),color="black") + theme(legend.position = "none") + xkcdaxis(xrange,yrange)
p <- ggplot() + geom_smooth(aes(x=year, y=value, group=variable,linetype=variable), data=mydatalong,color="black") + theme(legend.position = "none") + xkcdaxis(xrange,yrange)
p2 <- p + ylab("Change in real GDP (%)") + xlab("Economic Projections of several Institutes") + scale_x_continuous(breaks=c(mydata$year),   labels=c(mydata$year))
datalabel <- data.frame(x=2014.95,
                        y=t(mydata[mydata$year==2015,c(2,3,4,5)]),
                        label=c("Ministry of Economy","National Bank","International Monetary Fund","Homo Sapiens Sapiens*"))
names(datalabel) <- c("x","y","label")

p3 <- p2 + geom_text(aes(x=x,y=y,label=label), data=datalabel, hjust=1, vjust=1,family="xkcd",size=7) +
  annotate("text", x=2013.4, y=1.852, label="*Homo Sapiens Sapiens = Doubly Wise Man",family="xkcd",size=3.5)


p3

ggsave("GrHomoSapiens.png")

mydata[,c(2,3,4,5)]
