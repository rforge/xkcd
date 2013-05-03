library(ggplot2)
library(grid)
library(Hmisc) # bezier
library(R.utils) # doCall
datascaled <- data.frame(x=c(-3,3),y=c(-30,30))
dibujobase <- ggplot(data=datascaled, aes(x=x,y=y)) + geom_point()
xrange <- 6
yrange <- 60
ratioxy <- xrange / yrange

dibujobase + annotate("rect", xmin = 2.5, xmax = 2.75, ymin = 12, ymax = 21,
  alpha=0.0, colour = "white",fill="red") +
  annotate("text", x=-2, y = 10, label ="****", colour="white",size=12) +
  geom_rect(mapping=aes(xmin=2,xmax=3,ymin=7,ymax=10), fill="white") + annotate("text", x = 2, y = 8, label = "Some text\naad")

a <- annotate("text", x = 2, y = 10, label = "Some text\naad")
names(a)


