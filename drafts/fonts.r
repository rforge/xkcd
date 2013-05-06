# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-06 Mon 09:16 emilio on emilio-despacho>
# =====================================================================


## remove.packages("extrafont")
remove.packages("extrafontdb")
## ## install.packages("ggplot2")
## ##
install.packages("extrafont")


## install.packages("ggplot2")

## installed.packages()
## packs

require(extrafont)


## xkcdFontURL <- "http://simonsoftware.se/other/xkcd.ttf"
## download.file(xkcdFontURL,dest="xkcd.ttf")
## font_import(".")   ## because we downloaded to working directory
## ##font_import(system.file("extdata", "",package = "xkcd"))

dir()

loadfonts()

?loadfonts
fonts()


library(ggplot2)
p <- qplot(mpg, wt, data=mtcars) + theme(text = element_text(size=16, family="xkcd"))
p

+ annotate("text", x=20,y=4,label="Hello", family="xkcd")

quit()

dir()

library(ggplot2)
library(extrafont)

# Download XKCD-style font

download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf")
system("mkdir ~/.fonts")
system("cp xkcd.tff -t ~/.fonts")
library(extrafont)
font_import()


http://www.r-bloggers.com/change-fonts-in-ggplot2-and-create-xkcd-style-graphs/


getwd()
# Import the font and load fonts

library(extrafont)
fonts()
font_import(paths="~/.fonts/",recursive=FALSE, prompt=FALSE,pattern="xkcd")

font_import()
fonts()

remove.packages(c("extrafont","extrafontdb"))
install.packages(c("extrafont"))

?font_import

loadfonts()
