# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-02 Thu 20:19 emilio on emilio-laptop2>
# =====================================================================

## fibosworld 2013. Change fonts in ggplot2, and create xkcd style graphs \url{http://fibosworld.wordpress.com/2013/02/17/change-fonts-in-ggplot2-and-create-xkcd-style-graphs/}

theme_xkcd <- function(){
  theme(panel.grid.major = element_blank(),
        ##axis.ticks = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        text = element_text(size = 16, family = "xkcd"))
}
