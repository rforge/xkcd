## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-05-03 Fri 00:04 emilio on emilio-laptop2>
## ============================================================


library(xkcd)


mapping <- aes(j=t1,t2,x=z,z4,ll=jh,aa)

unlist(mapping)
names(mapping)

mandatoryarguments <- c("x","y","a","b")

nm <- names(mapping)
positionswithoutname <- (1:length(nm))[nm==""]
positionswithoutname
failsthisarguments <- mandatoryarguments[ !(mandatoryarguments %in% nm) ]
if(length(failsthisarguments) != length(positionswithoutname))
  stop(paste("Argumenst of aes are ", paste(mandatoryarguments, collapse=", "),".",sep=""))

names(mapping)[positionswithoutname] <- failsthisarguments
mapping
positionswithoutname
failsthisarguments

for( i in mandatoryarguments ) {
  if(! (i %in% nm )) {
  }
}
