# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-04-30 Tue 21:46 emilio on emilio-laptop2>
# =====================================================================


path <- function(mapping, data, mask = TRUE, ...) {
  if(mask) {
     argList<-list(...);
	 if(is.null(argList$size)==TRUE){
       mymask <- geom_path(mapping=aes(x,y),data = data , colour = "white", size = 3, ...)
     } else {
       if(argList$size < 3 ) size <- 3
       else size <- argList$size *2
       mymask <- geom_path(mapping=aes(x,y),data = data , colour = "white", size = size, ...)
     }    
    return(c(mymask,geom_path(mapping=aes(x,y),data = data , ...)))
  }
  else {
  return(geom_path(mapping=aes(x,y), data = data , ...))
  }
}



gp <- function(size=2, ...) {
print(size)
}

madre <- function(mask = FALSE, ...) {
  args <- list(...)
  print(args)
  if(is.null(args$size)==TRUE){
    args$size <- 3
  } else {
    if(args$size < 3 ) args$size <- 3
    else args$size <- args$size *1.5
  }
  
  do.call("gp",args)

}

madre(size=4)
