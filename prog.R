library(raster)
library(sp)
library(rgdal)

rzad <- function(ncol, x, y, a){
  tab <- c()
  for(i in 0:(ncol-1)){
    tab = rbind(tab, c((x+(sqrt(3)*a*i)),y))
  }
  return (tab)
}

srodki <- function(nrow, ncol, x, y, a){
    tab <- c()
    for(i in 0:(nrow-1)){
      tempx = x + ((sqrt(3)/2)*a)*(i %% 2)
      tempy = (3/2)*a*i + y
      tab = rbind(tab, rzad(ncol, tempx, tempy, a))
    }
    return(tab)
}


szesciokat <- function(r=1, x=0, y=0){
  k <- c((0:5)*(2*pi/6))
  x <- x+sin(k) * r
  y <- y+cos(k) * r
  (cbind(x,y))  
}


hex <- function (nrow, ncol, x, y, a){
  tab <- list()
  center <- srodki(nrow, ncol, x, y, a)
  for(i in 1:nrow(center)) {
    temphex <- szesciokat(a, center[i,1], center[i,2])
    tab <- append(tab, list(temphex))
  }
  tab
}

plot(spPolygons(hex(15,15,0,0,4)))
