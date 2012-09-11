# function for plotting each frame of the animated maps

library(maps)
library(maptools)
library(RColorBrewer)

addColorColumn <- function(column) {
  column <- as.factor(column)
  num.colors <- length(levels(column))
  if(num.colors > 11) {
    pal <- rainbow(num.colors)    
  }
  else {
    pal <- brewer.pal(num.colors,"RdYlGn")
  }
  
  levels(column) <- pal
  return(column)
}

