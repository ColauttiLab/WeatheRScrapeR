idw <- function(distance, value) {
  weighting<-1/distance^2
  predict<-(sum(weighting*value)/sum(weighting))
  return(predict)
}

