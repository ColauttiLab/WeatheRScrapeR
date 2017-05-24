idw <- function(distance, value) {
  weighting<-1/distance^2
  predict<-(sum(weighting*value, na.rm=T)/sum(weighting[value>=0], na.rm=TRUE))
  return(predict)
}
