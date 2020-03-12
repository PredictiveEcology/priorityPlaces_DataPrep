rescale <- function(x, newMin = 0, newMax = 1) {
  if (is(x, "RasterLayer")){
    xVec <- raster::getValues(x)
  } else {
    if (is(x, "numeric")) {
      xVec <- x
    } else {
      stop("x can only be numeric or a RasterLayer for now")
    }
  }
  xMin <- min(xVec, na.rm = TRUE)
  xMax <- max(xVec, na.rm = TRUE)
  if (all(xMin == 0, xMax == 0)){
    return(x)
  }
  newVals <- newMin + (xVec - xMin) * ((newMax - newMin) / (xMax - xMin))
  if (is(x, "RasterLayer")){
    return(raster::setValues(x, newVals))
  } else return(newVals)
}