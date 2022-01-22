cleanLayers <- function(stk){
  # We need to get rid of any really low values that might be essentially zeros.
  # We are using the limits set by prioritzR
  minVals <- unlist(lapply(1:nlayers(stk), function(L){
    return(minValue(stk[[L]]))
  }))
  maxVals <- unlist(lapply(1:nlayers(stk), function(L){
    return(maxValue(stk[[L]]))
  }))
  laysToClean <- c(which(minVals < 1e-5), which(maxVals > 1e+5))
  reclass <- matrix(c(-Inf, 1e-5, 0,
                      1e+5, Inf, 1e+5), 
                    ncol = 3, byrow = TRUE) # "from" "to" "becomes" 
  cleanStk <- stack(lapply(1:nlayers(stk), function(lay){
    if (lay %in% laysToClean){
      cleanLay <- reclassify(x = stk[[lay]], rcl = reclass)
      return(cleanLay)
    } else {
      return(stk[[lay]])
    }
    names(cleanStk) <- names(stk)
    # Assertion that cleaning worked
    minVals <- unlist(lapply(1:nlayers(cleanStk), function(L){
      return(minValue(cleanStk[[L]]))
    }))
    maxVals <- unlist(lapply(1:nlayers(cleanStk), function(L){
      return(maxValue(cleanStk[[L]]))
    }))
    laysToClean <- c(which(minVals < 1e-5), which(maxVals > 1e+5))
    if (length(laysToClean) == 0) message(crayon::green(paste0("Layers cleaned! Values between ", 
                                                               min(minVals), " and ", max(maxVals)))) else 
      stop("Something went wrong when cleaning layers. Please debug.")
    return(cleanStk)
  }))
  
}    

