normalizeStack <- function(stk){
  normalized <- lapply(names(stk), function(streamLay){
    lay <- rescale(stk[[streamLay]])
    names(lay) <- streamLay
    return(lay)
  })
  names(normalized) <- names(stk)
  return(normalized)
}