#' Normalize each layer of a \code{RasterStack}
#'
#' Rescales the values of of each \code{RasterLayer} between \code{[0,1]}.
#'
#' @param x A \code{RasterStack} object.
#'
#' @author Tati Micheletti
#' @export
#' @importFrom amc rescale
normalizeStack <- function(x) {
  normalized <- lapply(names(x), function(layer) {
    lay <- rescale(x[[layer]])
    names(lay) <- layer
    return(lay)
  })
  names(normalized) <- names(x)
  return(normalized)
}
