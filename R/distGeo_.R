
# no longer in Tingting's master package \pkg{tzh}

#' @title Distance on an Ellipsoid, as \link[stats]{dist} Object
#' 
#' @description ..
#' 
#' @param x 2-column \link[base]{numeric} \link[base]{matrix}, coordinates
#' 
#' @param Labels see **Value** section of \link[stats]{dist}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details 
#' 
#' Function [distGeo_] ...
#' 
#' @note
#' 
#' Function [distGeo_] can be passed into argument `distfun` of \link[stats]{heatmap}.
#' 
#' @importFrom geosphere distGeo
#' @export
distGeo_ <- function(x, Labels = dimnames(x)[[1L]], ...) {
  if (!is.matrix(x)) stop('`distGeo_` only takes matrix')
  dmx <- dim(x)
  if ((nr <- dmx[1L]) == 1L) return(invisible())
  if (dmx[2L] != 2L) stop('coordinates must be ncol-2')
  # ?geosphere::distm with missing `y` (non-missing `y` makes result non-symmetric)
  # improve from ?geosphere:::.distm1: ?geosphere::distGeo is 'vectorized'
  
  dm <- c(nr, nr)
  id <- which(.row(dm) > .col(dm), arr.ind = TRUE)
  
  ret <- distGeo(p1 = x[id[,1L],], p2 = x[id[,2L],]) / 1609.34 # ?grid::convertUnit does not have meter/miles conversion
  class(ret) <- 'dist'
  attr(ret, which = 'Size') <- nr
  attr(ret, which = 'Labels') <- Labels
  return(ret)
}



