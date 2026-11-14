# read stats:::na.omit.default carefully
#' @importFrom stats na.omit
#' @method na.omit POSIXlt
#' @export
na.omit.POSIXlt <- \(object, ...) {
  
  omit <- seq_along(object)[is.na(object)] 
  # base::is.na.POSIXlt is good
  
  if (!length(omit)) return(object)
  
  nm <- names(object)
  object <- object[-omit]
  
  if (any(omit > 0L)) {
    names(omit) <- nm[omit]
    attr(omit, 'class') <- 'omit'
    attr(object, 'na.action') <- omit
  }
  return(object)
  
}
