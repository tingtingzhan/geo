



#' @title One or More Air Travel Trips using IATA codes
#' 
#' @description 
#' 
#' One or more air travel trips using International Air Transport Association (IATA) airport codes.
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector}
#' 
#' @returns 
#' 
#' Function [IATA] returns an object of S3 class `'IATA'`, which is essentially
#' a \link[base]{list} of \link[base]{integer} \link[base]{vector}s.
#' 
#' @examples 
#' IATA('NRT-HNL-YVR')
#' IATA('CTU-PEK-ICN-JFK, EWR-IAH-LIM')
#' @export
IATA <- function(x) {
  if (!is.character(x)) stop('only accepts airport names as character')
  x <- unlist(strsplit(x, split = ', ', fixed = TRUE), use.names = FALSE)
  x_airpt <- strsplit(x, split = '-', fixed = TRUE) # always 'list'
  if (any(lengths(x_airpt, use.names = FALSE) < 2L)) stop('a trip must have >=2 airports')
  ret <- lapply(x_airpt, FUN = function(x) {
    if (!is.character(x) || !length(x) || anyNA(x) || !all(nzchar(x))) stop('illegal x')
    id <- match(x, table = airports_ip2location@data$iata, nomatch = NA_integer_)
    if (anyNA(id)) stop('must use IATA code')
    return(id)
  })
  class(ret) <- 'IATA'
  return(ret)
}


#' @importFrom geosphere distGeo
print_IATA_ <- function(x) {
  # `x` is one-trip 'IATA' (as \link[base]{vector}, not \link[base]{list}!!)
  ap <- airports_ip2location[x, , drop = FALSE]
  n <- length(x)
  sq1 <- seq_len(n-1L)
  sq2 <- seq_len(n)[-1L]
  m_ <- distGeo(p1 = ap@coords[sq1,], p2 = ap@coords[sq2,]) # in meters
  ret = cbind(
    Miles = m_ / 1609.34, # ?grid::convertUnit does not have meter/miles conversion
    Kilometer = m_ / 1e3
  )
  ret[] <- sprintf(fmt = '%.1f', ret)
  rownames(ret) <- sprintf(fmt = '%s \u2708\ufe0f %s', ap@data$iata[sq1], ap@data$iata[sq2])
  print(ret, quote = FALSE, right = TRUE)
  cat('\n')
  return(invisible(sum(m_ / 1609.34)))
}



#' @export
print.IATA <- function(x, ...) {
  cat('\n')
  
  tmp <- vapply(x, FUN = print_IATA_, FUN.VALUE = NA_real_)
  
  cat(sprintf(fmt = 'Total Mileage: %.1f\n\n', sum(tmp)))
  
  # I need to refine these functions some time
  #RoundTheWorld(x, airline = 'ANA')
  #RoundTheWorld(x, airline = 'SQ')
  
  # print(autoplot.IATA(x)) # no longer supported!!
  print(turn_IATA(x)) # this is much prettier!!
  
}











