



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
#' IATA('TFU-LAX-EWR')
#' IATA('CTU-PEK-ICN-JFK, EWR-IAH-LIM')
#' IATA('YDF-YVZ')
#' IATA('TFU-SIN-FRA-JFK')
#' IATA('IAD-NRT-PVG-TFU')
#' IATA('PHL-FRA-AAL, AAL-LIS-EWR')
#' IATA('PEK-ICN-AKL-PPT')
#' IATA('NRT-HNL-YVR, SEA-YYZ-IAD-VIE-LCA-HKG')
#' IATA(c('NRT-HNL-YVR', 'SEA-YYZ-IAD-VIE-LCA-HKG')) # same
#' @export
IATA <- function(x) {
  if (!is.character(x)) stop('only accepts airport names as character')
  x <- unlist(strsplit(x, split = ', ', fixed = TRUE), use.names = FALSE)
  x_airpt <- strsplit(x, split = '-', fixed = TRUE) # always 'list'
  if (any(lengths(x_airpt, use.names = FALSE) < 2L)) stop('a trip must have >=2 airports')
  ret <- lapply(x_airpt, FUN = match_airport)
  class(ret) <- 'IATA'
  return(ret)
}


match_airport <- function(x) {
  if (!is.character(x) || !length(x) || anyNA(x) || !all(nzchar(x))) stop('illegal x')
  ap <- airports_ip2location@data
  id <- match(x, table = ap$iata)
  id_na <- is.na(id)
  
  id[id_na] <- vapply(x[id_na], FUN = function(ix) { # NULL compatible
    if (length(id_city <- grep(ix, x = ap$municipality)) == 1L) return(id_city)
    if (length(id_name <- grep(ix, x = ap$name)) == 1L) return(id_name)
    id <- unique.default(c(id_city, id_name))
    if (!length(id)) stop('No city/airport match for ', ix)
    print.data.frame(ap[id, , drop = FALSE], row.names = FALSE)
    stop('Multiple city/airport match!')
  }, FUN.VALUE = 0L)
  
  return(id)
}



#' @importFrom geosphere distGeo
#' @export
print.IATA <- function(x, ...) {
  cat('\n')
  
  x_ <- lapply(x, FUN = function(ix) airports_ip2location[ix, , drop = FALSE])
  
  tmp <- vapply(x_, FUN = function(i) {
    # (i = x_[[1L]])
    
    n <- dim(i@coords)[1L]
    dist_m_ <- distGeo(
      p1 = i@coords[seq_len(n-1L),], 
      p2 = i@coords[seq_len(n)[-1L],]
    ) # in meters
    
    ret <- cbind(
      Miles = sprintf(fmt = '%.1f', dist_m_ / 1609.34), # ?grid::convertUnit does not have meter/miles conversion
      Kilometer = sprintf(fmt = '%.1f', dist_m_ / 1e3)
    )
    rownames(ret) <- sprintf(fmt = '%s \u2708\ufe0f %s', i@data$iata[seq_len(n-1L)], i@data$iata[seq_len(n)[-1L]])
    print.noquote(ret, right = TRUE)
    cat('\n')
    return(sum(dist_m_ / 1609.34))
  }, FUN.VALUE = NA_real_)
  
  cat(sprintf(fmt = 'Total mileage: %.1f\n\n', sum(tmp)))
  
  # I need to refine these functions some time
  #RoundTheWorld(x, airline = 'ANA')
  #RoundTheWorld(x, airline = 'SQ')
  
  # print(autoplot.IATA(x)) # no longer default!!
  print(turn_IATA(x)) # this is much prettier!!
  
}











