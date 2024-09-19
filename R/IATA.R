



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
  ap <- airports@data
  id <- match(x, table = ap$iata_code)
  id_na <- is.na(id)
  
  id[id_na] <- vapply(x[id_na], FUN = function(ix) { # NULL compatible
    if (length(id_city <- grep(ix, x = ap$municipality)) == 1L) return(id_city)
    if (length(id_name <- grep(ix, x = ap$name)) == 1L) return(id_name)
    id <- unique.default(c(id_city, id_name))
    if (!length(id)) stop('No city/airport match for ', ix)
    print.data.frame(ap[id, c('iata_code', 'municipality', 'name'), drop = FALSE], row.names = FALSE)
    stop('Multiple city/airport match!')
  }, FUN.VALUE = 0L)
  
  return(id)
}



#' @importFrom base.tzh lower_n
#' @export
print.IATA <- function(x, ...) {
  cat('\n')
  
  x_ <- lapply(x, FUN = function(ix) airports[ix, , drop = FALSE])
  
  #lapply(x_, FUN = function(i) {
  #  co_ <- i@coords
  #  cat(sprintf('%s (%.3f, %.3f)', dimnames(co_)[[1L]], co_[,1L], co_[,2L]), sep = '\n')
  #})
  #cat('\n')
  
  tmp <- vapply(x_, FUN = function(i) {
    dist_ <- distGeo_(i@coords, Labels = i@data[['iata_code']])
    dist_m <- as.matrix(dist_) # stats:::as.matrix.dist
    id <- lower_n(dist_m, n = -1L)
    dnm <- dimnames(dist_m)
    a <- which(id, arr.ind = TRUE)
    ret <- cbind(
      Miles = sprintf(fmt = '%.1f', dist_m[id]),
      Kilometer = sprintf(fmt = '%.1f', dist_m[id] * 1.609)
    )
    rownames(ret) <- sprintf(fmt = '%s \u2708\ufe0f %s', dnm[[1L]][a[,1L]], dnm[[2L]][a[,2L]])
    print.noquote(ret, right = TRUE)
    cat('\n')
    return(sum(dist_m[id]))
  }, FUN.VALUE = NA_real_)
  
  cat(sprintf(fmt = 'Total mileage: %.1f\n\n', sum(tmp)))
  
  # I need to refine these functions some time
  #RoundTheWorld(x, airline = 'ANA')
  #RoundTheWorld(x, airline = 'SQ')
  
  # print(autoplot.IATA(x)) # no longer default!!
  print(turn_IATA(x)) # this is much prettier!!
  
}











