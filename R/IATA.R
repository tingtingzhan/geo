



#' @title One or More Air Travel Trips using IATA codes
#' 
#' @description 
#' 
#' One or more air travel trips using International Air Transport Association (IATA) airport codes.
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector}
#' 
#' @note
#' \code{getMethod(sp::coordinates, signature('SpatialPoints'))}
#' is slow and will be avoided as much as possible
#' 
#' @returns 
#' 
#' Function [IATA()] returns an `'IATA'` object, which is essentially
#' a \link[base]{list} of \link[base]{integer} vectors.
#' 
#' @examples 
#' IATA('NRT-HNL-YVR')
#' IATA('TFU-LAX-EWR')
#' IATA('YDF-YVZ')
#' IATA('NRT-HNL-YVR, SEA-YYZ-IAD-VIE-LCA-HKG')
#' IATA(c('NRT-HNL-YVR', 'SEA-YYZ-IAD-VIE-LCA-HKG')) # same
#' 
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




#' @export
print.IATA <- function(x, ...) {
  cat('\n')
  
  x_ <- lapply(x, FUN = function(ix) airports[ix, , drop = FALSE])
  
  lapply(x_, FUN = function(i) {
    co_ <- i@coords
    cat(sprintf('%s (%.3f, %.3f)', dimnames(co_)[[1L]], co_[,1L], co_[,2L]), sep = '\n')
  })
  
  cat('\n')
  
  tmp <- vapply(x_, FUN = function(i) {
    dist_ <- distGeo_(i@coords, Labels = i@data[['iata_code']])
    dist_m <- as.matrix(dist_) # stats:::as.matrix.dist
    id <- lower_n(dist_m, n = -1L)
    dnm <- dimnames(dist_m)
    a <- which(id, arr.ind = TRUE)
    ret <- cbind(
      From = dnm[[1L]][a[,1L]],
      To = dnm[[2L]][a[,2L]],
      Miles = sprintf(fmt = '%.1f', dist_m[id]),
      Kilometer = sprintf(fmt = '%.1f', dist_m[id] * 1.609)
    )
    rownames(ret) <- character(length = dim(ret)[1L]) # inspired by ?base::prmatrix
    print.noquote(ret, right = TRUE)
    cat('\n')
    return(sum(dist_m[id]))
  }, FUN.VALUE = NA_real_)
  
  cat(sprintf(fmt = 'Total mileage: %.1f\n\n', sum(tmp)))
  
  # I need to refine these functions some time
  #RoundTheWorld(x, airline = 'ANA')
  #RoundTheWorld(x, airline = 'SQ')
  
  print(autoplot.IATA(x))
  
}


# I know very little about ?methods::as
# I debugged ?methods::as and found ?sf::st_as_sf needs to be imported
#' @importFrom ggplot2 aes autolayer geom_sf
#' @importFrom methods as
#' @importFrom sf st_as_sf
#' @export
autolayer.IATA <- function(object, ...) {
  
  apts <- lapply(object, FUN = function(obj) airports[obj, , drop = FALSE])

  # ?ggplot2::geom_sf includes ?ggplot2::layer_sf and ?ggplot2::coord_sf
  # ?ggplot2::coord_sf will print a message when 'Coordinate system already present'
  
  sf_data <- lapply(apts, FUN = function(iapt) as(greatCircle(iapt), Class = 'sf'))
  
  n <- length(object)
  if (n == 1L) return(geom_sf(data = sf_data[[1L]]))

  nseq <- seq_len(n)
  nm <- as.character.default(nseq)
  lapply(nseq, FUN = function(i) geom_sf(
    data = sf_data[[i]],
    mapping = aes(colour = nm[i]), 
    
    # see the use of ?grid::arrow
    # https://stackoverflow.com/questions/67412079/is-there-a-way-to-add-arrows-to-a-simple-features-line-in-ggplot-geom-sf
    #arrow = grid::arrow(angle = 25, 
    #              ends = "last", 
    #              type = "open", 
    #              length = unit(0.5, "cm")),
    # this has no effect for me, yet
    
    show.legend = FALSE
  ))
}


#' @importFrom ggplot2 autoplot ggplot xlim ylim
#' @export
autoplot.IATA <- function(object, ...) {
  ggplot() + geom_worldmap(
    fill = 'white', 
    linewidth = .2, # country border thickness
    colour = 'grey65', # country border colour
  ) + 
    autolayer.IATA(object, ...) + 
    xlim(c(-160, 175)) + ylim(c(-75, 80)) # have to leave some space for x- and y-labels to show! 
}


#' @title Layer of World Map
#' 
#' @description
#' ..
#' 
#' @param ... parameters of \link[ggplot2]{layer},
#' to be passed into \link[ggplot2]{geom_map}
#' 
#' @references 
#' \url{https://www.riinu.me/2022/02/world-map-ggplot2/}
#' 
#' @examples
#' # example code
#' library(ggplot2)
#' ggplot() + geom_worldmap() + xlim(c(-160, 175)) + ylim(c(-75, 80))
#' ggplot() + geom_worldmap() # does not work!!
#' 
#' @importFrom dplyr distinct
#' @importFrom ggplot2 aes fortify geom_map
#' @importFrom maps map
#' @export
geom_worldmap <- function(...) {
  worldmap <- fortify(map('world', regions = '.', exact = FALSE, plot = FALSE, fill = TRUE))
  # ?ggplot2::map_data; ?ggplot2:::fortify.map
  
  geom_map(
    data = eval(call('distinct', .data = quote(worldmap), quote(region))), # ?dplyr:::distinct.data.frame
    mapping = eval(call('aes', map_id = quote(region))),
    map = worldmap, 
    ...)
  
}




#' @title Great Circle as \link[sp]{SpatialLines} Object
#' 
#' @description ..
#' 
#' @param x \link[sp]{SpatialPoints} object
#' 
#' @returns 
#' Function [greatCircle()] returns an \link[sp]{SpatialLines} object.
#' 
#' @importFrom geosphere gcIntermediate
#' @export
greatCircle <- function(x) {
  
  nr <- dim(x@coords)[1L]
  
  gcIntermediate(
    p1 = x[1:(nr-1L), , drop = FALSE], 
    p2 = x[2:nr, , drop = FALSE], 
    n = 101L, 
    breakAtDateLine = TRUE, # or 'Meridian-wrap'
    addStartEnd = TRUE, 
    sp = TRUE) # returns 'SpatialLines' object
  
}








