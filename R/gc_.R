
#' @title Great Circle as \link[sp]{SpatialLines} Object
#' 
#' @description ..
#' 
#' @param x \link[sp]{SpatialPoints} object
#' 
#' @returns 
#' Function [gc_] returns an \link[sf]{sf} object.
#' 
#' @importFrom geosphere gcIntermediate
#' @importFrom methods as
#' @importFrom sf st_as_sf
#' @export
gc_ <- function(x) {
  
  nr <- dim(x@coords)[1L]
  
  sl <- gcIntermediate(
    p1 = x[1:(nr-1L), , drop = FALSE], 
    p2 = x[2:nr, , drop = FALSE], 
    n = 101L, 
    breakAtDateLine = TRUE, # or 'Meridian-wrap'
    addStartEnd = TRUE, 
    sp = TRUE) # 'SpatialLines'
  
  # I know very little about ?methods::as
  # I debugged ?methods::as and found ?sf::st_as_sf needs to be imported
  as(sl, Class = 'sf')
  
}

