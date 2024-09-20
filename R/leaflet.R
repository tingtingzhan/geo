

#' @title Print using \CRANpkg{leaflet}
#' 
#' @description 1 degree approx 69 miles
#' 
#' @param x see **Usage**
#' 
#' @param ... potential parameters, currently not in use
#' 
#' @details 
#' 
#' Google geocode API has a query limit, thus 'character' location name such as 'white house'
#' is not allowed.
#' 
#' @returns 
#' 
#' Function [as.leaflet] returns a \link[leaflet]{leaflet} object.
#' 
#' @examples
#' as.leaflet(IATA('EWR-PHL-JFK-IAD'))
#' 
#' @references
#' \url{http://rstudio.github.io/leaflet/}
#' \url{https://developers.google.com/maps/documentation/geocoding/}
#' \url{http://shiny.rstudio.com/gallery/tags/leaflet/}
#'
#' @export
as.leaflet <- function(x, ...) UseMethod('as.leaflet')

#' @export
as.leaflet.IATA <- function(x, ...) as.leaflet.SpatialPoints(airports_ip2location[x[[1L]], , drop = FALSE])

#' @export
as.leaflet.SpatialPoints <- function(x, ...) leaflet_popup(coords = x@coords, ...)

# still playing
# as.leaflet.SpatialPolygons <- (PACovid19)




#' @title \link[leaflet]{leaflet} with Popup
#' 
#' @description
#' ..
#' 
#' @param coords 2-column \link[base]{matrix} of popup 
#' *longitude* (1st column) and *latitude* (2nd column)
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @importFrom leaflet leaflet addPopups popupOptions addTiles fitBounds
#' @export
leaflet_popup <- function(
  coords,
  ...
) {
  
  if (!is.matrix(coords) || !is.numeric(coords) || anyNA(coords) || dim(coords)[2L] != 2L) stop('coords must be coords')
  
  popup <- dimnames(coords)[[1L]] # rownames of `coords` as popup labels
  if (!length(popup) || anyNA(popup) || !all(nzchar(popup))) stop('popup must be of same length as coords') # lazy evaluation!
  lng <- coords[,1L]
  lat <- coords[,2L]
  
  map_empty <- fitBounds(map = addTiles(leaflet()), lat1 = min(lat), lat2 = max(lat), lng1 = min(lng), lng2 = max(lng))
  map_popup <- addPopups(
    map = map_empty, lng = lng, lat = lat, popup = popup,
    options = popupOptions(closeButton = FALSE, closeOnClick = FALSE)
  )
  return(map_popup)
}

