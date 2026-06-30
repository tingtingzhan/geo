

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
#' The function [as.leaflet()] returns a \link[leaflet]{leaflet} object.
#' 
#' @references
#' \url{http://rstudio.github.io/leaflet/}
#' \url{https://developers.google.com/maps/documentation/geocoding/}
#' \url{http://shiny.rstudio.com/gallery}
#'
#' @examples
#' 'EWR-PHL-JFK-IAD' |> as.iata() |> as.leaflet()
#' @name as.leaflet
#' @export
as.leaflet <- function(x, ...) UseMethod(generic = 'as.leaflet')

#' @rdname as.leaflet
#' @export
as.leaflet.iatalist <- function(x, ...) {
  ap <- airports_ip2location[x[[1L]], , drop = FALSE] # 'SpatialPoints'
  leaflet_popup(coords = ap@coords, popup = ap$iata, ...)
}


#' @rdname as.leaflet
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
#' @param popup \link[base]{character} \link[base]{vector},
#' popup text.  Default is the \link[base]{rownames} of `coords`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' The function [leaflet_popup()] returns an \link[leaflet]{leaflet} object, which inherits from \CRANpkg{htmlwidgets}.
#' 
#' @keywords internal
#' @importFrom leaflet leaflet addPopups popupOptions addTiles fitBounds
#' @export
leaflet_popup <- function(coords, popup = rownames(coords), ...) {
  
  if (!is.matrix(coords) || !is.numeric(coords) || anyNA(coords) || dim(coords)[2L] != 2L) stop('coords must be coords')
  
  if (!length(popup) || anyNA(popup) || !all(nzchar(popup))) stop('popup must be of same length as coords') # lazy evaluation!
  lng <- coords[,1L]
  lat <- coords[,2L]
  
  leaflet() |>
    addTiles() |>
    fitBounds(lat1 = min(lat), lat2 = max(lat), lng1 = min(lng), lng2 = max(lng)) |>
    addPopups(
      lng = lng, lat = lat, popup = popup#,
      #options = popupOptions(closeButton = FALSE, closeOnClick = FALSE)
    )
  
}

