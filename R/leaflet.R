

#' @title Convert to \CRANpkg{leaflet}
#' 
#' @description
#' To convert an R object into a \link[leaflet]{leaflet}.
#' 
#' 
#' @param x see **Usage**
#' 
#' @param popup,... additional parameters of the function [leaflet_mrk()]
#' 
#' @returns 
#' 
#' The function [as.leaflet()] returns a \link[leaflet]{leaflet} object.
#' 
#' @note 
#' 1 degree approx 69 miles.
#' 
#' Google geocode API has a query limit, thus 'character' location name such as 'white house' is not allowed.
#' 
#' 
#' 
#' @references
#' \url{http://rstudio.github.io/leaflet/}
#' \url{https://developers.google.com/maps/documentation/geocoding/}
#' \url{http://shiny.rstudio.com/gallery}
#'
#' @name as.leaflet
#' @export
as.leaflet <- function(x, ...) UseMethod(generic = 'as.leaflet')

#' @rdname as.leaflet
#' @export
as.leaflet.SpatialPoints <- function(x, ...) {
  leaflet_mrk(coords = x@coords, ...)
} # for legacy \pkg{sp}


#' @rdname as.leaflet
#' @export
as.leaflet.sf <- function(x, ...) {
  leaflet_mrk(coords = st_coordinates(x), ...)
}

#' @rdname as.leaflet
#' @examples
#' 'EWR-PHL-JFK-IAD' |> as.iata() |> as.leaflet()
#' 'EWR-PHL-JFK-IAD, DFW-IAH' |> as.iata() |> as.leaflet()
#' @export
as.leaflet.iatalist <- function(x, popup = rownames(ap), ...) {
  ap <- airports_ip2location[unlist(x), , drop = FALSE]
  as.leaflet.sf(x = ap, popup = popup, ...)
}



#' @title \link[leaflet]{leaflet} with Markers
#' 
#' @description
#' To create a \link[leaflet]{leaflet} with markers from coordinates.
#' 
#' @param coords 2-column \link[base]{matrix} of markers, *longitude* on the 1st column and *latitude* on the 2nd column
#' 
#' @param popup \link[base]{character} \link[base]{vector}, the popup text, see the detailed description from the function \link[leaflet]{addMarkers}.  Default value is the \link[base]{rownames} of `coords`
#' 
#' @param clusterOptions,... additional parameters of the function \link[leaflet]{addMarkers}
#' 
#' @details
#' The function [leaflet_mrk()] is a simple wrapper of the pipeline `leaflet() |> addTiles() |> addMarkers()`.
#' 
#' 
#' @returns
#' The function [leaflet_mrk()] returns a \link[leaflet]{leaflet} object.
#' 
#' @importFrom leaflet leaflet addMarkers addTiles markerClusterOptions
#' @export
leaflet_mrk <- function(
    coords, 
    popup = rownames(coords), 
    clusterOptions = markerClusterOptions(),
    ...) {
  
  if (!is.matrix(coords) || !is.numeric(coords) || anyNA(coords) || dim(coords)[2L] != 2L) stop('coords must be coords')
  
  if (!length(popup) || anyNA(popup) || !all(nzchar(popup)))
    stop('popup must be of same length as coords') # lazy evaluation!
  
  leaflet() |>
    addTiles() |>
    addMarkers(
      lng = coords[,1L], lat = coords[,2L], 
      popup = popup,
      clusterOptions = clusterOptions,
      ...
    )
  
}

