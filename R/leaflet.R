

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
} # for legacy \CRANpkg{sp}


#' @rdname as.leaflet
#' @export
as.leaflet.sf <- function(x, ...) {
  coords <- st_coordinates(x)
  switch(
    EXPR = class(x$geometry)[[1L]], 
    sfc_POINT = {
      if (ncol(coords) != 2L) stop('learn sf-package')
      leaflet_mrk(coords = coords, ...)
    }, sfc_MULTIPOLYGON = {
      leaflet_plg(x, ...)
    }, stop('unsupported')
  )
  
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
    ...
) {
  
  if (!is.matrix(coords) || !is.numeric(coords) || anyNA(coords) || dim(coords)[2L] != 2L) stop('coords must be coords')
  
  if (length(popup)) {
    if (length(popup) != nrow(coords) || anyNA(popup) || !all(nzchar(popup))){
      stop('popup must be of same length as coords') # lazy evaluation!
    }
  }
  
  leaflet() |>
    addTiles() |>
    addMarkers(
      lng = coords[,1L], lat = coords[,2L], 
      popup = popup,
      clusterOptions = clusterOptions,
      ...
    )
  
}



#' @title \link[leaflet]{leaflet} with Polygons
#' 
#' @description
#' To create a \link[leaflet]{leaflet} with polygons.
#' 
#' @param sf an `sf` object
#' 
#' @param fillColor,fillOpacity,weight,color,opacity,... additional parameters of the function \link[leaflet]{addPolygons}
#' 
#' @importFrom sf st_transform
#' @importFrom leaflet leaflet addTiles addPolygons
#' @export
leaflet_plg <- function(
    sf,
    fillColor = 'royalblue', fillOpacity = .1,
    weight = 2, color = 'white', opacity = 1,
    ...
) {
  sf |>
    st_transform(x = _, crs = 4326) |>
    leaflet(data = _) |>
    addTiles() |>
    addPolygons(
      fillColor = fillColor, fillOpacity = fillOpacity,
      weight = weight, color = color, opacity = opacity,
      ...
    )
}



