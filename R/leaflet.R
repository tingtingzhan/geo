

#' @title Convert to \CRANpkg{leaflet}
#' 
#' @description
#' To convert an R object into a \link[leaflet]{leaflet}.
#' 
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters of the functions \link[leaflet]{addMarkers}, \link[leaflet]{addPolylines} and \link[leaflet]{addPolygons}.
#' 
#' @returns 
#' 
#' The function [as.leaflet()] returns a \link[leaflet]{leaflet} object.
#' 
#' @note 
#' 1 degree approx 69 miles.
#' 
#' @references
#' \url{http://rstudio.github.io/leaflet/}
#' \url{https://shiny.posit.co/r/gallery/}
#'
#' @name as.leaflet
#' @export
as.leaflet <- function(x, ...) UseMethod(generic = 'as.leaflet')


#' @rdname as.leaflet
#' 
#' @param clusterOptions see the function \link[leaflet]{addMarkers}
#' 
#' @param fillColor,fillOpacity,weight,color,opacity see the function \link[leaflet]{addPolygons}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @importFrom sf st_geometry
#' @importFrom leaflet addMarkers markerClusterOptions
#' @importFrom leaflet addPolygons
#' @importFrom leaflet addPolylines
#' @method as.leaflet sf
#' @export
as.leaflet.sf <- \(
    x, 
    clusterOptions = markerClusterOptions(), # ?leaflet::addMarkers
    fillColor = 'royalblue', fillOpacity = .1, weight = 2, color = 'white', opacity = 1, # ?leaflet::addPolygons
    ...
) {
  cls <- class(st_geometry(x))[[1L]]
  switch(
    EXPR = cls, 
    sfc_POINT = {
      x |>
        leaflet_minimal() |>
        addMarkers(
          clusterOptions = clusterOptions,
          ...
        )
    }, sfc_POLYGON =, sfc_MULTIPOLYGON = {
      x |>
        leaflet_minimal() |>
        addPolygons(
          fillColor = fillColor, fillOpacity = fillOpacity,
          weight = weight, color = color, opacity = opacity,
          ...
        )
    }, sfc_LINESTRING = {
      x |>
        leaflet_minimal() |>
        addPolylines(...)
    }, stop('unsupported ', sQuote(cls))
  )
  
}


#' @rdname as.leaflet
#' @examples
#' 'EWR-PHL-JFK-IAD' |> as.iata() |> as.leaflet(popup = ~ shortnm)
#' 'EWR-PHL-JFK-IAD, DFW-IAH' |> as.iata() |> as.leaflet(popup = ~ shortnm)
#' @method as.leaflet iatalist
#' @export
as.leaflet.iatalist <- \(x, ...) {
  airports_ip2location[unlist(x), , drop = FALSE] |>
    as.leaflet.sf(...)
}


#' @rdname as.leaflet
#' @examples
#' subsolarPoint() |> as.leaflet(popup = 'Sun')
#' @importFrom sf st_sf st_sfc
#' @method as.leaflet POINT
#' @export
as.leaflet.POINT <- \(x, ...) {
  st_sf(
    geometry = st_sfc(x, crs = 4326)
  ) |>
    as.leaflet.sf(...)
}

#' @rdname as.leaflet
#' @examples
#' twilightPolygon(sza = (90+6) * pi / 180) |> as.leaflet() # not perfect yet!
#' 
#' @method as.leaflet POLYGON
#' @export
as.leaflet.POLYGON <- as.leaflet.POINT




# leaflet::providers$Stadia.* # Quarto book authentication bug

# @param sf an `sf` object
#' @importFrom sf st_transform
#' @importFrom leaflet leaflet addTiles addProviderTiles providers addLayersControl layersControlOptions
leaflet_minimal <- \(sf) {
  sf |>
    st_transform(x = _, crs = 4326) |>
    leaflet(data = _) |>
    addTiles(group = 'OpenStreetMap') |>
    addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') |>
    addLayersControl(
      baseGroups = c('OpenStreetMap', 'Satellite'),
      options = layersControlOptions()
    )
  # returns a 'leaflet' object with 
  # attr(.$x, which = 'leafletData') being the input 'sf'
}




