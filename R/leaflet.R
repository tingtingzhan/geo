

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
#' @importFrom sf st_geometry
#' @method as.leaflet sf
#' @export
as.leaflet.sf <- function(x, ...) {
  cls <- class(st_geometry(x))[[1L]]
  switch(
    EXPR = cls, 
    sfc_POINT = {
      x |>
        leafletBase() |>
        leafletMarkers(...)
    }, sfc_POLYGON =, sfc_MULTIPOLYGON = {
      x |>
        leafletBase() |>
        leafletPolygons(...)
    }, sfc_LINESTRING = {
      x |>
        leafletBase() |>
        leafletPolylines(...)
    }, stop('unsupported ', sQuote(cls))
  )
  
}


#' @rdname as.leaflet
#' @examples
#' 'EWR-PHL-JFK-IAD' |> as.iata() |> as.leaflet(popup = ~ shortnm)
#' 'EWR-PHL-JFK-IAD, DFW-IAH' |> as.iata() |> as.leaflet(popup = ~ shortnm)
#' @method as.leaflet iatalist
#' @export
as.leaflet.iatalist <- function(x, ...) {
  airports_ip2location[unlist(x), , drop = FALSE] |>
    as.leaflet.sf(...)
}






# @param sf an `sf` object
#' @importFrom sf st_transform
#' @importFrom leaflet leaflet addTiles addProviderTiles providers 
leafletBase <- \(sf) {
  sf |>
    st_transform(x = _, crs = 4326) |>
    leaflet(data = _) |>
    addTiles(group = 'OpenStreetMap') |>
    addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite')
  # leaflet::providers$Stadia.* # Quarto book authentication bug
}


#' @importFrom leaflet addLayersControl layersControlOptions addMarkers markerClusterOptions
leafletMarkers <- \(
  x,
  clusterOptions = markerClusterOptions(),
  ...
) {
  x |>
    addMarkers(
      clusterOptions = clusterOptions,
      ...
    ) |>
    addLayersControl(
      baseGroups = c('OpenStreetMap', 'Satellite'),
      options = layersControlOptions()
    )
  
}



#' @importFrom leaflet addPolygons addLayersControl layersControlOptions
leafletPolygons <- \(
    x,
    fillColor = 'royalblue', fillOpacity = .1,
    weight = 2, color = 'white', opacity = 1,
    ...
) {
  x |>
    addPolygons(
      fillColor = fillColor, fillOpacity = fillOpacity,
      weight = weight, color = color, opacity = opacity,
      ...
    ) |>
    addLayersControl(
      baseGroups = c('OpenStreetMap', 'Satellite'),
      options = layersControlOptions()
    )
}


#' @importFrom leaflet addPolylines addLayersControl layersControlOptions
leafletPolylines <- \(
    x,
    ...
) {
  x |>
    addPolylines(
      ...
    ) |>
    addLayersControl(
      baseGroups = c('OpenStreetMap', 'Satellite'), 
      options = layersControlOptions()
    )
}

