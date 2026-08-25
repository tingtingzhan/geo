

#' @title Convert to \CRANpkg{leaflet}
#' 
#' @description
#' To convert an R object into a \link[leaflet]{leaflet}.
#' 
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters of the functions \link[leaflet]{addMarkers} and \link[leaflet]{addPolygons}.
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
#' @method as.leaflet sf
#' @export
as.leaflet.sf <- function(x, ...) {
  switch(
    EXPR = class(x$geometry)[[1L]], 
    sfc_POINT = {
      leafletMarkers(x, ...)
    }, sfc_POLYGON =, sfc_MULTIPOLYGON = {
      leafletPolygons(x, ...)
    }, stop('unsupported ', sQuote(class(x$geometry)[[1L]]))
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
#' @importFrom leaflet leaflet addTiles addProviderTiles providers addLayersControl layersControlOptions addMarkers addPolygons markerClusterOptions
leafletMarkers <- function(
    sf,
    clusterOptions = markerClusterOptions(),
    ...
) {
  
  sf |>
    st_transform(x = _, crs = 4326) |>
    leaflet(data = _) |>
    addTiles(group = 'OpenStreetMap') |>
    addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') |>
    #addProviderTiles(providers$Stadia.StamenTerrain, group = 'Terrain') |> # Quarto book authentication bug
    addMarkers(
      #group = 'Markers',
      clusterOptions = clusterOptions,
      ...
    ) |>
    addLayersControl(
      baseGroups = c('OpenStreetMap', 'Satellite'), # , 'Terrain'
      #overlayGroups = c('Markers'), # tzh prefers `addMarkers` be permanant
      options = layersControlOptions()
    )
  
}


leafletPolygons <- function(
    sf,
    fillColor = 'royalblue', fillOpacity = .1,
    weight = 2, color = 'white', opacity = 1,
    ...
) {
  sf |>
    st_transform(x = _, crs = 4326) |>
    leaflet(data = _) |>
    addTiles(group = 'OpenStreetMap') |>
    addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') |>
    #addProviderTiles(providers$Stadia.StamenTerrain, group = 'Terrain') |> # Quarto book authentication bug
    addPolygons(
      #group = 'Polygons',
      fillColor = fillColor, fillOpacity = fillOpacity,
      weight = weight, color = color, opacity = opacity,
      ...
    ) |>
    addLayersControl(
      baseGroups = c('OpenStreetMap', 'Satellite'), # , 'Terrain'
      #overlayGroups = c('Polygons'), # tzh prefers `addPolygons` be permanant
      options = layersControlOptions()
    )
}



