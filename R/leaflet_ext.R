

# leaflet::providers$Stadia.* # Quarto book authentication bug


#' @title Minimal Map by \CRANpkg{leaflet}
#' 
#' @description
#' To create a minimal map by \CRANpkg{leaflet} containing an `'sf'` input.
#' 
#' @param x an `'sf'` object
#' 
#' @returns
#' The function [leaflet_minimal()] returns a \link[leaflet]{leaflet} object with `attr(.$x, which = 'leafletData')` being the input `'sf'` object.
#' 
#' @importFrom leaflet leaflet addTiles addProviderTiles providers addLayersControl layersControlOptions
#' @keywords minimal-map leaflet
#' @export
leaflet_minimal <- \(x) {
  
  z <- if (missing(x)) {
    leaflet()
  } else leaflet(data = x)
  
  z |>
    addTiles(
      group = 'OpenStreetMap'
    ) |>
    addProviderTiles(
      providers$Esri.WorldImagery, 
      group = 'Satellite'
    ) |>
    addLayersControl(
      baseGroups = c('OpenStreetMap', 'Satellite'),
      options = layersControlOptions()
    )
  
}





