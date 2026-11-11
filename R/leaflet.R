

# leaflet::providers$Stadia.* # Quarto book authentication bug


#' @title [leaflet_minimal()]
#' 
#' @param x an `'sf'` object
#' 
#' @importFrom leaflet leaflet addTiles addProviderTiles providers addLayersControl layersControlOptions
#' @export
leaflet_minimal <- \(x) {
  x |> # 
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





# how to do this elegantly??
# @examples
# subSolarPoint() |> as.leaflet(popup = 'Sub-Solar Point')
# subLunarPoint() |> as.leaflet(popup = 'Sub-Lunar Point')
# @importFrom sf st_sf st_sfc
# @method as.leaflet POINT
# @export
#as.leaflet.POINT <- \(x, ...) {
#  x |>
#    st_sfc(crs = 4326) |>
#    st_sf(geometry = _) |>
#    as.leaflet.sf(...)
#}

# @examples
# # \pkg{leaflet} does **not** draw edges with great circle!
# terminatorPolygon() |> as.leaflet()
# terminatorPolygon(n = 501L) |> as.leaflet()
# twilightPolygon(sza = (90+6) * pi / 180) |> as.leaflet() # not perfect yet!
# @method as.leaflet POLYGON
# @export
#as.leaflet.POLYGON <- as.leaflet.POINT




# library(leaflet)
# leaflet_minimal() |> addPolygons(terminatorPolygon())
# # not yet

