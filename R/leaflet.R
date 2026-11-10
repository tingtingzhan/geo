

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
#' @note
#' The function \link[sf]{st_geometry} picks only the active geometry.
#' 
#' @importFrom sf st_transform st_geometry<-
#' @method as.leaflet sf
#' @export
as.leaflet.sf <- \(x, ...) {
  
  map <- x |>
    leaflet_minimal()
  
  cls <- x |>
    vapply(FUN = inherits, what = 'sfc', FUN.VALUE = NA)
  
  if (sum(cls) == 1L) {
    
    map <- map |>
      add_sf_active(x = x, ...)
    
  } else {
    
    for (i in names(which(cls))) {
      st_geometry(x) <- i
      map <- map |>
        add_sf_active(x = x, ...)
    }
    
  }
  
  return(map)
  
}




#' @importFrom leaflet addMarkers addAwesomeMarkers markerClusterOptions
#' @importFrom leaflet addPolygons
#' @importFrom leaflet addPolylines
#' @importFrom sf st_geometry
add_sf_active <- \(
  map, # 'leaflet'
  x, # 'sf'
  clusterOptions = markerClusterOptions(), icon = NULL, # ?leaflet::addMarkers
  fillColor = 'royalblue', fillOpacity = .1, weight = 2, color = 'white', opacity = 1, # ?leaflet::addPolygons
  ...
) {
  
  x <- x |>
    st_transform(x = _, crs = 4326)
  # sf:::st_transform.sf transforms only the active 'sfc'
  
  cls <- class(st_geometry(x))[[1L]]
  
  switch(
    
    EXPR = cls, 
    
    sfc_POINT = {
      
      if (inherits(icon, what = 'leaflet_awesome_icon')) {
        
        map |>
          addAwesomeMarkers(
            data = x,
            clusterOptions = clusterOptions,
            icon = icon,
            ...
          )
        
      } else {
      
        map |>
          addMarkers(
            data = x,
            clusterOptions = clusterOptions,
            icon = icon,
            ...
          )
        
      }

    }, sfc_POLYGON =, sfc_MULTIPOLYGON = {
      
      map |>
        addPolygons(
          data = x,
          fillColor = fillColor, fillOpacity = fillOpacity,
          weight = weight, color = color, opacity = opacity,
          ...
        )
      
    }, sfc_LINESTRING = {
      
      map |>
        addPolylines(
          data = x,
          ...
        )
      
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
#' subSolarPoint() |> as.leaflet(popup = 'Sub-Solar Point')
#' subLunarPoint() |> as.leaflet(popup = 'Sub-Lunar Point')
#' @importFrom sf st_sf st_sfc
#' @method as.leaflet POINT
#' @export
as.leaflet.POINT <- \(x, ...) {
  x |>
    st_sfc(crs = 4326) |>
    st_sf(geometry = _) |>
    as.leaflet.sf(...)
}

#' @rdname as.leaflet
#' @examples
#' # \pkg{leaflet} does **not** draw edges with great circle!
#' terminatorPolygon() |> as.leaflet()
#' 
#' terminatorPolygon(n = 501L) |> as.leaflet()
#' 
#' twilightPolygon(sza = (90+6) * pi / 180) |> as.leaflet() # not perfect yet!
#' 
#' @method as.leaflet POLYGON
#' @export
as.leaflet.POLYGON <- as.leaflet.POINT




# leaflet::providers$Stadia.* # Quarto book authentication bug

# @param x an `sf` object
#' @importFrom leaflet leaflet addTiles addProviderTiles providers addLayersControl layersControlOptions
leaflet_minimal <- \(x) {
  x |> # 'sf' object
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






# library(leaflet)
# leaflet() |> addTiles() |> addPolygons(terminatorPolygon())
# # not yet







