
#' @title The Earth with Various Layers, Animated
#' 
#' @param map an \CRANpkg{htmlwidgets} created by the function \link[plotly]{plot_geo}. Default value is the return of the function [add_TropicPolar()]
#' 
#' @param time a \link[base]{POSIXct} scalar or \link[base]{vector}. Default value is six (`6L`) equidistant timepoints in the next 24 hours.
#' 
#' @param type,sza see the function [add_twilight()]
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @note
#' The function [add_twilight_animated()] does **not** print twilight polygons, which is compute extensive, for more than six `time` points.
#' 
#' @examples
#' add_twilight_animated()
#' 
#' @importFrom plotly add_text add_polygons
#' @export
add_twilight_animated <- \(
  map = add_TropicPolar(),
  time = Sys.time() + seq(from = 0, to = 60*60*24, length.out = 6L), 
  type = c('civil', 'nautical', 'astronomical'),
  sza,
  ...
) {
  
  force(time)
  
  subsolar <- subSolarPoint(time = time)
  sublunar <- subLunarPoint(time = time)
  
  terminator <- time |>
    lapply(FUN = terminatorPolygon) |>
    do.call(what = 'rbind.data.frame', args = _)
  
  twilight <- if (length(time) <= 6L) {
    time |>
      lapply(FUN = twilightPolygons, type = type, sza = sza) |>
      do.call(what = 'rbind.data.frame', args = _)
  } # else NULL
  
  map |>
    #add_text(
    #  data = rbind.data.frame(subsolar, sublunar),
    #  x = ~ lon, y = ~ lat, text = ~ text, frame = ~ time,
    #  hovertemplate = ~ hovertemplate_(label), # fine
    #  hoverlabel = ~ hoverlabel_(color) # \CRANpkg{plotly} messes up!!!
    #) |>
    add_text(
      data = subsolar,
      x = ~ lon, y = ~ lat, text = ~ text, frame = ~ time,
      hovertemplate = ~ hovertemplate_(label),
      hoverlabel = ~ hoverlabel_(color)
    ) |>
    add_text(
      data = sublunar,
      x = ~ lon, y = ~ lat, text = ~ text, frame = ~ time,
      hovertemplate = ~ hovertemplate_(label),
      hoverlabel = ~ hoverlabel_(color)
    ) |>
    add_polygons(
      data = terminator,
      x = ~ lon, y = ~ lat, frame = ~ time,
      line = list(width = 0), fillcolor = ~ fillcolor,
      hoverinfo = 'none'
    ) |>
    add_polygons(
      data = twilight, # use NULL to suppress
      x = ~ lon, y = ~ lat, frame = ~ time,
      line = list(width = 0), fillcolor = ~ fillcolor,
      hoverinfo = 'none'
    )
  
}

