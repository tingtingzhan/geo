
#' @title The Earth with Various Layers, Animated
#' 
#' @param time a \link[base]{POSIXct} scalar or \link[base]{vector}. Default value is six (`6L`) equidistant timepoints in the next 24 hours.
#' 
#' @param map an \CRANpkg{htmlwidgets} created by the function \link[plotly]{plot_geo}. Default value is the return of the function [globe_TropicPolar()]
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @note
#' The function [globe_twilight_animated()] does **not** print twilight polygons, which is compute extensive, for more than six `time` points.
#' 
#' @examples
#' globe_twilight_animated()
#' 
#' @importFrom plotly add_text add_polygons
#' @export
globe_twilight_animated <- \(
  time = seq(from = Sys.time(), to = Sys.time() + 60*60*24, length.out = 6L), 
  map = globe_TropicPolar(),
  ...
) {
  
  subsolarlunar <- rbind.data.frame(
    subSolarPoint(time = time),
    subLunarPoint(time = time)
  )
  
  terminator <- time |>
    lapply(FUN = terminatorPolygon) |>
    do.call(what = 'rbind.data.frame', args = _)
  
  twilight <- if (length(time) <= 6L) { # FALSE && 
    time |>
      lapply(FUN = \(i) {
        rbind.data.frame(
          twilightPolygon(time = i, type = 'civil'), 
          twilightPolygon(time = i, type = 'nautical'), 
          twilightPolygon(time = i, type = 'astronomical')
        )
      }) |>
      do.call(what = 'rbind.data.frame', args = _)
  } # else NULL
  
  map |>
    add_text(
      data = subsolarlunar,
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

