
#' @title The Earth with Various Layers, Animated
#' 
#' @param time a \link[base]{POSIXct} scalar or \link[base]{vector}. Default value is ten (10) equidistant timepoints in the next 24 hours.
#' 
#' @param map an \CRANpkg{htmlwidgets} created by the function \link[plotly]{plot_geo}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @examples
#' globe_sunmoon_animated()
#' 
#' @importFrom plotly add_text add_polygons
#' @export
globe_sunmoon_animated <- \(
  time = seq(from = Sys.time(), to = Sys.time() + 60*60*24, length.out = 11L), 
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
  
  if (FALSE) {# computation burden!!
    twilight <- time |>
      lapply(FUN = \(i) {
        rbind.data.frame(
          twilightPolygon(time = i, type = 'civil'), 
          twilightPolygon(time = i, type = 'nautical'), 
          twilightPolygon(time = i, type = 'astronomical')
        )
      }) |>
      do.call(what = 'rbind.data.frame', args = _)
  }
  
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
    )# |>
  #add_polygons(
  # warning: number of items to replace is not a multiple of replacement length
  # not sure why..
  #  data = twilight,
  #  x = ~ lon, y = ~ lat, frame = ~ time,
  #  line = list(width = 0), fillcolor = ~ fillcolor,
  #  hoverinfo = 'none'
  #)
  
}

