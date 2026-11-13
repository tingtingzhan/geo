
#' @title The Earth with Various Layers, Animated
#' 
#' @param time a \link[base]{POSIXct} object, see the function [subSolarPoint()]
#' 
#' @param n \link[base]{integer} scalar, number of frames
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @examples
#' globe_twilight_animated()
#' 
#' @importFrom plotly add_text add_polygons
#' @export
globe_twilight_animated <- \(
  time = Sys.time(), 
  n = 11L,
  ...
) {
  
  # +/- 12hours
  intv <- time + c(-1, 1) * 60*60*24/2
  tm <- seq(from = intv[1L], to = intv[2L], length.out = n)
  
  subsolarlunar <- rbind.data.frame(
    subSolarPoint(time = tm),
    subLunarPoint(time = tm)
  )
  
  terminator <- tm |>
    lapply(FUN = terminatorPolygon) |>
    do.call(what = 'rbind.data.frame', args = _)
  
  if (FALSE) {# computation burden!!
    twilight <- tm |>
      lapply(FUN = \(i) {
        rbind.data.frame(
          twilightPolygon(time = i, type = 'civil'), 
          twilightPolygon(time = i, type = 'nautical'), 
          twilightPolygon(time = i, type = 'astronomical')
        )
      }) |>
      do.call(what = 'rbind.data.frame', args = _)
  }
  
  globe_TropicPolar() |>
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

