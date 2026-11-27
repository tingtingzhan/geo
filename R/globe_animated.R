
#' @title The Earth with Various Layers, Animated
#' 
#' @param map an \CRANpkg{htmlwidgets} created by the function \link[plotly]{plot_geo}. Default value is the return of the function [add_TropicPolar()]
#' 
#' @param time a \link[base]{POSIXct} scalar or \link[base]{vector}. Default value is six (`6L`) equidistant time points in the next 24 hours.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @examples
#' add_twilight_animated()
#' 
#' @importFrom plotly add_text add_polygons
#' @export
add_twilight_animated <- \(
  map = add_TropicPolar(),
  time = Sys.time() + seq(from = 0, to = 60*60*24, length.out = 6L), 
  ...
) {
  
  force(time)
  
  ecliptic <- time |>
    lapply(FUN = eclipticPolygon) |>
    do.call(what = 'rbind.data.frame', args = _)
  
  lunarOrbit <- time |>
    lapply(FUN = lunarOrbitPolygon) |>
    do.call(what = 'rbind.data.frame', args = _)
  
  twilight <- if (length(time) <= 6L) {
    time |>
      lapply(FUN = twilightPolygons) |>
      do.call(what = 'rbind.data.frame', args = _)
  } # else NULL
  
  map |>
    add_text( # Sub-Solor Point
      data = subSolarPoint(time = time),
      x = ~ lon, y = ~ lat, text = ~ text, 
      frame = ~ time,
      customdata = ~ label,
      hovertemplate = '%{customdata}: (%{lon:.3f}\u00b0, %{lat:.3f}\u00b0)<extra></extra>',
      hoverlabel = ~ list(bgcolor = 'white', font = color, bordercolor = color)
    ) |>
    add_text( # Sub-Lunar Point
      data = subLunarPoint(time = time),
      x = ~ lon, y = ~ lat, text = ~ text, 
      frame = ~ time,
      customdata = ~ label,
      hovertemplate = '%{customdata}: (%{lon:.3f}\u00b0, %{lat:.3f}\u00b0)<extra></extra>',
      hoverlabel = ~ list(bgcolor = 'white', font = color, bordercolor = color)
    ) |>
    add_paths( # ecliptic
      data = ecliptic,
      x = ~ lon, y = ~ lat, 
      frame = ~ time,
      line = ~ list(color = color, dash = 'dot', width = .5),
      hoverinfo = 'none'
    ) |>
    add_paths( # lunar orbit
      data = lunarOrbit,
      x = ~ lon, y = ~ lat, 
      frame = ~ time,
      line = ~ list(color = color, dash = 'dot', width = .5),
      hoverinfo = 'none'
    ) |>
    add_polygons(
      data = twilight, # use NULL to suppress
      x = ~ lon, y = ~ lat, 
      frame = ~ time,
      line = list(width = 0), fillcolor = ~ fillcolor,
      hoverinfo = 'none'
    )
  
}

