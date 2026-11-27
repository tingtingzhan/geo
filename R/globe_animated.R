
#' @title The Earth with Various Layers, Animated
#' 
#' @param map an \CRANpkg{htmlwidgets} created by the function \link[plotly]{plot_geo}. Default value is the return of the function [add_TropicPolar()]
#' 
#' @param time a \link[base]{POSIXct} scalar or \link[base]{vector}. Default value is six (`6L`) equidistant timepoints in the next 24 hours.
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
  ...
) {
  
  force(time)
  
  subsolar <- subSolarPoint(time = time)
  sublunar <- subLunarPoint(time = time)
  
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
    #add_text(
    #  data = rbind.data.frame(subsolar, sublunar),
    #  x = ~ lon, y = ~ lat, text = ~ text, frame = ~ time,
    #  name = ~ label,
    #  hovertemplate = '(%{lon:.3f}\u00b0, %{lat:.3f}\u00b0)', # fine
    #  hoverlabel = ~ hoverlabel_(color) # \CRANpkg{plotly} messes up!!!
    #) |>
    add_text(
      data = subsolar,
      x = ~ lon, y = ~ lat, text = ~ text, frame = ~ time,
      name = 'Sun', # why not working?
      # name = ~ label, # why not working either?
      hovertemplate = '(%{lon:.3f}\u00b0, %{lat:.3f}\u00b0)',
      hoverlabel = ~ hoverlabel_(color)
    ) |>
    add_text(
      data = sublunar,
      x = ~ lon, y = ~ lat, text = ~ text, frame = ~ time,
      name = 'Moon', # why not working?
      hovertemplate = '(%{lon:.3f}\u00b0, %{lat:.3f}\u00b0)',
      hoverlabel = ~ hoverlabel_(color)
    ) |>
    add_paths( # ecliptic
      data = ecliptic,
      x = ~ lon, y = ~ lat, frame = ~ time,
      line = ~ list(color = color, dash = 'dot', width = .5),
      hoverinfo = 'none'
    ) |>
    add_paths( # lunar orbit
      data = lunarOrbit,
      x = ~ lon, y = ~ lat, frame = ~ time,
      line = ~ list(color = color, dash = 'dot', width = .5),
      hoverinfo = 'none'
    ) |>
    add_polygons(
      data = twilight, # use NULL to suppress
      x = ~ lon, y = ~ lat, frame = ~ time,
      line = list(width = 0), fillcolor = ~ fillcolor,
      hoverinfo = 'none'
    )
  
}

