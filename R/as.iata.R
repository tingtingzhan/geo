

#' @title One or More Air Travel Trips using IATA codes
#' 
#' @description 
#' 
#' One or more air travel trips using International Air Transport Association (IATA) airport codes.
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector}
#' 
#' @returns 
#' 
#' The function [as.iata()] returns an object of S3 class `'iata'`, which is essentially a \link[base]{list} of \link[base]{integer} \link[base]{vector}s.
#' 
#' @examples 
#' as.iata('NRT-HNL-YVR')
#' as.iata('CTU-PEK-ICN-JFK, EWR-IAH-LIM')
#' @export
as.iata <- function(x) {
  if (!is.character(x)) stop('only accepts airport names as character')
  ret <- x |>
    strsplit(split = ', ', fixed = TRUE) |>
    unlist(use.names = FALSE) |> 
    strsplit(split = '-', fixed = TRUE) |>
    lapply(FUN = \(x) {
      if (!is.character(x) || length(x) < 2L || anyNA(x) || !all(nzchar(x))) {
        stop('a trip must have >=2 airports')
      } 
      id <- x |>
        toupper() |>
        match(table = airports_ip2location$iata, nomatch = NA_integer_)
      if (anyNA(id)) stop('must use IATA code')
      class(id) <- 'iata'
      return(id)
    })
  class(ret) <- 'iatalist'
  return(ret)
}







#' @importFrom geosphere distGeo
#' @importFrom sf st_coordinates
#' @export
print.iata <- function(x, ...) {
  ap <- airports_ip2location[x, , drop = FALSE]
  n <- length(x)
  sq1 <- seq_len(n-1L)
  sq2 <- seq_len(n)[-1L]
  coords <- ap |>
    st_coordinates()
  m_ <- distGeo(p1 = coords[sq1,], p2 = coords[sq2,]) # in meters
  ret = cbind(
    Miles = m_ / 1609.34, # ?grid::convertUnit does not have meter/miles conversion
    Kilometer = m_ / 1e3#,
    #Hour = (m_ / 1609.34) / 550 # average cruising speed, mile per hour
  )
  ret[] <- sprintf(fmt = '%.1f', ret)
  rownames(ret) <- sprintf(fmt = '%s \u2708 %s', ap$iata[sq1], ap$iata[sq2])
  print(ret, quote = FALSE, right = TRUE)
  cat('\n')
  return(invisible(sum(m_ / 1609.34)))
}



#' @export
print.iatalist <- function(x, ...) {
  
  x |>
    vapply(FUN = print.iata, ..., FUN.VALUE = NA_real_) |> 
    sum() |> 
    sprintf(fmt = 'Total Mileage: %.1f') |> 
    cat()
  
  x |> 
    plot.iatalist() |> 
    print() # ?htmlwidgets:::print.htmlwidget
  
}








#' @export
plot.iatalist <- function(x, ..., map = plot_geo()) {
  
  n <- x |> 
    length()
  col <- n |> 
    pal_hue()()
  
  p <- map
  for (i in seq_len(n)) {
    p <- plot.iata(x = x[[i]], map = p, col = col[i], ...)
  }
  return(p)
  
}




# @param map an htmlwidget of world map, default is the return of the function \link[plotly]{plot_geo}
# @param col \link[base]{character} scalar
# @param geo a \link[base]{list}, see \url{https://plotly.com/r/reference/layout/geo/} for detail
# @references 
# \url{https://plotly.com/r/lines-on-maps/}
#' @importFrom plotly plot_geo add_markers add_segments add_lines layout toRGB
#' @importFrom scales pal_hue
#' @importFrom sf st_coordinates
#' @export
plot.iata <- function(
    x,
    ..., 
    map = plot_geo(),
    col = pal_hue()(n = 1L),
    geo = list( # https://plotly.com/r/reference/layout/geo/
      resolution = 50, # 50 high resolution, 110 low resolution
      framewidth = .7, framecolor = toRGB('grey80'), # outer frame of the earth
      showland = TRUE, landcolor = toRGB('linen'),
      showocean = TRUE, oceancolor = toRGB('aliceblue'), coastlinecolor = toRGB('peachpuff'), coastlinewidth = .5,
      showlakes = TRUE, lakecolor = toRGB('lightblue'),
      showrivers = TRUE, rivercolor = toRGB('lightblue'), riverwidth = .5,
      showcountries = TRUE, countrycolor = toRGB('peachpuff'), countrywidth = .7, 
      # showsubunits = TRUE, subunitcolor = toRGB('blue'), # state borders; not working, not sure why
      lonaxis = list(showgrid = TRUE, gridcolor = toRGB('gray80'), gridwidth = .5),
      lataxis = list(showgrid = TRUE, gridcolor = toRGB('gray80'), gridwidth = .5),
      projection = list(
        type = 'orthographic',
        rotation = list(
          # roll = 0 # default 0, roll of rotational axis of Earth
          lon = -100, lat = 40#, # let USA face user
          # 'mean' of longitude is *not* easy to define!!
          # mean of latitude is easy
        )
      )
    )
) {
  
  ap <- airports_ip2location[x, , drop = FALSE]
  coords <- ap |>
    st_coordinates()
  col <- col |> toRGB()
  lon <- coords[,1L]
  lat <- coords[,2L]
  n <- dim(coords)[1L]
  if (n <= 1L) stop('wont happen')
  
  map |>
    add_segments(
      x = lon[seq_len(n-1L)], xend = lon[2:n],
      y = lat[seq_len(n-1L)], yend = lat[2:n],
      line = list(color = col, width = 2),
      #hoverinfo = 'none'
      text = rep('abc', n-1L), hoverinfo = 'text'
      # https://stackoverflow.com/questions/63458372/plotly-add-segment-with-tooltip-along-entire-segment
      # https://github.com/plotly/plotly.R/issues/1832#issuecomment-675721763
      # TL;DR: plotly cannot do this, as of Sep 2024
    ) |> 
    add_markers(
      # `add_markers` after `add_segments` !!
      # it seems `hoverinfo` overwrites!!
      x = lon, y = lat, text = ap$shortnm,
      marker = list(color = col),
      hoverinfo = 'text', 
      hoverlabel = list(
        font = list(color = 'white'),
        bordercolor = col # default 'black' 
      )
    ) |> 
    layout(
      #title = NULL,
      showlegend = FALSE,
      geo = geo
    )
  
}




