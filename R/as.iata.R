

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
#' Function [as.iata()] returns an object of S3 class `'iata'`, which is essentially
#' a \link[base]{list} of \link[base]{integer} \link[base]{vector}s.
#' 
#' @examples 
#' as.iata('NRT-HNL-YVR')
#' as.iata('CTU-PEK-ICN-JFK, EWR-IAH-LIM')
#' @keywords internal
#' @export
as.iata <- function(x) {
  if (!is.character(x)) stop('only accepts airport names as character')
  ret <- x |>
    strsplit(split = ', ', fixed = TRUE) |>
    unlist(use.names = FALSE) |> 
    strsplit(split = '-', fixed = TRUE) |>
    lapply(FUN = \(x) {
      if (!is.character(x) || length(x) < 2L || anyNA(x) || !all(nzchar(x))) stop('a trip must have >=2 airports')
      id <- match(x, table = airports_ip2location@data$iata, nomatch = NA_integer_)
      if (anyNA(id)) stop('must use IATA code')
      class(id) <- 'iata'
      return(id)
    })
  class(ret) <- 'iatalist'
  return(ret)
}

#' @title print.iata
#' 
#' @param x `'iata'`
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom geosphere distGeo
#' @export print.iata
#' @export
print.iata <- function(x, ...) {
  ap <- airports_ip2location[x, , drop = FALSE]
  n <- length(x)
  sq1 <- seq_len(n-1L)
  sq2 <- seq_len(n)[-1L]
  m_ <- distGeo(p1 = ap@coords[sq1,], p2 = ap@coords[sq2,]) # in meters
  ret = cbind(
    Miles = m_ / 1609.34, # ?grid::convertUnit does not have meter/miles conversion
    Kilometer = m_ / 1e3#,
    #Hour = (m_ / 1609.34) / 550 # average cruising speed, mile per hour
  )
  ret[] <- sprintf(fmt = '%.1f', ret)
  rownames(ret) <- sprintf(fmt = '%s \u2708 %s', ap@data$iata[sq1], ap@data$iata[sq2])
  print(ret, quote = FALSE, right = TRUE)
  cat('\n')
  return(invisible(sum(m_ / 1609.34)))
}


#' @title print.iatalist
#' 
#' @param x `'iatalist'`
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @export print.iatalist
#' @export
print.iatalist <- function(x, ...) {
  
  x |>
    vapply(FUN = print.iata, ..., FUN.VALUE = NA_real_) |> 
    sum() |> 
    sprintf(fmt = 'Total Mileage: %.1f') |> 
    cat()
  
  # I need to refine these functions some time
  #RoundTheWorld(x, airline = 'ANA')
  #RoundTheWorld(x, airline = 'SQ')
  
  x |> 
    plot.iatalist() |> 
    print() # ?htmlwidgets:::print.htmlwidget
  
}








#' @title Draw `'iatalist'` using \CRANpkg{plotly}
#' 
#' @description
#' ..
#' 
#' @param x an `'iatalist'`
#' 
#' @param ... ..
#' 
#' @param map an htmlwidget of world map, default is the return of function
#' \link[plotly]{plot_geo}
#' 
#' @references 
#' \url{https://plotly.com/r/lines-on-maps/}
#' 
#' @keywords internal
#' @export plot.iatalist
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




#' @title Draw `'iata'` using \CRANpkg{plotly}
#' 
#' @param x an `'iata'`
#' 
#' @param ... ..
#' 
#' @param map an htmlwidget of world map, default is the return of function
#' \link[plotly]{plot_geo}
#' 
#' @param col \link[base]{character} scalar
#' 
#' @param geo a \link[base]{list}, see \url{https://plotly.com/r/reference/layout/geo/} for detail
#' 
#' @keywords internal
#' @importFrom plotly plot_geo add_markers add_segments add_lines layout toRGB
#' @importFrom scales pal_hue
#' @export plot.iata
#' @export
plot.iata <- function(
    x,
    ..., 
    map = plot_geo(),
    col = pal_hue()(n = 1L),
    geo = list( # https://plotly.com/r/reference/layout/geo/
      resolution = 50, # 50 high resolution, 110 low resolution
      showland = TRUE, landcolor = toRGB('grey97'),
      showlakes = TRUE, lakecolor = toRGB('lightblue'),
      showrivers = TRUE, rivercolor = toRGB('lightblue'), riverwidth = .5,
      showcountries = TRUE, countrycolor = toRGB('grey50'), countrywidth = .5, 
      # showsubunits = TRUE, subunitcolor = toRGB('blue'), # state borders; not working, not sure why
      showocean = TRUE, oceancolor = toRGB('white'), coastlinecolor = toRGB('grey50'), coastlinewidth = .5,
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
  
  coords <- airports_ip2location[x, , drop = FALSE]@coords
  col <- col |> toRGB()
  lon <- coords[,1L]
  lat <- coords[,2L]
  nm <- rownames(coords)
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
      x = lon, y = lat, text = nm,
      marker = list(color = col),
      hoverinfo = 'text', 
      hoverlabel = list(
        font = list(
          color = 'white'
        ), 
        #bordercolor = 'white' # default 'black' 
        bordercolor = col
      )
    ) |> 
    layout(
      #title = NULL,
      showlegend = FALSE,
      geo = geo
    )
  
}




