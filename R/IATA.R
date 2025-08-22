

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
#' Function [IATA()] returns an object of S3 class `'IATA'`, which is essentially
#' a \link[base]{list} of \link[base]{integer} \link[base]{vector}s.
#' 
#' @examples 
#' IATA('NRT-HNL-YVR')
#' IATA('CTU-PEK-ICN-JFK, EWR-IAH-LIM')
#' @keywords internal
#' @export
IATA <- function(x) {
  if (!is.character(x)) stop('only accepts airport names as character')
  ret <- x |>
    strsplit(split = ', ', fixed = TRUE) |>
    unlist(use.names = FALSE) |> 
    strsplit(split = '-', fixed = TRUE) |>
    lapply(FUN = \(x) {
      if (!is.character(x) || length(x) < 2L || anyNA(x) || !all(nzchar(x))) stop('a trip must have >=2 airports')
      id <- match(x, table = airports_ip2location@data$iata, nomatch = NA_integer_)
      if (anyNA(id)) stop('must use IATA code')
      return(id)
    })
  class(ret) <- 'IATA'
  return(ret)
}


#' @importFrom geosphere distGeo
print_IATA_ <- function(x) {
  # `x` is one-trip 'IATA' (as \link[base]{vector}, not \link[base]{list}!!)
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



#' @export
print.IATA <- function(x, ...) {
  
  x |>
    vapply(FUN = print_IATA_, FUN.VALUE = NA_real_) |> 
    sum() |> 
    sprintf(fmt = 'Total Mileage: %.1f') |> 
    cat()
  
  # I need to refine these functions some time
  #RoundTheWorld(x, airline = 'ANA')
  #RoundTheWorld(x, airline = 'SQ')
  
  x |> 
    plot.IATA() |> 
    print() # ?htmlwidgets:::print.htmlwidget
  
}








#' @title Draw `'IATA'` using \CRANpkg{plotly}
#' 
#' @description
#' ..
#' 
#' @param x `'IATA'`
#' 
#' @param ... ..
#' 
#' @references 
#' \url{https://plotly.com/r/lines-on-maps/}
#' 
#' @keywords internal
#' @importFrom plotly plot_geo add_markers add_segments add_lines layout toRGB
#' @importFrom scales pal_hue
#' @export plot.IATA
#' @export
plot.IATA <- function(x, ...) {
  x |>
    lapply(FUN = \(i) airports_ip2location[i, , drop = FALSE]@coords) |>
    turn_coords(...)
}



# @param coords \link[base]{list} of \link[base]{matrix}
turn_coords <- function(coords, ...) {
  
  p0 <- plot_geo()
  
  col <- coords |>
    length() |> 
    pal_hue()() |>
    toRGB()
  
  p1 <- p0
  for (i in seq_along(coords)) {
    lon <- coords[[i]][,1L]
    lat <- coords[[i]][,2L]
    nm <- rownames(coords[[i]])
    n <- dim(coords[[i]])[1L]
    if (n <= 1L) stop('wont allow')
    
    p1 <- p1 |>
      add_segments(
        x = lon[seq_len(n-1L)], xend = lon[2:n],
        y = lat[seq_len(n-1L)], yend = lat[2:n],
        line = list(color = col[i], width = 2),
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
        marker = list(color = col[i]),
        hoverinfo = 'text', 
        hoverlabel = list(
          font = list(
            color = 'white'
          ), 
          #bordercolor = 'white' # default 'black' 
          bordercolor = col[i]
        )
      )
  }
  
  # https://plotly.com/r/reference/layout/geo/
  geo <- list(
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
  
  return(layout(
    p = p1,
    #title = NULL,
    showlegend = FALSE,
    geo = geo
  ))
  
}







