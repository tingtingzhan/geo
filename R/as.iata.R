

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
distGeo_ <- \(sf, nm) {
  n <- nrow(sf)
  sq1 <- seq_len(n-1L)
  sq2 <- seq_len(n)[-1L]
  coords <- sf |>
    st_coordinates()
  ret <- distGeo(p1 = coords[sq1,], p2 = coords[sq2,]) # in meters
  nm <- eval(nm[[2L]], envir = sf) # `nm` must be one-sided formula
  names(ret) <- sprintf(fmt = '%s \u2708 %s', nm[sq1], nm[sq2])
  class(ret) <- 'distGeo'
  return(ret)
}


#' @export
format.distGeo <- function(x, ...) {
  n <- length(x) + 1L
  z <- character(length = n)
  z[1L] <- sprintf(fmt = '%s: %.0f miles', names(x[1L]), x[1L]/1609.34)
  z[n] <- sprintf(fmt = '%s: %.0f miles', names(x[n-1L]), x[n-1L]/1609.34)
  if (n > 2L) {
    for (i in 2:(n-1L)) {
      z[i] <- sprintf(
        fmt = '%s: %.0f miles\n%s: %.0f miles', 
        names(x[i-1L]), x[i-1L]/1609.34,
        names(x[i]), x[i]/1609.34
      )
    }
  }
  return(z)
}





#' @export
print.iata <- function(x, ...) {
  m_ <- airports_ip2location[x, , drop = FALSE] |>
    distGeo_(nm = ~ iata)
  ret = cbind(
    Miles = m_ / 1609.34, # ?grid::convertUnit does not have meter/miles conversion
    Kilometer = m_ / 1e3#,
    #Hour = (m_ / 1609.34) / 550 # average cruising speed, mile per hour
  )
  ret[] <- sprintf(fmt = '%.1f', ret)
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
    geo = layout_geo
) {
  
  ap <- airports_ip2location[x, , drop = FALSE]
  coords <- ap |>
    st_coordinates()
  col <- col |> toRGB()
  lon <- coords[,1L]
  lat <- coords[,2L]
  
  n <- dim(coords)[1L]
  if (n <= 1L) stop('wont happen')
  sq1 <- seq_len(n-1L)
  sq2 <- seq_len(n)[-1L]
  
  marker_text <- distGeo_(sf = ap, nm = ~ iata) |>
    format.distGeo() |>
    sprintf(fmt = '%s\n%s', ap$shortnm, . = _)
  
  map |>
    add_segments(
      x = lon[sq1], xend = lon[sq2],
      y = lat[sq1], yend = lat[sq2],
      line = list(color = col, width = 2)
    ) |> 
    add_markers(
      x = lon, y = lat, text = marker_text,
      marker = list(color = col),
      hoverinfo = 'text', 
      hoverlabel = list(
        #font = list(color = 'white'), # also not bad
        bgcolor = 'white', font = col, # prettier
        align = 'center', # seems not working consistently..
        bordercolor = col # default 'black' 
      )
    ) |> 
    layout(
      #title = NULL,
      showlegend = FALSE,
      hoverlabel = list(align = 'center'), # seems not working consistently..
      geo = geo
    )
  
}




