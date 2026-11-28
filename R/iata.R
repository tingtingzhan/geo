

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
#' The function [iata()] returns a \link[base]{list} of `'iata'` objects.   
#' 
#' The `S3` class `'iata'` \link[base]{inherits} from the `S3` class \link[sf]{sf}.
#' 
#' @examples 
#' iata('NRT-HNL-YVR')
#' iata('CTU-PEK-ICN-JFK, EWR-IAH-LIM')
#' 
#' data.frame(
#'  iata = c('JFK', 'LAX', 'PPT'),
#'  departure = c('2027-04-14 17:30', '2027-04-14 23:00', NA),
#'  arrival = c(NA, '2027-04-14 20:55', '2027-04-15 4:10')
#' ) |>
#'  iata()
#' 
#' @name iata
#' @export
iata <- \(x) UseMethod(generic = 'iata')

#' @rdname iata
#' @method iata data.frame
#' @export
iata.data.frame <- \(x) {
  
  if (any(c('iata', 'departure', 'arrival') %notin% names(x))) stop()
  
  z <- x[['iata']] |>
    iata_int() |>
    merge(y = x, by = 'iata') # sf:::merge.sf

  fmt_tz <- \(x, tz) {
    as.POSIXlt.character(x = x, tz = tz) |>
      format.POSIXlt(usetz = TRUE)
  }
    
  z$departure_text <- mapply(FUN = fmt_tz, x = z[['departure']], tz = z[['tz']], SIMPLIFY = TRUE) |>
    sprintf(fmt = 'Departure\n%s')
  z$arrival_text <- mapply(FUN = fmt_tz, x = z[['arrival']], tz = z[['tz']], SIMPLIFY = TRUE) |>
    sprintf(fmt = 'Arrival\n%s')
  
  z$departure <- mapply(
    FUN = as.POSIXct.default, SIMPLIFY = TRUE,
    #FUN = as.POSIXlt.character,
    x = z[['departure']], tz = z[['tz']]
  )# |> 
  #  vapply(FUN = as.POSIXct.POSIXlt, FUN.VALUE = NA_real_, USE.NAMES = TRUE) # verified correct

  z$arrival <- mapply(
    FUN = as.POSIXct.default, SIMPLIFY = TRUE,
    x = z[['arrival']], tz = z[['tz']]
  )
  
  class(z) <- c('iata', class(z))
  return(z)
  
}



#' @rdname iata
#' @export
iata.character <- \(x) {
  ret <- x |>
    strsplit(split = ', ', fixed = TRUE) |>
    unlist(use.names = FALSE) |> 
    strsplit(split = '-', fixed = TRUE) |>
    lapply(FUN = iata_int)
  class(ret) <- c('iatalist', 'listof', 'list')
  return(ret)
}


#' @importFrom sf st_coordinates
iata_int <- \(x) {
  
  if (!is.character(x)) stop('input airport codes must be character')
  if (anyNA(x) || !all(nzchar(x))) stop('illegal input airport codes')
  if (length(x) < 2L) stop('a trip must have >=2 airports')
  
  id <- x |>
    toupper() |>
    match(table = airports_ip2location$iata, nomatch = NA_integer_)
  if (anyNA(id)) stop('must use IATA code')
  
  z <- airports_ip2location[id, , drop = FALSE]
  class(z) <- c('iata', class(z))
  return(z)
  
}



#' @importFrom geosphere distGeo
#' @importFrom sf st_coordinates
distGeo_ <- \(x, nm) {
  n <- nrow(x)
  sq1 <- seq_len(n-1L)
  sq2 <- seq_len(n)[-1L]
  coords <- x |>
    st_coordinates()
  ret <- distGeo(p1 = coords[sq1,], p2 = coords[sq2,]) # in meters
  nm <- eval(nm[[2L]], envir = x) # `nm` must be one-sided formula
  names(ret) <- sprintf(fmt = '%s \u2708 %s', nm[sq1], nm[sq2])
  class(ret) <- 'distGeo'
  return(ret)
}


#' @export
format.distGeo <- \(x, ...) {
  n <- length(x) + 1L
  x <- x / 1609.34 # in miles
  z <- character(length = n)
  z[1L] <- sprintf(fmt = '%s: %.0f miles', names(x[1L]), x[1L])
  z[n] <- sprintf(fmt = '%s: %.0f miles', names(x[n-1L]), x[n-1L])
  if (n > 2L) {
    for (i in 2:(n-1L)) {
      z[i] <- sprintf(
        fmt = '%s: %.0f miles\n%s: %.0f miles', 
        names(x[i-1L]), x[i-1L],
        names(x[i]), x[i]
      )
    }
  }
  return(z)
}



#' @export
format.iata <- \(x, fmt = '%.1f', ...) {
  m_ <- x |>
    distGeo_(nm = ~ iata)
  ret = cbind(
    Miles = m_ / 1609.34, # ?grid::convertUnit does not have meter/miles conversion
    Kilometer = m_ / 1e3
  )
  ret[] <- sprintf(fmt = fmt, ret)
  ret |> 
    noquote(right = TRUE)
}


#' @export
print.iata <- \(x, ...) {
  
  x |>
    format.iata() |>
    print()
  cat('\n')
  
  if (all(c('departure', 'arrival') %in% names(x))) {
    x |>
      plot.iata() |>
      print()
  }
  
  return(invisible())
  
}








#' @export
print.iatalist <- \(x, ...) {
  
  x |>
    lapply(FUN = print.iata, ...)
  
  x |>
    vapply(FUN = \(i) {
      z <- i |>
        distGeo_(nm = ~ iata) |>
        sum()
      z / 1609.34
    }, FUN.VALUE = NA_real_) |>
    sum() |> 
    sprintf(fmt = 'Total Mileage: %.1f') |> 
    cat()
  
  x |> 
    plot.iatalist() |> 
    print() # ?htmlwidgets:::print.htmlwidget
  
}







#' @importFrom scales pal_hue
#' @export
plot.iatalist <- \(
    x, 
    ..., 
    col = pal_hue()(n = length(x)),
    map = add_twilight()
) {
  
  p <- map
  for (i in seq_along(x)) {
    p <- plot.iata(x = x[[i]], map = p, col = col[i], ...)
  }
  return(p)
  
}




# @param map an htmlwidget of world map, default is the return of the function \link[plotly]{plot_geo}
# @param col \link[base]{character} scalar
# @references 
# \url{https://plotly.com/r/lines-on-maps/}
#' @export
plot.iata <- \(x, ...) {
  
  if (all(c('departure', 'arrival') %in% names(x))) {
    return(add_iata_animated(x = x, ...))
  }
  
  return(add_iata(x = x, ...))
  
}




#' @importFrom sf st_coordinates
iata_animated_data <- \(x) {
  
  tm <- list(x[['arrival']], x[['departure']]) |>
    lapply(FUN = as.POSIXlt.POSIXct, tz = 'UTC') |>
    c(list(f = c)) |>
    do.call(what = 'Map', args = ) |>
    do.call(what = 'c', args = _) |>
    na.omit.POSIXlt()
  
  id <- attr(tm, which = 'na.action', exact = TRUE)
  
  hovertext <- list(x[['arrival_text']], x[['departure_text']]) |>
    c(list(f = c)) |>
    do.call(what = 'Map', args = ) |>
    unname() |> # friendlier to human eyes
    do.call(what = 'c', args = _)# |>
    #na.omit()
  
  airplane <- c(
    '\U1f6ec', # Airplane Arrival
    '\U1f6eb' # Airplane Departure
  ) |>
    rep(times = nrow(x))
  
  coords <- x |>
    st_coordinates()
  lon <- coords[,1L] |>
    rep(each = 2L)
  lat <- coords[,2L] |>
    rep(each = 2L)
  
  data.frame(
    lon = lon[-id], lat = lat[-id], 
    text = airplane[-id],
    hovertext = hovertext[-id],
    tm = tm, # 'POSIXlt' in UTC, to be aligned with [add_twilight_frame()]
    color = 'royalblue', # to match the color of '\U1f6ec' and '\U1f6eb'
    time = tm |>
      format(usetz = TRUE) # to be aligned with [subSolarPoint()], etc.
  )
  
}



#' @importFrom plotly add_text
add_iata_animated <- \(
  map = add_twilight_frame(time = d[['tm']]),
  x, 
  ...
) {
  
  d <- iata_animated_data(x)
  
  map |>
    add_iata(x = x, ...) |>
    add_text(
      data = d,
      x = ~ jitter(lon), y = ~ jitter(lat), 
      text = ~ text, frame = ~ time,
      hovertext = ~ hovertext,
      hoverinfo = 'text', 
      hoverlabel = ~ list(bgcolor = 'white', font = color, bordercolor = color)
    )

}


#' @importFrom plotly add_markers add_segments
#' @importFrom sf st_coordinates
#' @importFrom scales pal_hue
add_iata <- \(
  map = add_twilight(),
  x,
  col = pal_hue()(n = 1L),
  ...
) {
  
  coords <- x |>
    st_coordinates()
  lon <- coords[,1L]
  lat <- coords[,2L]
  
  n <- dim(coords)[1L]
  if (n <= 1L) stop('wont happen')
  sq1 <- seq_len(n-1L)
  sq2 <- seq_len(n)[-1L]
  
  marker_text <- x |>
    distGeo_(nm = ~ iata) |>
    format.distGeo() |>
    sprintf(fmt = '%s\n%s', x$shortnm, . = _)
  
  map |>
    add_segments(
      x = lon[sq1], xend = lon[sq2],
      y = lat[sq1], yend = lat[sq2],
      line = list(color = col)
    ) |> 
    add_markers(
      x = lon, y = lat, text = marker_text,
      marker = list(color = col),
      hoverinfo = 'text', 
      hoverlabel = list(
        align = 'center',
        bgcolor = 'white', font = col,
        bordercolor = col # default 'black' 
      )
    )
  
}




