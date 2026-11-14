

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
  
  if (any(c('tz') %in% names(x))) stop()
  
  z <- x[['iata']] |>
    iata_int() |>
    merge(y = x, by = 'iata') # sf:::merge.sf
  
  z$departure <- mapply(
    FUN = as.POSIXct.default, SIMPLIFY = TRUE,
    #FUN = as.POSIXlt.character,
    x = z$departure, tz = z$tz
  )# |> 
  #  vapply(FUN = as.POSIXct.POSIXlt, FUN.VALUE = NA_real_, USE.NAMES = TRUE) # verified correct

  z$arrival <- mapply(
    FUN = as.POSIXct.default, SIMPLIFY = TRUE,
    x = z$arrival, tz = z$tz
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
  class(ret) <- 'iatalist'
  return(ret)
}


#' @importFrom lutz tz_lookup_coords
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
  coords <- z |>
    st_coordinates()
  z$tz <- tz_lookup_coords(
    lon = coords[,1L], lat = coords[,2L],
    method = 'accurate' # too slow for the whole of `airports_ip2location`
  )
  class(z) <- c('iata', class(z))
  return(z)
  
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
format.distGeo <- \(x, ...) {
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
print.iata <- \(x, ...) {
  
  m_ <- x |>
    distGeo_(nm = ~ iata)
  ret = cbind(
    Miles = m_ / 1609.34, # ?grid::convertUnit does not have meter/miles conversion
    Kilometer = m_ / 1e3#,
    #Hour = (m_ / 1609.34) / 550 # average cruising speed, mile per hour
  )
  ret[] <- sprintf(fmt = '%.1f', ret)
  print(ret, quote = FALSE, right = TRUE)
  cat('\n')
  
  if (all(c('departure', 'arrival') %in% names(x))) {
    x |>
      plot.iata() |>
      print()
  }
  
  return(invisible(sum(m_ / 1609.34)))
  
}








#' @export
print.iatalist <- \(x, ...) {
  
  x |>
    vapply(FUN = print.iata, ..., FUN.VALUE = NA_real_) |>
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
    map = globe_twilight()
) {
  
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
# @references 
# \url{https://plotly.com/r/lines-on-maps/}
#' @export
plot.iata <- \(x, ...) {
  
  if (all(c('departure', 'arrival') %notin% names(x))) {
    return(plot_iata_int(x, ...))
  }
  
  plot_iata_animated(x, ...)
  
}

#' @importFrom sf st_coordinates
iata_animated_data <- \(x) {
  
  tm1 <- x[['arrival']] |>
    as.POSIXlt(tz = 'UTC')
  tm2 <- x[['departure']] |>
    as.POSIXlt(tz = 'UTC')
  tm <- list(tm1, tm2) |>
    c(list(f = c)) |>
    do.call(what = Map, args = ) |>
    do.call(what = c, args = _) |>
    na.omit.POSIXlt()
  
  id <- attr(tm, which = 'na.action', exact = TRUE)
  
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
    tm = tm,
    time = tm |>
      as.POSIXlt(tz = 'UTC') |>
      format(usetz = TRUE)
  )
  
}



#' @importFrom plotly add_text
plot_iata_animated <- \(
  x, 
  map = globe_twilight_animated(time = d[['tm']]),
  ...
) {
  
  d <- iata_animated_data(x)
  
  plot_iata_int(x, map = map, ...) |>
    add_text(
      data = d,
      x = ~ jitter(lon), y = ~ jitter(lat), 
      text = ~ text, frame = ~ time,
      textfont = list(size = 20),
      hoverinfo = 'none'
    )

}


#' @importFrom plotly add_markers add_segments
#' @importFrom sf st_coordinates
#' @importFrom scales pal_hue
plot_iata_int <- \(
  x,
  map = globe_twilight(),
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
  
  marker_text <- distGeo_(sf = x, nm = ~ iata) |>
    format.distGeo() |>
    sprintf(fmt = '%s\n%s', x$shortnm, . = _)
  
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
    )
  
}




