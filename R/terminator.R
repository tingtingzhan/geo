


#' @title Subsolar/Sublunar Point, Terminator and  Twilight Zones
#' 
#' @description
#' To find the latitude and longitude of the subsolar/sublunar point, the day-night terminator and the various twilight zones at the current date and time.
#' 
#' 
#' @param time a \link[base]{POSIXct} object, default value is the current date and time on user's computer from the function \link[base]{Sys.time}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' The function [subSolarPoint()] finds the latitude and longitude of the subsolar point. 
#' 
#' @returns 
#' The function [subSolarPoint()] returns a \link[base]{data.frame} (may be converted to \link[crosstalk]{SharedData}).
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Solar_zenith_angle}
#' \url{https://en.wikipedia.org/wiki/Subsolar_point}
#' 
#' @examples
#' subSolarPoint()
#' 
#' @name terminator
#' @export
subSolarPoint <- \(time = Sys.time(), ...) {
  
  # already vectorized!!
  
  time <- time |>
    as.POSIXlt(tz = 'UTC')
  
  doy <- time$yday + 1 # day of year
  hour <- time$hour + time$min/60 + time$sec/3600 # hour of day
  
  # Solar Declination (latitude where sun is directly overhead)
  # Cooper (1969) doi:10.1016/0038-092X(69)90047-4
  # same as ?solrad::Declination
  declination <- 23.45 * sin(2 * pi * (284+doy) / 365.25)
  
  # Greenwich Hour Angle / Subsolar Longitude
  # The sun moves 15 degrees per hour; 12:00 UTC is roughly 0 degrees longitude
  solar_lon <- (12 - hour) * 15
  # Keep within -180 to 180 degrees
  solar_lon <- ((solar_lon + 180) %% 360) - 180
  
  # \CRANpkg{crosstalk} as engine
  return(data.frame(
    lon = solar_lon,
    lat = declination,
    time = time |>
      format(usetz = TRUE),
    text = '\u2600\ufe0f', # sun emoji
    label = 'Sun',
    color = 'darkorange'
  ))
  
}




#' @rdname terminator
#' 
#' @param n \link[base]{integer} scalar.
#' \itemize{
#' \item{Number of points in [terminatorPolygon()], default value is `4L`.  Three points determine a polygon on the globe, while +/-180 longitude overlap.}
#' \item{Maximum fineness for [twilightPolygon()], default value is `1001L`.  Twilight zone borders are **not** great circles!}
#' }
#' 
#' @details
#' The function [terminatorPolygon()] finds the latitude and longitude of the points on the day-night terminator.
#' 
#' @returns 
#' The function [terminatorPolygon()] returns a \link[base]{data.frame} (may be converted to \link[crosstalk]{SharedData}).
#' 
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Terminator_(solar)}
#' 
#' @examples
#' terminatorPolygon()
#' 
#' @export
terminatorPolygon <- \(time = Sys.time(), n = 4L, ...) {
  
  ss <- time |>
    subSolarPoint()
  sslon <- ss$lon * pi / 180
  sslat <- ss$lat * pi / 180
  
  # clockwise / counterclockwise determines which hemisphere is shaded in ?plotly::add_polygons !!!
  lon <- seq.int(from = pi, to = -pi, length.out = n) # terminator is a great circle!
  
  z <- cbind(
    lon = lon,
    lat = atan(-cos(lon - sslon) / tan(sslat))
  )  / pi * 180
  
  rbind(z, z[1L, ]) |> # to close the polygon
    as.data.frame.matrix()
  
}



#' @rdname terminator
#' 
#' @details
#' The function [subLunarPoint()] finds the latitude and longitude of the sublunar point. 
#' 
#' @returns 
#' The function [subLunarPoint()] returns a \link[base]{data.frame} (may be converted to \link[crosstalk]{SharedData}).
#' 
#' @references
#' \url{https://www.timeanddate.com/worldclock/sunearth.html}
#' 
#' @examples
#' subLunarPoint()
#' 
#' @importFrom oce moonAngle siderealTime
#' @export
subLunarPoint <- \(time = Sys.time(), ...) {
  
  # already vectorized!!
  
  # 1. Parse the target timestamp in strict UTC time
  time <- time |>
    as.POSIXct(tz = 'UTC')
  
  # 2. Extract geocentric Right Ascension and Declination 
  # Using long = 0, lat = 0 gives us the unprimed Greenwich baseline coordinates
  lunar_data <- moonAngle(t = time, longitude = 0, latitude = 0)
  
  # Latitude of the sublunar point matches the declination exactly
  sublunar_lat <- lunar_data$declination
  
  # 3. Calculate Greenwich Sidereal Time (GST) using the updated standalone function
  # Note: oce::siderealTime returns time in decimal hours (0 to 24)
  gst_hours <- siderealTime(time)
  gst_degrees <- gst_hours * 15 # Convert hours to degrees (360 degrees / 24 hours)
  
  # 4. Longitude is the Moon's Right Ascension minus Greenwich Sidereal Time
  sublunar_lon <- lunar_data$rightAscension - gst_degrees
  
  # Normalize longitude to the standard range of [-180, +180] degrees
  sublunar_lon <- ((sublunar_lon + 180) %% 360) - 180
  
  # \CRANpkg{crosstalk} as engine
  return(data.frame(
    lon = sublunar_lon,
    lat = sublunar_lat,
    time = time |>
      format(usetz = TRUE),
    text = moonEmoji(time = time),
    label = 'Moon',
    color = 'cornflowerblue'
  ))

}

#' @rdname terminator
#' 
#' @details
#' The function [moonEmoji()] finds the moon phase.
#' @returns 
#' The function [moonEmoji()] returns a \link[base]{character} scalar or \link[base]{vector}.
#' 
#' @examples
#' moonEmoji()
#' 
#' @importFrom lunar lunar.phase
#' @export
moonEmoji <- \(time = Sys.time(), ...) {
  time |>
    as.Date() |>
    lunar.phase(...) |> # next: find the hour `shift`
    cut.default(
      #breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), # ?lunar::lunar.phase
      breaks = seq.int(from = 0, to = 2*pi, length.out = 10L), # for the 9 emoji's
      labels = c('\U1f311', '\U1f312', '\U1f313', '\U1f314', '\U1f315', '\U1f316', '\U1f317', '\U1f318', '\U1f311'),
      include.lowest = TRUE, right = TRUE
    )
}




#' @rdname terminator
#' 
#' @param sza \link[base]{double} scalar, the solar zenith angle (SZA) \eqn{\theta_z} in radius, from \eqn{\pi/2} (the sun at horizon) to \eqn{\pi} (local midnight)
#' 
#' @param mc.cores \link[base]{integer} scalar, see the function \link[parallel]{mclapply}. Default value is `1L` on Windows, and \link[parallel]{detectCores} on Unix-like system.
#' 
#' @details
#' The function [twilightPolygon()] finds the latitude and longitude of the borders of
#' \describe{
#' \item{\href{https://en.wikipedia.org/wiki/Twilight#Civil_twilight}{Civil twilight}}{when the solar zenith angle (SZA) \eqn{\theta_z \in (\frac{90}{180}\pi, \frac{90+6}{180}\pi)}, *enough natural light remains so that artificial light in towns and cities is not needed to see adequately*}
#' \item{\href{https://en.wikipedia.org/wiki/Twilight#Nautical_twilight}{Nautical twilight}}{when the solar zenith angle \eqn{\theta_z \in (\frac{90+6}{180}\pi, \frac{90+12}{180}\pi)}, *the human eye finds it difficult, if not impossible, to discern traces of illumination near the sunset or sunrise point of the horizon (first light after nautical dawn but before civil dawn and nightfall after civil dusk but before nautical dusk)*}
#' \item{\href{https://en.wikipedia.org/wiki/Twilight#Astronomical_twilight}{Astronomical twilight}}{when the solar zenith angle \eqn{\theta_z \in (\frac{90+12}{180}\pi, \frac{90+18}{180}\pi)}, *the faintest stars detectable by the naked eye (those of approximately the sixth magnitude) will become visible in the evening at astronomical dusk, and become invisible at astronomical dawn*}
#' }
#' 
#' @returns
#' The function [twilightPolygon()] returns a \link[base]{data.frame} (may be converted to \link[crosstalk]{SharedData}).
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Twilight}
#' 
#' @examples
#' twilightPolygon(sza = (90+12) * pi / 180, n = 201L)
#' 
#' @importFrom rootSolve uniroot.all
#' @importFrom parallel mclapply detectCores 
# @importFrom parallel makeCluster stopCluster parLapply
#' @export
twilightPolygon <- \(
  time = Sys.time(), 
  sza = stop('must provide `sza`'),
  n = 1001L,
  mc.cores = switch(.Platform$OS.type, unix = detectCores(), 1L),
  ...
) {
  
  if (!is.double(sza) || length(sza) != 1L || is.na(sza)) stop('sza must be double scalar')
  
  if ((sza < pi/2) || (sza > pi)) stop('`sza` out of range')
  
  ss <- time |>
    subSolarPoint()
  sslon <- ss$lon * pi / 180
  sslat <- ss$lat * pi / 180
  
  # twilight is **not** great circle!!
  # `lon` # \lambda in radius, \link[base]{vector}
  # must start with `sslon`, otherwise polygon may be messed up!
  lon <- seq(from = sslon, to = sslon - 2*pi, length.out = n)
  # Normalize longitude to (-pi, pi)
  lon <- ((lon + pi) %% (2*pi)) - pi
  
  fn <- \(x, lon) {
    (sin(x)*sin(sslat) + cos(x)*cos(sslat)*cos(lon - sslon)) - cos(sza)
  } 
  # `fn` does not have analytical solution
  # `fn` is not monotone, i.e., **not** ?stats::uniroot
  # `fn` may have multiple local minimum, i.e., **not** ?base::optimize
  
  unr_fn <- \(i) {
    uniroot.all( # already super fast!!!
      f = fn, lon = i,
      lower = -pi/2, upper = pi/2
    )
  }

  if ((.Platform$OS.type == 'windows') || 
      (Sys.getenv('POSITRON') == '1')) {
    
    # multi-core: POSITRON
    #cl <- makeCluster(mc.cores, type = 'PSOCK')
    #z <- lon |>
    #  parLapply(cl = cl, fun = unr_fn)
    #stopCluster(cl)
    # end of multi-core
    
    # not using multi-core is actually faster
    z <- lon |>
      lapply(FUN = unr_fn)
    
  } else {
    # does improve speed in RStudio!
    z <- lon |>
      mclapply(FUN = unr_fn, mc.cores = mc.cores)
  }
  
  nz <- lengths(z)
  
  lat <- z |>
    unlist(use.names = FALSE) # whatever!!!
  
  if (all(nz == 1L)) { # hew!!
    # do nothing!!
  } else {
    id <- (nz > 0L)
    lon <- c(lon[id], rev.default(lon[id]))
    lat <- c(
      z[id] |> 
        vapply(FUN = `[`, 1L, FUN.VALUE = NA_real_),
      z[id] |> 
        rev.default() |>
        vapply(FUN = \(i) i[length(i)], FUN.VALUE = NA_real_)
    )
  }
  
  z <- cbind(lon, lat) / pi * 180
  
  rbind(z, z[1L, ]) |> # to close the polygon
    as.data.frame.matrix()
  
}


