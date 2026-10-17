
# first draft by Gemini !!!


#' @title Subsolar Point and Terminator Points at Current Time
#' 
#' @description
#' To find the latitude and longitude of the subsolar point and the day-night terminator.
#' 
#' 
#' @param time a \link[base]{POSIXct} object, default value is the current time on user's computer from the function \link[base]{Sys.time}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' The function [subsolarPoint()] finds the latitude and longitude of the subsolar point. 
#' 
#' @returns 
#' The function [subsolarPoint()] returns a \link[sf]{st_point} object.
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Solar_zenith_angle}
#' \url{https://en.wikipedia.org/wiki/Subsolar_point}
#' 
#' @author
#' Google Gemini \url{https://gemini.google.com}.
#' 
#' Tingting Zhan (for the function [moonEmoji()], and correcting a critical flaw in the function [terminatorPolygon()])
#' 
#' @examples
#' subsolarPoint()
#' 
#' @name terminator
#' @importFrom sf st_point
#' @export
subsolarPoint <- \(time = Sys.time(), ...) {
  
  time <- time |>
    as.POSIXlt(tz = 'UTC')
  
  doy <- time$yday + 1 # day of year
  hour <- time$hour + time$min/60 + time$sec/3600 # hour of day
  
  # Solar Declination (latitude where sun is directly overhead)
  # Simple approximation formula
  declination <- 23.44 * sin(2 * pi * (doy - 80) / 365.25)
  
  # Greenwich Hour Angle / Subsolar Longitude
  # The sun moves 15 degrees per hour; 12:00 UTC is roughly 0 degrees longitude
  solar_lon <- (12 - hour) * 15
  # Keep within -180 to 180 degrees
  solar_lon <- ((solar_lon + 180) %% 360) - 180
  
  return(st_point(x = c(solar_lon, declination)))
  
  
}


# library(leaflet)
# leaflet() |> addTiles() |> addPolygons(terminatorPolygon())
# # not yet



#' @rdname terminator
#' 
#' @param n \link[base]{integer} scalar, number of points, default value is `4L` (three points determine a polygon on the globe, while +/-180 longitude overlap)
#' 
#' @details
#' The function [terminatorPolygon()] finds the latitude and longitude of the points on the day-night terminator.
#' 
#' @returns 
#' The function [terminatorPolygon()] returns a \link[sf]{st_polygon} object.
#' 
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Terminator_(solar)}
#' 
#' @examples
#' terminatorPolygon()
#' 
#' @importFrom sf st_polygon
#' @export
terminatorPolygon <- \(time = Sys.time(), n = 4L, ...) {
  
  ss <- time |>
    subsolarPoint()
  sslon <- ss[1L] * pi / 180
  sslat <- ss[2L] * pi / 180
  
  # clockwise / counterclockwise determines which hemisphere is shaded in ?plotly::add_polygons !!!
  lon <- seq.int(from = pi, to = -pi, length.out = n) # terminator is a great circle!
  
  z <- cbind(
    lon = lon,
    lat = atan(-cos(lon - sslon) / tan(sslat))
  )  / pi * 180
  
  rbind(z, z[1L, ]) |> # to close the polygon
    list() |>
    st_polygon()
  
}



#' @rdname terminator
#' 
#' @details
#' The function [sublunarPoint()] finds the latitude and longitude of the sublunar point. 
#' 
#' @returns 
#' The function [sublunarPoint()] returns a \link[sf]{st_point} object.
#' 
#' @references
#' \url{https://www.timeanddate.com/worldclock/sunearth.html}
#' 
#' @examples
#' sublunarPoint()
#' 
#' @importFrom oce moonAngle siderealTime
#' @importFrom sf st_point
#' @export
sublunarPoint <- \(time = Sys.time(), ...) {
  # 1. Parse the target timestamp in strict UTC time
  target_time <- as.POSIXct(time, tz = "UTC")
  
  # 2. Extract geocentric Right Ascension and Declination 
  # Using long = 0, lat = 0 gives us the unprimed Greenwich baseline coordinates
  lunar_data <- moonAngle(t = target_time, longitude = 0, latitude = 0)
  
  # Latitude of the sublunar point matches the declination exactly
  sublunar_lat <- lunar_data$declination
  
  # 3. Calculate Greenwich Sidereal Time (GST) using the updated standalone function
  # Note: oce::siderealTime returns time in decimal hours (0 to 24)
  gst_hours <- siderealTime(target_time)
  gst_degrees <- gst_hours * 15 # Convert hours to degrees (360 degrees / 24 hours)
  
  # 4. Longitude is the Moon's Right Ascension minus Greenwich Sidereal Time
  sublunar_lon <- lunar_data$rightAscension - gst_degrees
  
  # Normalize longitude to the standard range of [-180, +180] degrees
  sublunar_lon <- ((sublunar_lon + 180) %% 360) - 180
  
  return(st_point(x = c(sublunar_lon, sublunar_lat)))
  
}

#' @rdname terminator
#' 
#' @details
#' The function [moonEmoji()] finds the moon phase.
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
#' @param sza \link[base]{double} scalar, the solar zenith angle (SZA) \eqn{\theta_z} in radius
#' 
#' @param max.n \link[base]{integer} scalar, maximum fineness for twilight (which is **not** a great circle!).  Default value is `1001L`
#' 
#' @param mc.cores \link[base]{integer} scalar, see the function \link[parallel]{mclapply}. Default value is `1L` on Windows, and \link[parallel]{detectCores} on Unix-like system.
#' 
#' @details
#' The function [twilightPolygon()] finds the latitude and longitude of the points on
#' \describe{
#' \item{Civil twilight}{when \eqn{\theta_z = \frac{90+6}{180}\cdot\pi}}
#' \item{Nautical twilight}{when \eqn{\theta_z = \frac{90+12}{180}\cdot\pi}}
#' \item{Astronomical twilight}{when \eqn{\theta_z = \frac{90+18}{180}\cdot\pi}}
#' }
#' 
#' @returns
#' The function [twilightPolygon()] returns a \link[sf]{st_polygon} object.
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Twilight}
#' 
#' @examples
#' twilightPolygon(sza = (90+12) * pi / 180, n = 201L)
#' 
#' @importFrom sf st_polygon
#' @importFrom rootSolve uniroot.all
#' @importFrom parallel mclapply detectCores 
# @importFrom parallel makeCluster stopCluster parLapply
#' @export
twilightPolygon <- \(
  time = Sys.time(), 
  sza,
  max.n = 1001L,
  mc.cores = switch(.Platform$OS.type, unix = detectCores(), 1L),
  ...
) {
  
  ss <- time |>
    subsolarPoint()
  sslon <- ss[1L] * pi / 180
  sslat <- ss[2L] * pi / 180
  
  # clockwise / counterclockwise determines which hemisphere is shaded in ?plotly::add_polygons !!!
  # twilight is **not** great circle!!
  # `lon` # \lambda in radius, \link[base]{vector}
  # must start with `sslon`, otherwise polygon may be messed up!
  lon <- seq(from = sslon, to = sslon - 2*pi, length.out = max.n)
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

  if (Sys.getenv('POSITRON') == '1') {
    
    # multi-core: correct way
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
    list() |>
    st_polygon()
  
}


