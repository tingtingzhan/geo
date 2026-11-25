


#' @title Subsolar/Sublunar Point, Terminator and Twilight Zones
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
#' The function [subSolarPoint()] returns a \link[base]{data.frame}.
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Subsolar_point}
#' 
#' @name terminator
#' @export
subSolarPoint <- \(time = Sys.time(), ...) {
  
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
  solar_lon <- ((solar_lon + 180) %% 360) - 180
  
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
#' \item{Number of points in [terminatorPolygon()] and [eclipticPolygon()], default value is `4L`.  Three points determine a polygon on the globe, while +/-180 longitude overlap.}
#' \item{Maximum fineness for [twilightPolygon()], default value is `1001L`.  Twilight zone borders are **not** great circles!}
#' }
#' 
#' @details
#' The function [terminatorPolygon()] finds the latitude and longitude of the points on the day-night terminator.
#' 
#' @returns 
#' The function [terminatorPolygon()] returns a \link[base]{data.frame}.
#' 
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Terminator_(solar)}
#' 
#' @export
terminatorPolygon <- \(time = Sys.time(), n = 4L, ...) {
  
  time <- time |>
    as.POSIXlt(tz = 'UTC')
  
  ss <- time |>
    subSolarPoint()
  sslon <- ss$lon/180*pi
  sslat <- ss$lat/180*pi
  
  ha <- hour_angle_seq(sslat = sslat, length.out = n)
  
  z <- cbind(
    lon = ((ha + sslon + pi) %% (2*pi)) - pi,
    lat = atan(-cos(ha) / tan(sslat))
  )  / pi * 180
  
  ret <- rbind(z, z[1L, ]) |> # to close the polygon
    as.data.frame.matrix()
  ret$time <- time |>
    format(usetz = TRUE)
  ret$fillcolor <- 'rgba(0, 0, 0, .05)'
  return(ret)
  
}




#' @rdname terminator
#' 
#' @details
#' The function [subLunarPoint()] finds the latitude and longitude of the sublunar point. 
#' 
#' @returns 
#' The function [subLunarPoint()] returns a \link[base]{data.frame}.
#' 
#' @references
#' \url{https://www.timeanddate.com/worldclock/sunearth.html}
#' 
#' @importFrom oce moonAngle siderealTime
#' @export
subLunarPoint <- \(time = Sys.time(), ...) {
  
  time <- time |>
    as.POSIXct(tz = 'UTC')
  
  mA <- moonAngle(t = time, longitude = 0, latitude = 0)
  # Important!!
  # mA$illuminatedFraction: does not tell wane vs. wax!!
  
  sublunar_lon <- celestialDegree(time) |> 
    with(expr = {
      ((mA$rightAscension - gmst + 180) %% 360) - 180
    })
  
  return(data.frame(
    lon = sublunar_lon,
    lat = mA$declination,
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
#' @param type \link[base]{character} scalar
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
#' The function [twilightPolygon()] returns a \link[base]{data.frame}.
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Twilight}
#' 
#' @importFrom rootSolve uniroot.all
#' @importFrom parallel mclapply detectCores 
# @importFrom parallel makeCluster stopCluster parLapply
#' @export
twilightPolygon <- \(
  time = Sys.time(), 
  type = c('civil', 'nautical', 'astronomical'),
  sza = NULL,
  n = 1001L, # twilight is **not** great circle!!
  mc.cores = detectCores(),
  ...
) {
  
  if (missing(sza) || !length(sza)) {
    type <- match.arg(type)
    sza <- (switch(type, civil = 6, nautical = 12, astronomical = 18) + 90) / 180 * pi
  } else {
    type <- NULL
  }
  
  if (!is.double(sza) || length(sza) != 1L || is.na(sza)) stop('sza must be double scalar')
  
  if ((sza < pi/2) || (sza > pi)) stop('`sza` out of range')
  
  ss <- time |>
    subSolarPoint()
  sslon <- ss$lon/180*pi
  sslat <- ss$lat/180*pi
  
  # constants!
  sin_sslat <- sin(sslat)
  cos_sslat <- cos(sslat)
  cos_sza <- cos(sza)
  
  ha <- hour_angle_seq(sslat = sslat, length.out = n)

  unr_fn <- \(i) {
    uniroot.all( # look inside!
      f = \(x, ha) {
        (sin(x)*sin_sslat + cos(x)*cos_sslat*cos(ha)) - cos_sza  
        # \url{https://en.wikipedia.org/wiki/Solar_zenith_angle}
        # does **not** have analytical solution
        # is not monotone, i.e., can **not** use ?stats::uniroot
        # may have multiple local minimum, i.e., can **not** use ?base::optimize
      }, 
      ha = i,
      lower = -pi/2, upper = pi/2
    ) |>
      sort.int() # needed as of \CRANpkg{rootSolve} 2023-09-21
  }

  if (Sys.getenv('POSITRON') == '1') {
    
    # multi-core: POSITRON
    #cl <- makeCluster(mc.cores, type = 'PSOCK')
    #z <- lon |>
    #  parLapply(cl = cl, fun = unr_fn)
    #stopCluster(cl)
    # end of multi-core
    
    # not using multi-core is actually faster
    z <- ha |>
      lapply(FUN = unr_fn)
    
  } else {
    # does improve speed in RStudio!
    z <- ha |>
      mclapply(FUN = unr_fn, mc.cores = mc.cores)
  }
  
  nz <- lengths(z)
  
  lat <- z |>
    unlist(use.names = FALSE) # whatever!!!
  
  if (all(nz == 1L)) { # hew!!
    # do nothing!!
  } else { # when (sslat < 0)
    id <- (nz > 0L)
    ha <- c(ha[id], rev.default(ha[id]))
    lat <- c(
      z[id] |> 
        vapply(FUN = `[`, 1L, FUN.VALUE = NA_real_),
      z[id] |> 
        rev.default() |>
        vapply(FUN = \(i) i[length(i)], FUN.VALUE = NA_real_)
    )
    if (sslat > 0) {
      # summer time
      # hah! tzh does not know why!!!
      ha <- rev.default(ha)
      lat <- rev.default(lat)
    }
  }
  
  z <- cbind(
    lon = ((ha + sslon + pi) %% (2*pi)) - pi,
    lat = lat
  ) / pi * 180
  
  ret <- rbind(z, z[1L, ]) |> # to close the polygon
    as.data.frame.matrix()
  ret$time <- time |>
    as.POSIXlt(tz = 'UTC') |>
    format(usetz = TRUE)

  ret$fillcolor <- if (length(type)) {
    switch(
      EXPR = type, 
      civil = 'rgba(0, 0, 0, .051)',
      nautical = 'rgba(0, 0, 0, .052)',
      astronomical = 'rgba(0, 0, 0, .053)'
    )
  } else 'rgba(0, 0, 0, .051)'
  
  return(ret)
  
}








#' @rdname terminator
#' @export
eclipticPolygon <- \(time = Sys.time(), n = 4L, ...) {
  
  z <- celestialDegree(time) |>
    with(expr = {
      ecliptic2geo(
        lambda = seq.int(from = 0, to = 2*pi, length.out = n),
        eps = obliquity / 180 * pi,
        gmst = gmst
      )
    })
  
  z$color <- 'darkorange'
  z$time <- time |>
    as.POSIXlt(tz = 'UTC') |>
    format(usetz = TRUE)
  return(z)
  
}



#' @rdname terminator
#' @export
LunarOrbitalPolygon <- \(time = Sys.time(), n = 4L, ...) {
  
  # Argument of latitude u (0° = Ascending Node, 180° = Descending Node)
  u_deg <- seq.int(from = 0, to = 360, length.out = n)
  u_rad <- u_deg / 180 * pi
  
  # Lunar orbit in ecliptic coordinates
  moon_beta <- asin(sin(i_moon / 180 * pi) * sin(u_rad))
  # print(moon_beta) # debugging only
  
  z <- celestialDegree(time) |>
    with(expr = {
      moon_lambda <- omega / 180 * pi + 
        atan2(
          cos(i_moon / 180 * pi) * sin(u_rad), 
          cos(u_rad)
        )
      # print(moon_lambda) # debugging only
      ecliptic2geo(
        lambda = moon_lambda, 
        beta = moon_beta, 
        eps = obliquity / 180 * pi, 
        gmst = gmst)
    })
  
  z$u <- u_deg
  
  # Label Ascending & Descending Nodes along the lunar path
  # (not feasible with `n=4L`)
  #z$label <- ""
  #z$label[z$u == 0]   <- "\u260a Ascending Node"
  #z$label[z$u == 180] <- "\u260b Descending Node"
  
  z$time <- time |>
    as.POSIXlt(tz = 'UTC') |>
    format(usetz = TRUE)
  z$color <- 'cornflowerblue'
  
  return(z)
  
}






twilightPolygons <- \(
  time = Sys.time(),
  type = c('civil', 'nautical', 'astronomical'),
  sza = NULL,
  ...
) {
  
  force(time)
  
  z <- if (!missing(sza) && length(sza)) {
    sza |>
      lapply(FUN = \(i) twilightPolygon(time = time, sza = i, ...)) 
  } else {
    type |>
      lapply(FUN = \(i) twilightPolygon(time = time, type = i, ...))
  }
  
  z |>
    do.call(what = 'rbind.data.frame', args = _)
  
}



hour_angle_seq <- \(sslat, ...) {
  # clockwise / counterclockwise determines which hemisphere is shaded in ?plotly::add_polygons
  ifelse(
    test = sslat > 0,
    yes = 2*pi, # summer of Northern Hemisphere
    no = - 2*pi # winter of Northern Hemisphere
  )  |>
    seq(from = 0, to = _, ...)
}



# Ecliptic (lambda, beta) -> Geographic (lon, lat)
ecliptic2geo <- \(
  lambda, # radius
  beta = 0, # radius
  eps, # radius
  gmst # degree
) {
  
  # Equatorial Declination (Latitude)
  declination <- asin(sin(beta) * cos(eps) + cos(beta) * sin(eps) * sin(lambda))
  
  # Equatorial Right Ascension
  ra <- atan2(
    sin(lambda) * cos(eps) - tan(beta) * sin(eps),
    cos(lambda)
  )
  
  return(data.frame(
    lon = ((ra / pi * 180 - gmst + 180) %% 360) - 180, 
    lat = declination / pi * 180
  ))
}


celestialDegree <- \(time, ...) {
  
  # by Gemini!
  
  time <- time |>
    as.POSIXlt(tz = 'UTC')
  
  # https://www.omnicalculator.com/everyday-life/sidereal-time
  
  D <- as.numeric(time) / (60*60*24) + jd_1970010100 - jd_2000010112
  
  T_cen <- D / (1e2 * c_Julian_yr) # the fraction of the century elapsed
  
  # Greenwich Mean Sidereal Time (GMST): IAU 1982 standard
  gmst <- c_gmst2000 + 
    c_siderealRotationDay * D + 
    .000387933 * T_cen^2 - 
    T_cen^3 / 3871e4
  
  # Mean Longitude of the Ascending Node (Omega)
  # http://www.mat.uc.pt/~efemast/netscape/help/en/est_nut.htm
  omega <- c_omega2000 - 
    c_omegaRegression * T_cen + 
    .0020708 * T_cen^2 + 
    T_cen^3 / 45e4
  
  return(list( # all in degree (not radius)
    gmst = gmst %% 360,
    obliquity = c_obliquity2000 - c_precessionSecular * T_cen, # at `time`
    omega = omega %% 360
  ))
  
}


