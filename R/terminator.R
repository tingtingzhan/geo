


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
  solar_lon <- ((12 - hour) * 15) |>
    wrap180()
  
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
      (mA$rightAscension - gmst) |>
        wrap180()
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
#' The function [moonEmoji()] returns a \link[base]{factor}.
#' 
#' @importFrom lunar lunar.phase
#' @export
moonEmoji <- \(time = Sys.time(), ...) {
  
  time |>
    as.Date() |>
    lunar.phase(...) |> # next: find the hour `shift`
    cut.default(
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
#' @export
#' @export
twilightPolygon <- \(
  time = Sys.time(), 
  type = c('civil', 'nautical', 'astronomical'),
  sza = NULL,
  n = 1001L, # twilight is **not** great circle!!
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
  cos_sza <- cos(sza) # compute intensive
  # sza \in (pi/2, pi)
  # therefore, cos_sza < 0
  
  ha <- hour_angle_seq(sslat = sslat, length.out = n)
  
  # (sin(x)*sin(sslat) + cos(x)*cos(sslat)*cos(ha)) - cos(sza) # GOLD!!!
  # \url{https://en.wikipedia.org/wiki/Solar_zenith_angle} has analytical solution!!
  # ask Gemini: find analytical solution to \phi in Solar_zenith_angle
  R <- sqrt(sin(sslat)^2 + cos(sslat)^2 * cos(ha)^2)
  cos_sza_R <- cos_sza / R
  # cos_sza_R < 0
  alpha <- atan2(
    y = sin(sslat), 
    x = cos(sslat) * cos(ha)
  ) # atan(tan(sslat)/cos(ha)) is wrong!!
  # see ?atan2 
  
  z <- replicate(n = length(ha), expr = numeric(), simplify = FALSE)
  id <- (abs(cos_sza_R) <= 1)
  
  phi <- (alpha[id] + tcrossprod(
    acos(cos_sza_R[id]),
    c(-1, 1)
  )) |>
    wrap_pi()
  phi[phi < -pi/2] <- NA_real_
  phi[phi > pi/2] <- NA_real_
  z[id] <- phi |>
    apply(MARGIN = 1L, FUN = \(i) {
      i[!is.na(i)] # faster than ?stats:::na.omit.default
      #base::sort.int() # is very slow!!
    }, simplify = FALSE)
  
  nz <- lengths(z)
  
  if (all(nz == 1L)) { # hew!!
    
    lat <- z |>
      unlist(use.names = FALSE)
    
  } else { # when (sslat < 0)
    
    id <- (nz > 0L)
    ha <- c(ha[id], rev.default(ha[id]))
    lat <- c(
      z[id] |> 
        vapply(FUN = min, FUN.VALUE = NA_real_),
      z[id] |> 
        rev.default() |>
        vapply(FUN = max, FUN.VALUE = NA_real_)
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




lunarOrbitPolygon <- \(time = Sys.time(), n = 4L, ...) {
  
  u <- seq.int(from = 0, to = 2*pi, length.out = n)
  
  z <- celestialDegree(time) |>
    with(expr = {
      
      moon_beta <- asin(sin(i_moon / 180 * pi) * sin(u))
      # print(moon_beta) # debugging only
      
      moon_lambda <- omega / 180 * pi + 
        atan2(
          cos(i_moon / 180 * pi) * sin(u), 
          cos(u)
        )
      # print(moon_lambda) # debugging only
      
      ecliptic2geo(
        lambda = moon_lambda, 
        beta = moon_beta, 
        eps = obliquity / 180 * pi, 
        gmst = gmst)
      
    })
  
  z$time <- time |>
    as.POSIXlt(tz = 'UTC') |>
    format(usetz = TRUE)
  z$color <- 'cornflowerblue'
  
  return(z)
  
}















eclipticEquator_lon <- \(time = Sys.time()) {
  
  z <- celestialDegree(time) |>
    with(expr = {
      d_psi <- (-17.20 * sin(omega) - 1.32 * sin(2 * L)) / 3600 # Nutation in longitude (deg)
      gast <- (gmst + d_psi * cos(obliquity / 180 * pi)) %% 360
      list(
        gast = gast,
        obliquity = obliquity
      )
    })
  
  return(data.frame(
    time = time |>
      as.POSIXlt(tz = 'UTC') |>
      format(usetz = TRUE),
    lon = c(
      - z$gast, # Ascending node (March Equinox / 0° Aries)
      - z$gast + 180 # Descending node (September Equinox)
    ) |>
      wrap180(), 
    color = 'darkorange',
    incl = z$obliquity |>
      sprintf(fmt = '\u9ec4\u8d64\u4ea4\u89d2 %.3f\u00b0'),
    text = c('Ascending', 'Descending')
  ))
}



lunarEquator_lon <- \(time = Sys.time()) {
  
  z <- celestialDegree(time) |>
    with(expr = {
      
      omega_r <- omega * pi / 180
      eps_r   <- obliquity * pi / 180
      i_r     <- i_moon * pi / 180
      
      d_psi <- (-17.20 * sin(omega_r) - 1.32 * sin(2 * L)) / 3600
      gast  <- (gmst + d_psi * cos(eps_r)) %% 360
      
      # 黄道坐标系下的白道法向量 n_ecl
      nx_ecl <-  sin(i_r) * sin(omega_r)
      ny_ecl <- -sin(i_r) * cos(omega_r)
      nz_ecl <-  cos(i_r)
      
      # 绕 X 轴旋转 eps_r 转换到地心赤道坐标系下的白道法向量 n_eq
      nx <- nx_ecl
      ny <- ny_ecl * cos(eps_r) - nz_ecl * sin(eps_r)
      nz <- ny_ecl * sin(eps_r) + nz_ecl * cos(eps_r)
      
      # 赤道面法向量为 (0, 0, 1)
      # 白道在赤道面上的升交点向量 = (0, 0, 1) × (nx, ny, nz) = (-ny, nx, 0)
      ra_moon_asc_deg <- (atan2(nx, -ny) * 180 / pi) %% 360
      
      lon <- c(
        ra_moon_asc_deg - gast, # Lunar Ascending Node on Equator
        ra_moon_asc_deg - gast + 180  # Lunar Descending Node on Equator
      ) |>
        wrap180()
      
      # Inclination of the Moon's orbit directly to Earth's equator (ranges ~18.3° to ~28.6°)
      lunarEquator_incl <- acos(cos(eps_r) * cos(i_r) - sin(eps_r) * sin(i_r) * cos(omega_r)) * 180 / pi
      
      list(
        lon = lon,
        lunarEquator_incl = lunarEquator_incl
      )
      
    })
  
  return(data.frame(
    time = time |>
      as.POSIXlt(tz = 'UTC') |>
      format(usetz = TRUE),
    lon = z$lon,
    color = 'cornflowerblue',
    incl = z$lunarEquator_incl |>
      sprintf(fmt = '\u767d\u8d64\u4ea4\u89d2 %.3f\u00b0'),
    text = c('Ascending', 'Descending')
  ))

}




# intersection of the ecliptic and the lunar orbit
lunarNodePoint <- \(time = Sys.time()) {
  
  tmp <- celestialDegree(time)
  
  z <- tmp |>
    with(expr = {
      ecliptic2geo(
        lambda = c(
          omega_true, # ascending_node
          (omega_true + 180) %% 360 # descending_node
        ) / 180 * pi,
        eps = obliquity / 180 * pi,
        gmst = gmst
      )
    })
  
  return(data.frame(
    time = time |>
      as.POSIXlt(tz = 'UTC') |>
      format(usetz = TRUE),
    z,
    color = 'darkolivegreen',
    incl = tmp$i_moon |>
      sprintf(fmt = '\u9ec4\u767d\u4ea4\u89d2 %.3f\u00b0'),
    text = c('Ascending', 'Descending')
  ))
  
}



