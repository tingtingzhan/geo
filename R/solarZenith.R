

# @param sza \link[base]{double} scalar, the solar zenith angle (SZA) \eqn{\theta_z} in radius

solarZenithPolygon <- \(
  time = Sys.time(), 
  sza = pi/2, # sun at horizon
  n = if (abs(sza - pi/2) < .Machine$double.eps) 4L else 1001L, # only the terminator is a great circle!!
  ...
) {
  
  if (!is.double(sza) || length(sza) != 1L || is.na(sza)) stop('sza must be double scalar')
  
  if ((sza < 0) || (sza > pi)) stop('`sza` out of range')
  
  ss <- time |>
    subSolarPoint()
  sslon <- ss$lon/180*pi
  sslat <- ss$lat/180*pi
  
  # constants!
  cos_sza <- cos(sza)
  
  ha <- hour_angle_seq(sslat = sslat, sza = sza, length.out = n)
  
  # (sin(x)*sin(sslat) + cos(x)*cos(sslat)*cos(ha)) - cos(sza) # GOLD!!!
  # \url{https://en.wikipedia.org/wiki/Solar_zenith_angle} has analytical solution!!
  # ask Gemini: find analytical solution to \phi in Solar_zenith_angle
  R <- sqrt(sin(sslat)^2 + cos(sslat)^2 * cos(ha)^2)
  cos_sza_R <- cos_sza / R
  
  alpha <- atan2(
    y = sin(sslat), 
    x = cos(sslat) * cos(ha)
  )
  
  phi <- array(NA_real_, dim = c(2L, n))
  id1 <- (abs(cos_sza_R) <= 1)
  
  phi[,id1] <- (alpha[id1] + tcrossprod(
    acos(cos_sza_R[id1]),
    c(-1, 1)
  )) |>
    t.default() |> # dont forget!
    wrap_pi()
  phi[which(phi < -pi/2)] <- NA_real_
  phi[which(phi > pi/2)] <- NA_real_
  
  n_phi <- (!is.na(phi)) |>
    .colSums(m = 2L, n = n)
  
  if (all(n_phi == 1L)) { # hew!!
    
    lat <- phi[!is.na(phi)]
    
  } else {
    
    id2 <- (n_phi > 0L)
    ha <- ha[id2]
    ha <- c(ha, rev.default(ha))
    
    phi. <- phi[, id2, drop = FALSE] |>
      apply(MARGIN = 2L, FUN = identity, simplify = FALSE)
    
    lat <- c(
      phi. |> 
        vapply(FUN = min, na.rm = TRUE, FUN.VALUE = NA_real_),
      phi. |> 
        rev.default() |>
        vapply(FUN = max, na.rm = TRUE, FUN.VALUE = NA_real_)
    )
    
    if (xor(sslat > 0, sza < pi/2)) {
      # hahaha!
      ha <- rev.default(ha)
      lat <- rev.default(lat)
    }
    
  }
  
  z <- cbind(
    lon = (ha + sslon) |>
      wrap_pi(),
    lat = lat
  ) / pi * 180
  
  ret <- rbind(z, z[1L, ]) |> # to close the polygon
    as.data.frame.matrix()
  
  ret$time <- time |>
    as.POSIXlt(tz = 'UTC') |>
    format(usetz = TRUE)
  
  return(ret)
  
}





hour_angle_seq <- \(sslat, sza, ...) {
  
  # clockwise / counterclockwise determines which hemisphere is shaded in ?plotly::add_polygons
  
  z <- ifelse(
    test = sslat > 0,
    yes = 2*pi, # summer of Northern Hemisphere
    no = - 2*pi # winter of Northern Hemisphere
  )  |>
    seq(
      from = 0, # from sub-solar point
      to = _, 
      ...
    )
  
  if (sza >= pi/2) return(z)
  
  return(z + pi) # otherwise, from the opposite side of the globe!!
  
}





# @details
# The function [twilightPolygons()] finds the latitude and longitude of the borders of
# \describe{
# \item{\href{https://en.wikipedia.org/wiki/Twilight#Civil_twilight}{Civil twilight}}{when the solar zenith angle (SZA) \eqn{\theta_z \in (\frac{90}{180}\pi, \frac{90+6}{180}\pi)}, *enough natural light remains so that artificial light in towns and cities is not needed to see adequately*}
# \item{\href{https://en.wikipedia.org/wiki/Twilight#Nautical_twilight}{Nautical twilight}}{when the solar zenith angle \eqn{\theta_z \in (\frac{90+6}{180}\pi, \frac{90+12}{180}\pi)}, *the human eye finds it difficult, if not impossible, to discern traces of illumination near the sunset or sunrise point of the horizon (first light after nautical dawn but before civil dawn and nightfall after civil dusk but before nautical dusk)*}
# \item{\href{https://en.wikipedia.org/wiki/Twilight#Astronomical_twilight}{Astronomical twilight}}{when the solar zenith angle \eqn{\theta_z \in (\frac{90+12}{180}\pi, \frac{90+18}{180}\pi)}, *the faintest stars detectable by the naked eye (those of approximately the sixth magnitude) will become visible in the evening at astronomical dusk, and become invisible at astronomical dawn*}
# }
# @references
# \url{https://en.wikipedia.org/wiki/Twilight}
# \url{https://en.wikipedia.org/wiki/Terminator_(solar)}
twilightPolygons <- \(
  time = Sys.time(),
  sza = (90 + c(0, 6, 12, 18)) / 180 * pi,
  fillcolor = (50 - 1 + seq_along(sza)) |>
    sprintf(fmt = 'rgba(0, 0, 0, .0%d)'),
  ...
) {
  
  force(time)
  
  if (length(sza) != length(fillcolor)) {
    stop('`sza` and `fillcolor` must have same length')
  }
  
  mapply(
    FUN = \(sza, fillcolor) {
      z <- solarZenithPolygon(time = time, sza = sza, ...)
      z$fillcolor <- fillcolor
      return(z)
    }, 
    sza = sza, fillcolor = fillcolor, 
    SIMPLIFY = FALSE
  ) |>
    do.call(what = 'rbind.data.frame', args = _)
  
}

