



# @references
# \url{https://en.wikipedia.org/wiki/Subsolar_point}
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



# @references
# \url{https://www.timeanddate.com/worldclock/sunearth.html}
# 
#' @importFrom oce moonAngle siderealTime
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




#' @importFrom lunar lunar.phase
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








eclipticPolygon <- \(time = Sys.time(), n = 4L, ...) {
  
  z <- celestialDegree(time) |>
    with(expr = {
      ecliptic2geo(
        lambda = seq.int(from = 0, to = 2*pi, length.out = n),
        eps = eps,
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
        eps = eps,
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
      #gast <- (gmst + d_psi * cos(eps)) %% 360
      gast <- gmst + d_psi * cos(eps)
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
      i_r     <- i_moon * pi / 180
      
      d_psi <- (-17.20 * sin(omega_r) - 1.32 * sin(2 * L)) / 3600
      #gast  <- (gmst + d_psi * cos(eps)) %% 360
      gast  <- gmst + d_psi * cos(eps)
      
      # 黄道坐标系下的白道法向量 n_ecl
      nx_ecl <-  sin(i_r) * sin(omega_r)
      ny_ecl <- -sin(i_r) * cos(omega_r)
      nz_ecl <-  cos(i_r)
      
      # 绕 X 轴旋转 eps 转换到地心赤道坐标系下的白道法向量 n_eq
      nx <- nx_ecl
      ny <- ny_ecl * cos(eps) - nz_ecl * sin(eps)
      nz <- ny_ecl * sin(eps) + nz_ecl * cos(eps)
      
      # 赤道面法向量为 (0, 0, 1)
      # 白道在赤道面上的升交点向量 = (0, 0, 1) × (nx, ny, nz) = (-ny, nx, 0)
      #ra_moon_asc_deg <- (atan2(nx, -ny) * 180 / pi) %% 360
      ra_moon_asc_deg <- atan2(nx, -ny) * 180 / pi
      
      #lon <- c(
      #  ra_moon_asc_deg - gast, # Lunar Ascending Node on Equator
      #  ra_moon_asc_deg - gast + 180  # Lunar Descending Node on Equator
      #) |>
      #  wrap180()
      lon <- (ra_moon_asc_deg - gast + c(0, 180)) |>
        wrap180()
      
      # Inclination of the Moon's orbit directly to Earth's equator (ranges ~18.3° to ~28.6°)
      lunarEquator_incl <- acos(cos(eps) * cos(i_r) - sin(eps) * sin(i_r) * cos(omega_r)) * 180 / pi
      
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
        #lambda = c(
        #  omega, # ascending_node
        #  (omega + 180) %% 360 # descending_node
        #) / 180 * pi,
        lambda = (omega + c(0, 180)) / 180 * pi,
        eps = eps,
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



