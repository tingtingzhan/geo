celestialDegree <- \(time = Sys.time(), ...) {
  
  time <- time |>
    as.POSIXlt(tz = 'UTC')
  
  # https://www.omnicalculator.com/everyday-life/sidereal-time
  # Greenwich Mean Sidereal Time (GMST): IAU 1982 standard
  
  Day <- as.numeric(time) / (60*60*24) + 
    2440587.5 - # Julian Date of January 1, 1970, at 00:00:00 UTC
    2451545.0 # Julian date of January 1, 2000, at 12:00 (noon)
  
  T_cen <- Day / (1e2 * 365.25) # the fraction of the century elapsed
  
  gmst <- 280.46061837 + 
    360.98564736629 * Day + 
    .000387933 * T_cen^2 - 
    T_cen^3 / 3871e4
  
  # Sun's Geometric Mean Longitude
  L <- 280.46646 + 
    36000.76983 * T_cen +
    .0003032 * T_cen^2
  L_r <- L / 180 * pi
  
  # Mean Longitude of the Ascending Node (Omega)
  # 月球平升交点黄经
  # http://www.mat.uc.pt/~efemast/netscape/help/en/est_nut.htm
  omega_mean <- 125.0445479 + 
    (-1934.1362891) * T_cen + 
    0.0020754 * T_cen^2 + # instead of .0020708 (1991 ed.1)
    T_cen^3 / 467441 + # instead of 45e4 (1991 ed.1)
    (- T_cen^4 / 60616000)
  
  D  <- (297.8501921 + 445267.1114034 * T_cen - 0.0018819 * T_cen^2) * pi / 180
  # mean anomaly (M)
  M  <- (357.5291092 + 35999.0502909 * T_cen - 0.0001536 * T_cen^2) * pi / 180
  Mp <- (134.9633964 + 477198.8675055 * T_cen + 0.0087414 * T_cen^2) * pi / 180
  F  <- (93.2720950 + 483202.0175233 * T_cen - 0.0036539 * T_cen^2) * pi / 180
  
  # Sun's Equation of Center (C) & True Longitude (sun_true_lon)
  C <- sin(M) * (
    1.914602 - 
      0.004817 * T_cen -
      0.000014 * T_cen^2
  ) +
    sin(2 * M) * (0.019993 - 0.000101 * T_cen) +
    sin(3 * M) * 0.000289
  sun_true_lon <- L + C
  
  omega <- omega_mean - 
    1.4979 * sin(2 * (D - F)) - 
    0.1500 * sin(M) - 
    0.1226 * sin(2 * D) + 
    0.1176 * sin(2 * F) - 
    0.0801 * sin(2 * (Mp - F))
  omega_r <- omega / 180 * pi
  
  obliquity <- 23.439291 - # instead of 23.43928
    0.0130042 * T_cen - # instead of 0.0130102
    1.64e-7 * T_cen^2 + 
    5.04e-7 * T_cen^3 +
    0.00256 * cos(omega_r)
  eps <- obliquity / 180 * pi
  
  # Apparent Longitude (lambda)
  sun_lambda <- sun_true_lon - 0.00569 - 0.00478 * sin(omega_r)
  sun_lambda_r <- sun_lambda / 180 * pi
  # Solar Declination (delta) in degrees
  sun_delta_r <- asin(sin(eps) * sin(sun_lambda_r))
  sun_delta <- sun_delta_r / pi * 180
  # Right Ascension (alpha) in degrees [0, 360)
  alpha_r <- atan2(cos(eps) * sin(sun_lambda_r), cos(sun_lambda_r))
  #alpha_deg <- rad2deg(alpha_rad) %% 360
  sun_lon <- alpha_r / pi * 180 - gmst
  
  # Mean longitude of the Moon (Lp)
  Lp <- 218.3165 + 
    481267.8813 * T_cen
  Lp_r <- Lp / 180 * pi
  
  i_moon <- 5.145396 + 
    0.1460 * cos(2 * (D - F)) -
    0.0045 * cos(M) +
    0.0156 * cos(2 * F) -
    0.0110 * cos(2 * D) +
    0.0080 * cos(2 * (Mp - F))
  
  # https://en.wikipedia.org/wiki/Astronomical_nutation
  d_psi <- (
    -17.20 * sin(omega_r) - 
      1.32 * sin(2 * L_r) - 
      0.23 * sin(2 * Lp_r) + 
      0.21 * sin(2 * omega_r)
  ) / 3600 # Dividing by 3,600 converts arcseconds into degrees
  
  # Greenwich Apparent Sidereal Time
  gast <- gmst + d_psi * cos(eps)
  
  return(list(
    gmst = gmst, # degree
    obliquity = obliquity, eps = eps, # degree/radius
    omega = omega, omega_r = omega_r, # degree/radius
    sun_lon = sun_lon, sun_delta = sun_delta,
    L = L, L_r = L_r, # degree/radius
    d_psi = d_psi, # degree
    gast = gast, # degree
    i_moon = i_moon, i_moon_r = i_moon / 180 * pi # degree/radius
  ))
  
}


.wrap <- \(x, a) {
  # @param a positive \link[base]{double} scalar
  x[] <- ((x + a) %% (2*a)) - a
  return(x)
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
    lon = (ra / pi * 180 - gmst) |>
      .wrap(a = 180), 
    lat = declination / pi * 180
  ))
}


