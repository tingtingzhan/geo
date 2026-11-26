celestialDegree <- \(time = Sys.time(), ...) {
  
  time <- time |>
    as.POSIXlt(tz = 'UTC')
  
  # https://www.omnicalculator.com/everyday-life/sidereal-time
  # Greenwich Mean Sidereal Time (GMST): IAU 1982 standard
  
  Day <- as.numeric(time) / (60*60*24) + 
    jd_1970010100 - 
    jd_2000010112
  
  T_cen <- Day / (1e2 * c_Julian_yr) # the fraction of the century elapsed
  
  gmst <- c_gmst2000 + 
    c_siderealRotationDay * Day + 
    .000387933 * T_cen^2 - 
    T_cen^3 / 3871e4
  
  L <- c_L2000 + 
    36000.76983 * T_cen +
    .0003032 * T_cen^2
  
  # Mean Longitude of the Ascending Node (Omega)
  # 月球平升交点黄经
  # http://www.mat.uc.pt/~efemast/netscape/help/en/est_nut.htm
  omega <- c_omega2000 - 
    c_omegaRegression * T_cen + 
    0.0020754 * T_cen^2 + # instead of .0020708 (1991 ed.1)
    T_cen^3 / 467441 + # instead of 45e4 (1991 ed.1)
    (- T_cen^4 / 60616000)
  
  D  <- ((297.8501921 + 445267.1114034 * T_cen - 0.0018819 * T_cen^2) %% 360) * pi / 180
  M  <- ((357.5291092 + 35999.0502909 * T_cen - 0.0001536 * T_cen^2) %% 360) * pi / 180
  Mp <- ((134.9633964 + 477198.8675055 * T_cen + 0.0087414 * T_cen^2) %% 360) * pi / 180
  F  <- ((93.2720950 + 483202.0175233 * T_cen - 0.0036539 * T_cen^2) %% 360) * pi / 180
  
  omega_true <- omega - 
    1.4979 * sin(2 * (D - F)) - 
    0.1500 * sin(M) - 
    0.1226 * sin(2 * D) + 
    0.1176 * sin(2 * F) - 
    0.0801 * sin(2 * (Mp - F))
  
  i_moon <- i_moon_mean + 
    .145 * cos(2*(L - omega))
  
  return(list( # all in degree (not radius)
    gmst = gmst %% 360,
    obliquity = c_obliquity2000 - c_precessionSecular * T_cen,
    omega = omega %% 360,
    omega_true = omega_true %% 360,
    L = L,
    i_moon = i_moon
  ))
  
}




wrap180 <- \(x) {
  ((x + 180) %% 360) - 180
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
      wrap180(), 
    lat = declination / pi * 180
  ))
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

