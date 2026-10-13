
# first draft by Gemini !!!


#' @title Subsolar Point and Terminator Points at Current Time
#' 
#' @param time a \link[base]{POSIXct} object, default value is `Sys.time()`
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @name terminator
#' @export
subsolarPoint <- \(time = Sys.time(), ...) {
  
  time <- time |>
    as.POSIXlt(tz = 'UTC')
  
  doy <- time$yday + 1 # day of year
  hour <- time$hour + time$min/60 + time$sec/3600 # hour of day
  
  # Solar Declination (latitude where sun is directly overhead)
  # Simple approximation formula (by Gemini)
  declination <- 23.44 * sin(2 * pi * (doy - 80) / 365.25)
  
  # Greenwich Hour Angle / Subsolar Longitude
  # The sun moves 15 degrees per hour; 12:00 UTC is roughly 0 degrees longitude (by Gemini)
  solar_lon <- (12 - hour) * 15
  # Keep within -180 to 180 degrees
  solar_lon <- ((solar_lon + 180) %% 360) - 180
  
  return(data.frame(lat = declination, lon = solar_lon))
}



#' @rdname terminator
#' 
#' @param n \link[base]{integer} scalar, default value is `4L`
#' 
#' @export
terminatorPoints <- \(time = Sys.time(), n = 4L, ...) {
  
  ss <- time |>
    subsolarPoint()
  
  # Convert degrees to radians for trigonometry (by Gemini)
  gplat_rad <- ss$lat * pi / 180
  gplon_rad <- ss$lon * pi / 180
  
  #longs <- seq.int(from = -180, to = 180, length.out = n) # Gemini original, wrong!!
  longs <- seq.int(from = 180, to = -180, length.out = n) 
  # clockwise / counterclockwise determines which hemisphere is shaded in ?plotly::add_polygons
  # **not** subsolar point and the opposite polar !!! 
  longs_rad <- longs * pi / 180
  
  # Spherical math to find latitude for each longitude step (by Gemini)
  ha <- longs_rad - gplon_rad
  lats_rad <- atan(-cos(ha) / tan(gplat_rad))
  lats <- lats_rad * 180 / pi
  
  data.frame(lat = lats, lon = longs)
  
}





