
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
#' @references
#' \url{https://en.wikipedia.org/wiki/Subsolar_point}
#' 
#' @author
#' Google Gemini \url{https://gemini.google.com}.
#' 
#' Tingting Zhan (for the function [moonEmoji()], and correcting a critical flaw in the function [terminatorPoints()])
#' 
#' @examples
#' subsolarPoint()
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
#' @param n \link[base]{integer} scalar, number of points, default value is `4L` (three points determine a circle on the globe, while +/-180 longitude overlap)
#' 
#' @details
#' The function [terminatorPoints()] finds the latitude and longitude of the points on the day-night terminator.
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Terminator_(solar)}
#' 
#' @examples
#' terminatorPoints()
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


#' @rdname terminator
#' 
#' @details
#' The function [sublunarPoint()] finds the latitude and longitude of the sublunar point. 
#' 
#' @references
#' \url{https://www.timeanddate.com/worldclock/sunearth.html}
#' 
#' @examples
#' sublunarPoint()
#' 
#' @importFrom oce moonAngle siderealTime
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
  
  data.frame(lat  = sublunar_lat, lon = sublunar_lon)
  
}

#' @rdname terminator
#' 
#' @param date a \link[base]{Date} object, default value is the current time on user's computer from the function \link[base]{Sys.Date}
#' 
#' @details
#' The function [moonEmoji()] finds the moon phase.
#' 
#' @examples
#' moonEmoji()
#' 
#' @importFrom lunar lunar.phase
#' @export
moonEmoji <- \(date = Sys.Date(), ...) {
  date |>
    lunar.phase(...) |>
    cut.default(
      #breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), # ?lunar::lunar.phase
      breaks = seq.int(from = 0, to = 2*pi, length.out = 10L), # for the 9 emoji's
      labels = c('\U1f311', '\U1f312', '\U1f313', '\U1f314', '\U1f315', '\U1f316', '\U1f317', '\U1f318', '\U1f311'),
      include.lowest = TRUE, right = TRUE
    )
}

