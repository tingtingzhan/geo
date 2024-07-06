


RoundTheWorld <- function(x, airline = c('ANA', 'SQ')) {
  # `x` is 'IATA'
  airline <- match.arg(airline)
  # https://roundtheworld.staralliance.com/staralliance/en/round-the-world
  # ANA policy https://www.ana.co.jp/en/us/amc/partner-flight-awards/around-the-world/
  # SQ policy https://www.singaporeair.com/en_UK/us/ppsclub-krisflyer/use-miles/redeem-miles/star-alliance-round-the-world-award/
  # LH policy https://www.lufthansa.com/au/en/local-page/star-alliance-au
  # Air Canada https://thepointsguy.com/guide/aeroplan-routing-stopover-rules/
  
  nx <- length(x)
  not_stop <- if (nx == 1L) 'both' else c('head', rep('none', times = nx - 2L), 'tail')
  
  if (airline == 'ANA') {
    # stop('ANA must be same direction')
    if (!all(duplicated.default(unlist(longitudeDirection(x), use.names = FALSE))[-1L])) return(invisible())
  }
  
  airport_data <- .mapply(RoundTheWord_stopover, dots = list(x = x, not_stop = not_stop), MoreArgs = NULL)
  stops0 <- lapply(airport_data, FUN = function(dt) c(
    total = .row_names_info(dt, type = 2L),
    japan = sum(dt$iso_country == 'JP'),
    europe = sum(dt$continent == 'EU')
  ))
  
  stops <- Reduce(`+`, stops0)
  SUM <- stops[1L]
  
  switch(airline, ANA = {
    cat('ANA (Star-Alliance) Round-The-World\n')
    cat('  Total Stop-over (\u22648): ', SUM, if (SUM > 8L) ' \u2718', '\n', sep = '')
    cat('   Stop-over in Japan (\u22644): ', JP <- stops[2L], if (JP > 4L) ' \u2718', '\n', sep = '')
    cat('   Stop-over in Europe (\u22643): ', EU <- stops[3L], if (EU > 3L) ' \u2718', '\n\n', sep = '')
  }, SQ = {
    cat('SQ (Star-Alliance) Round-The-World\n')
    cat('  Total Stop-over (\u22647): ', SUM, if (SUM > 7L) ' \u2718', '\n\n', sep = '')
  })
  
}



RoundTheWord_stopover <- function(x, not_stop = 'both') {
  # `x` is 'integer' vector (i.e., element of 'IATA')
  nx <- length(x)
  if (nx < 2L) stop('definition of IATA does not allow this')
  id <- switch(not_stop,
               none = x, # middle segment, all are stopover
               head = x[-1L], # 1st segment, start city not stopover
               tail = x[-nx], # last segment, end city not stopover
               both = x[-c(1L,nx)]) # single segment, start & end city not stopover
  airports@data[id, , drop = FALSE]
}







# [longitudeDirection()] finds the direction of travel by longitude.
# longitudeDirection(IATA('EWR-HNL-GUM-NRT-ICN-HKG-ARN-BCN-LIS-EWR'))
# longitudeDirection(IATA(c('EWR-HNL-GUM-NRT', 'ICN-HKG-ARN-BCN-LIS-EWR')))
longitudeDirection <- function(x) {
  # `x` is 'IATA'
  lapply(x, FUN = function(ix) {
    longitudeDirection_int(airports[ix, , drop = FALSE]@coords[,1L])
  })
}

longitudeDirection_int <- function(x) { # `x` is a vector of longitude
  nx <- length(x)
  if (nx < 2L) return(invisible())
  if (anyNA(x)) stop('do not allow NA in longitude')
  if (any(abs(x) > 180)) stop('illegal longitude!')
  x0 <- x[-1L] - x[-nx]
  ifelse(x0 < -180, yes = 'east', 
         no = ifelse(x0 < 0, 
                     yes = 'west', 
                     no = ifelse(x0 < 180, yes = 'east', no = 'west')))
}



