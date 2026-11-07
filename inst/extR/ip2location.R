
# Commits on Oct 27, 2026
# https://github.com/ip2location/ip2location-iata-icao/blob/master/iata-icao.csv
airports_ip2location = 'https://raw.githubusercontent.com/ip2location/ip2location-iata-icao/master/iata-icao.csv' |>
  read.csv(na.strings = '', stringsAsFactors = FALSE) |> # 'NA' indicates 'North America'
  # dim(.) # 9160
  # head(.)
  # sapply(., class) # no continent, which is fine. I am not doing [RoundTheWorld] right now
  subset.data.frame(subset = !is.na(iata)) |>
  # dim(.) # 9126
  # which(duplicated(.$iata))
  subset.data.frame(subset = iata %notin% c('SGG')) |>
  within.data.frame(expr = {
    shortnm = sprintf(
      fmt = '%s, %s, %s (%s)',
      airport, region_name, country_code,
      iata
    )
    stopifnot(!anyDuplicated(shortnm))
  }) |>
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) 
dim(airports_ip2location)

