

# https://github.com/ip2location/ip2location-iata-icao/blob/master/iata-icao.csv
url0 = 'https://raw.githubusercontent.com/ip2location/ip2location-iata-icao/master/iata-icao.csv'
airpt0 = read.csv(url0, na.strings = '', stringsAsFactors = FALSE) # 'NA' means 'North America'
dim(airpt0) # 8946
head(airpt0)
sapply(airpt0, class)
# no continent, which is fine. I am not doing [RoundTheWorld] right now

stopifnot(!anyDuplicated(airpt0$iata))

# this dataset has better naming than `datasets` github repo

# ULN airport is semi-operational
# https://en.wikipedia.org/wiki/Buyant-Ukhaa_International_Airport

# ISL airport is cargo
# https://rentotransfer.com/blog/istanbul-airports

dim(airports_ip2location <- subset(airpt0, !(iata %in% c('ULN', 'ISL'))))


shortnm = with(airports_ip2location, paste(airport, region_name, country_code, sep = ', '))
stopifnot(!anyDuplicated(shortnm))
attr(airports_ip2location, which = 'row.names') = shortnm
coordinates(airports_ip2location) = ~ longitude + latitude # colnames
# coordinates(airports_ip2location) = ~ `coord[,2L]` + `coord[,1L]` # does not work
proj4string(airports_ip2location) = CRS('+proj=longlat +datum=WGS84')
# `airports_ip2location` is 'SpatialPointsDataFrame'

if (FALSE) {
  getMethod(`coordinates<-`, signature = signature(object = 'data.frame'))
  
  # `getMethod(sp::coordinates, signature('SpatialPoints'))`
  # is slow and will be avoided as much as possible
}

rm(list = c('url0', 'shortnm'))
rm(list = ls(pattern = 'airpt[0-9]'))
