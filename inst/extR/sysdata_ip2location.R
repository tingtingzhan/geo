
# internal data used by the package (only for developer)

# Shift+Command+S to source

rm(list = ls(all.names = TRUE))

library(sp)

# https://github.com/ip2location/ip2location-iata-icao/blob/master/iata-icao.csv
url0 = 'https://raw.githubusercontent.com/ip2location/ip2location-iata-icao/master/iata-icao.csv'
airports_github = read.csv(url0, na.strings = '', stringsAsFactors = FALSE) # 'NA' means 'North America'
dim(airports_github)
sapply(airports_github, class)
# no continent