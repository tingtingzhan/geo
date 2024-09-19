# internal data used by the package (only for developer)

# Shift+Command+S to source

rm(list = ls(all.names = TRUE))

library(sp)

# Latest commit f230d87 on July 22, 2023
# https://github.com/datasets/airport-codes/tree/master/data
url0 = 'https://raw.githubusercontent.com/datasets/airport-codes/master/data/airport-codes.csv'
airports_github = read.csv(url0, na.strings = '', stringsAsFactors = FALSE) # 'NA' means 'North America'
dim(airports_github)
sapply(airports_github, class)

stopifnot(
  !anyNA(airports_github$continent), 
  !anyDuplicated(airports_github$ident)
)

if (FALSE) {
  table(airports_github$type)
  mean(is.na(airports_github$iata_code))
  mean(is.na(airports_github$municipality))
}
dim(airports_IATA <- subset(airports_github, subset = !is.na(iata_code) & !is.na(municipality) & (type %in% c('large_airport', 'medium_airport')) & !endsWith(name, suffix = 'Air Base')))

stopifnot(
  !anyDuplicated(airports_IATA$iata_code),
  !anyDuplicated(airports_IATA$ident),
  !anyNA(airports_IATA$iso_country)
)

if (FALSE) {
  subset(airports_IATA, grepl('Deer Lake', name)) # two true airports
}

airports = within(airports_IATA, expr = {
  tmp = strsplit(coordinates, split = ', '); coordinates = NULL
  stopifnot(lengths(tmp) == 2L)
  coord = do.call(rbind, args = tmp) # slighly faster than # vapply(tmp, FUN = `[`, 1L, FUN.VALUE = '')
  storage.mode(coord) <- 'double'
  latitude = coord[, 1L]
  longitude = coord[, 2L]
  coord = NULL
  
  name = gsub(pattern = ' National Airport| International Airport| Airport', replacement = '', x = name)
  municipality = gsub(pattern = ' National Airport| International Airport| Airport', replacement = '', x = municipality) # GUM
})

#range.default(airports$longitude) # so that lat & lng are not confused!!
#range.default(airports$latitude)

#shortnm = with(airports, paste(name, municipality, iso_country, sep = ', '))
shortnm = with(airports, paste(name, municipality, iso_region, sep = ', ')) # two 'Deer Lake'
stopifnot(!anyDuplicated(shortnm))
attr(airports, which = 'row.names') = shortnm
coordinates(airports) = ~ longitude + latitude # colnames
# coordinates(airports) = ~ `coord[,2L]` + `coord[,1L]` # does not work
proj4string(airports) = CRS('+proj=longlat +datum=WGS84')
# airports is 'SpatialPointsDataFrame'

if (FALSE) {
  getMethod(`coordinates<-`, signature = signature(object = 'data.frame'))
  
  # `getMethod(sp::coordinates, signature('SpatialPoints'))`
  # is slow and will be avoided as much as possible
}




# https://www.riinu.me/2022/02/world-map-ggplot2/
worldmap = ggplot2::fortify(maps::map(database = 'world', regions = '.', exact = FALSE, plot = FALSE, fill = TRUE))



# SAVE !!!
save(airports, worldmap,
     file = './R/sysdata.rda', compress = 'xz')

rm(list = ls(all.names = TRUE))
