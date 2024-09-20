# internal data used by the package (only for developer)

# Shift+Command+S to source

rm(list = ls(all.names = TRUE))

library(sp)

# Latest commit f230d87 on July 22, 2023
# https://github.com/datasets/airport-codes/tree/master/data
url0 = 'https://raw.githubusercontent.com/datasets/airport-codes/master/data/airport-codes.csv'
airpt0 = read.csv(url0, na.strings = '', stringsAsFactors = FALSE) # 'NA' means 'North America'
dim(airpt0)
head(airpt0)
sapply(airpt0, class)

stopifnot(
  !anyNA(airpt0$continent), 
  !anyDuplicated(airpt0$ident)
)

if (FALSE) {
  table(airpt0$type)
  mean(is.na(airpt0$iata_code))
  mean(is.na(airpt0$municipality))
}
dim(airpt1 <- subset(airpt0, subset = !is.na(iata_code) & !is.na(municipality) & (type %in% c('large_airport', 'medium_airport')) & !endsWith(name, suffix = 'Air Base')))

stopifnot(
  !anyDuplicated(airpt1$iata_code),
  !anyDuplicated(airpt1$ident),
  !anyNA(airpt1$iso_country)
)

if (FALSE) {
  subset(airpt1, grepl('Deer Lake', name)) # two true airports_datasets
}

airports_datasets = within(airpt1, expr = {
  tmp = strsplit(coordinates, split = ', '); coordinates = NULL
  stopifnot(lengths(tmp) == 2L)
  coord = do.call(rbind, args = tmp) # slighly faster than # vapply(tmp, FUN = `[`, 1L, FUN.VALUE = '')
  storage.mode(coord) <- 'double'
  latitude = coord[, 1L]
  longitude = coord[, 2L]
  coord = NULL
  iata = iata_code; iata_code = NULL
})

#range.default(airports_datasets$longitude) # so that lat & lng are not confused!!
#range.default(airports_datasets$latitude)

#shortnm = with(airports_datasets, paste(name, municipality, iso_country, sep = ', '))
shortnm = with(airports_datasets, paste(name, municipality, iso_region, sep = ', ')) # two 'Deer Lake'
stopifnot(!anyDuplicated(shortnm))
attr(airports_datasets, which = 'row.names') = shortnm
coordinates(airports_datasets) = ~ longitude + latitude # colnames
# coordinates(airports_datasets) = ~ `coord[,2L]` + `coord[,1L]` # does not work
proj4string(airports_datasets) = CRS('+proj=longlat +datum=WGS84')
# `airports_datasets` is 'SpatialPointsDataFrame'

if (FALSE) {
  getMethod(`coordinates<-`, signature = signature(object = 'data.frame'))
  
  # `getMethod(sp::coordinates, signature('SpatialPoints'))`
  # is slow and will be avoided as much as possible
}


rm(list = c('url0', 'shortnm'))
rm(list = ls(pattern = 'airpt[0-9]'))


