
# Commits on Nov 6, 2026
# https://github.com/datasets/airport-codes/tree/master/data
airports_datahub = 'https://raw.githubusercontent.com/datasets/airport-codes/master/data/airport-codes.csv' |>
  read.csv(na.strings = '', stringsAsFactors = FALSE) |> # 'NA' indicate 'North America'
  # dim(.) # 85884
  subset.data.frame(
    subset = !is.na(iata_code) & !is.na(municipality) & (type %in% c('large_airport', 'medium_airport')) & !endsWith(name, suffix = 'Air Base')
  ) |>
  # dim(.) # 4348
  within.data.frame(expr = {
    !anyNA(continent)
    !anyDuplicated(ident)
    !anyDuplicated(iata_code)
    !anyDuplicated(ident)
    !anyNA(iso_country)
    tmp = strsplit(coordinates, split = ', '); coordinates = NULL
    stopifnot(lengths(tmp) == 2L)
    tmp = .mapply(FUN = c, dots = tmp, MoreArgs = NULL)
    latitude = tmp[[1L]] |> as.double()
    longitude = tmp[[2L]] |> as.double()
    # print(range.default(longitude)) # so that lat & lng are not confused!!
    # print(range.default(latitude))
    tmp = NULL
    iata = iata_code; iata_code = NULL
    shortnm = sprintf(
      fmt = '%s, %s, %s (%s)',
      name, municipality, iso_region,
      iata
    )
    stopifnot(!anyDuplicated(shortnm))
    tz = lutz::tz_lookup_coords(
      lon = longitude, lat = latitude,
      method = 'accurate' # not slow at all!!
    ) |> 
      suppressWarnings() # Some points are in areas with more than one time zone defined. These are often disputed areas and should be treated with care.
  }) |>
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) 
dim(airports_datahub) # 4358

