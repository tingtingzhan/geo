
# Commits on May 28, 2026
# https://github.com/tayljordan/ports/blob/main/ports.json
tmp = 'https://raw.githubusercontent.com/tayljordan/ports/refs/heads/main/ports.json' |>
  jsonlite::fromJSON()
stopifnot(length(tmp) == 2L)

ports_tayljordan = tmp[[2L]] |> 
  within.data.frame(expr = {
    tz = lutz::tz_lookup_coords(
      lon = longitude, lat = latitude,
      method = 'accurate' # not slow at all!!
    ) |> 
      suppressWarnings() # Some points are in areas with more than one time zone defined. These are often disputed areas and should be treated with care.
  }) |>
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) 

rm(tmp)