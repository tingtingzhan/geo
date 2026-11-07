
# Commits on May 28, 2026
# https://github.com/tayljordan/ports/blob/main/ports.json
tmp = 'https://raw.githubusercontent.com/tayljordan/ports/refs/heads/main/ports.json' |>
  jsonlite::fromJSON()
stopifnot(length(tmp) == 2L)

ports_tayljordan = tmp[[2L]] |> 
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) 

rm(tmp)