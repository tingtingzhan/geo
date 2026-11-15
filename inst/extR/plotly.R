
# Jan 27, 2017
# https://github.com/plotly/datasets/blob/master/volcano_db.csv

volcano_plotly = 'https://github.com/plotly/datasets/raw/refs/heads/master/volcano_db.csv' |>
  read.csv(na.strings = '', stringsAsFactors = FALSE) |>
  # dim(.) # 1571
  subset.data.frame(
    subset = Last.Known %notin% c('?', 'Unknown')
  ) |>
  # dim(.) # 1387
  within.data.frame(expr = {
    tz = lutz::tz_lookup_coords(
      lon = Longitude, lat = Latitude,
      method = 'accurate' # not slow at all!!
    ) |> 
      suppressWarnings() # Some points are in areas with more than one time zone defined. These are often disputed areas and should be treated with care.
  }) |>
  sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) 
dim(volcano_plotly) # 4358



