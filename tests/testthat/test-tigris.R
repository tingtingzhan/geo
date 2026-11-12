

library(tigris)
library(leaflet)
options(tigris_use_cache = TRUE)



expect_no_error(object = {
  rails() |>
    subset(subset = (FULLNAME == 'Pennsylvania RR')) |>
    st_transform_all() |>
    leaflet_minimal() |>
    addPolylines() |>
    print()
})


expect_no_error(object = {
  landmarks(state = 'RI') |> 
    subset.data.frame(subset = !is.na(FULLNAME)) |>
    st_transform_all() |>
    leaflet_minimal() |>
    addMarkers(
      popup = ~ FULLNAME,
      clusterOptions = markerClusterOptions()
    ) |>
    print()
})



expect_no_error(object = {
  states() |>
    subset(subset = STUSPS %notin% c(
      'AK', # Alaska
      'GU', # Guam
      'MP' # Commonwealth of the Northern Mariana Islands
    )) |>
    st_transform_all() |>
    leaflet_minimal() |>
    addPolygons(
    popup = ~ NAME,
      fillColor = 'royalblue', fillOpacity = .1, weight = 2, color = 'white', opacity = 1
    ) |>
    print()
})


expect_no_error(object = {
  w1 = area_water(state = 'PA', county = 'Philadelphia') |>
    st_transform_all()
  w2 = linear_water(state = 'PA', county = 'Philadelphia') |>
    st_transform_all()
  w1 |> 
    leaflet_minimal() |>
    addPolylines(color = '#566CF2') |>
    addPolylines(
      color = '#566CF2', 
      data = w2
    ) |>
    print()
})


