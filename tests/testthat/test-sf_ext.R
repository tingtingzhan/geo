

expect_no_error(
  object = {
    library(sf)
    library(leaflet)
    'EWR-PHL-JFK-IAD' |> 
      as.iata() |> 
      st_as_sf() |>
      leaflet_minimal() |>
      addMarkers(popup = ~ shortnm)
  }
)


expect_no_error(
  object = {
    library(sf)
    library(leaflet)
    'EWR-PHL-JFK-IAD, DFW-IAH' |> 
      as.iata() |> 
      st_as_sf() |>
      leaflet_minimal() |>
      addMarkers(popup = ~ shortnm)
  }
)