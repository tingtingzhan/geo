

library(sf)
library(leaflet)
library(leaflet.tzh)

    
expect_no_error(object = {
  'EWR-PHL-JFK-IAD' |> 
    iata() |> 
    st_as_sf() |>
    leaflet_minimal() |>
    addMarkers(popup = ~ shortnm) |>
    print()
})


expect_no_error(object = {
  'EWR-PHL-JFK-IAD, DFW-IAH' |> 
    iata() |> 
    st_as_sf() |>
    leaflet_minimal() |>
    addMarkers(popup = ~ shortnm) |>
    print()
})
