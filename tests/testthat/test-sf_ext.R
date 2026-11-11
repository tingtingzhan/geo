
expect_no_error(object = {
  library(sf)
  library(leaflet)
})
    
expect_no_error(object = {
  'EWR-PHL-JFK-IAD' |> 
    as.iata() |> 
    st_as_sf() |>
    leaflet_minimal() |>
    addMarkers(popup = ~ shortnm) |>
    print()
})


expect_no_error(object = {
  'EWR-PHL-JFK-IAD, DFW-IAH' |> 
    as.iata() |> 
    st_as_sf() |>
    leaflet_minimal() |>
    addMarkers(popup = ~ shortnm) |>
    print()
})