
library(leaflet)


expect_no_error(object = {
  trip1 = 'EWR-PHL-JFK-IAD' |> 
    iata()
  trip1
})


expect_no_error(object = {
  trip2 = 'EWR-PHL-JFK-IAD, DFW-IAH' |> 
    iata()
  trip2
})


expect_no_error(object = {
  trip1 |>
    moving_dist2gc()
})




expect_no_error(object = {
  iata('JFK-EWR-PHL-BWI-IAD')[[1L]] |> 
    leaflet() |>
    addTiles() |>
    addMarkers(popup = ~ shortnm)
})


