

library(sf)

    
expect_no_error(object = {
  'EWR-PHL-JFK-IAD' |> 
    iata() |> 
    st_as_sf()
})


expect_no_error(object = {
  'EWR-PHL-JFK-IAD, DFW-IAH' |> 
    iata() |> 
    st_as_sf()
})
