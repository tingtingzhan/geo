


expect_no_error(object = {
  'EWR-PHL-JFK-IAD' |> 
    iata()
})


expect_no_error(object = {
  'EWR-PHL-JFK-IAD, DFW-IAH' |> 
    iata()
})
