
expect_no_error(object = {
  globe_minimal()
})


expect_no_error(object = {
  globe()
})


expect_no_error(object = {
  add_meridian()
})

expect_no_error(object = {
  add_TropicPolar()
})

expect_no_error(object = {
  '2026-04-14 21:30:00' |> # summer
    as.POSIXct() |> 
    add_sunmoon(time = _)
  '2026-11-14 21:30:00' |> # winter
    as.POSIXct() |> 
    add_sunmoon(time = _)
})


expect_no_error(object = {
  '2026-04-14 21:30:00' |> # summer
    as.POSIXct() |> 
    add_twilight(time = _)
  '2026-11-14 21:30:00' |> # winter
    as.POSIXct() |> 
    add_twilight(time = _)
})

