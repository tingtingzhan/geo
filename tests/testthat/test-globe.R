
expect_no_error(object = {
  htmltools::tagList(
    globe_minimal(),
    globe(),
    add_meridian(),
    add_TropicPolar()
  ) |> 
    htmltools::browsable() |>
    print()
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

