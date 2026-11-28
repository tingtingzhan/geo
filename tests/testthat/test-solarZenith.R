

(time = as.POSIXlt('2026-11-26 01:01:01 EST') + 
  60*60*24 * c(0, 90, 180))



expect_no_error(object = {
  htmltools::tagList(
    add_solarZenith_frame(time = time),
    add_solarZenith_frame(time = time, sza = (90+30)/180*pi),
    add_solarZenith_frame(time = time, sza = (90-30)/180*pi)
  ) |> 
    htmltools::browsable() |>
    print()
})


