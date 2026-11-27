
winter = "2026-11-26 01:01:01 EST" |>
  as.POSIXlt()
(summer = winter + 60*60*24*180)
(spring = winter + 60*60*24*90)

# only for debugging
add_solarZenith <- \(
  map = add_sunmoon(time = time),
  time = Sys.time(), 
  ...
) {
  
  force(time)
  
  map |>
    add_polygons(
      data = solarZenithPolygon(time = time, ...),
      x = ~ lon, y = ~ lat,
      line = list(width = 0), 
      fillcolor = ~ 'rgba(0, 0, 0, .051)',
      hoverinfo = 'none'
    ) |>
    layout(
      title = time |>
        format(usetz = TRUE)
    )
  
}






expect_no_error(object = { # terminator
  add_solarZenith(time = winter)
  add_solarZenith(time = summer)
  add_solarZenith(time = spring)
})

expect_no_error(object = { # sza > pi/2
  sza1a = (90+10)/180*pi
  add_solarZenith(time = winter, sza = sza1a)
  add_solarZenith(time = summer, sza = sza1a)
  add_solarZenith(time = spring, sza = sza1a)
})


expect_no_error(object = { # sza > pi/2
  sza1b = (90+30)/180*pi
  add_solarZenith(time = winter, sza = sza1b)
  add_solarZenith(time = summer, sza = sza1b)
  add_solarZenith(time = spring, sza = sza1b)
})

expect_no_error(object = { # sza < pi/2
  sza2a = (90-10)/180*pi
  add_solarZenith(time = winter, sza = sza2a)
  add_solarZenith(time = summer, sza = sza2a)
  add_solarZenith(time = spring, sza = sza2a)
})


expect_no_error(object = { # sza < pi/2
  sza2b = (90-30)/180*pi
  add_solarZenith(time = winter, sza = sza2b)
  add_solarZenith(time = summer, sza = sza2b)
  add_solarZenith(time = spring, sza = sza2b)
})
