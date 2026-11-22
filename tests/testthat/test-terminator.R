
expect_no_error(object = {
  subSolarPoint()
})

expect_no_error(object = {
  subLunarPoint()
})

expect_no_error(object = {
  moonEmoji()
})

expect_no_error(object = {
  terminatorPolygon()
})

expect_no_error(object = { 
  twilightPolygon(sza = (90+12)/180*pi, n = 11L)
  twilightPolygon(type = 'nautical', n = 11L)
}) 
