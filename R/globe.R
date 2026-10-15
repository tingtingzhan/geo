

#' @importFrom plotly add_text add_polygons
.globe3 <- \(time = Sys.time(), ...) {
  
  .globe2(time = time) |>
    add_polygons(
      data = twilightPoints(zenith = (90+6) * pi / 180),
      x = ~ lon, y = ~ lat,
      line = list(width = 0),
      fillcolor = 'rgba(0, 0, 0, .06)',
      hoverinfo = 'none'
    ) |>
    add_polygons(
      data = twilightPoints(zenith = (90+12) * pi / 180),
      x = ~ lon, y = ~ lat,
      line = list(width = 0),
      fillcolor = 'rgba(0, 0, 0, .07)',
      hoverinfo = 'none'
    ) |>
    add_polygons(
      data = twilightPoints(zenith = (90+18) * pi / 180),
      x = ~ lon, y = ~ lat,
      line = list(width = 0),
      fillcolor = 'rgba(0, 0, 0, .08)',
      hoverinfo = 'none'
    )
  
}




#' @importFrom plotly add_text add_polygons
.globe2 <- \(time = Sys.time(), ...) {
  
  .globe1() |>
    add_text(
      data = subsolarPoint(time = time),
      x = ~ lon, y = ~ lat,
      text = '\u2600\ufe0f',
      textfont = list(size = 20),
      hoverinfo = 'none'
    ) |>
    add_text(
      data = sublunarPoint(time = time),
      x = ~ lon, y = ~ lat,
      text = time |> as.Date() |> moonEmoji(),
      textfont = list(size = 18),
      hoverinfo = 'none'
    ) |>    
    add_polygons(
      data = terminatorPoints(time = time),
      x = ~ lon, y = ~ lat,
      line = list(width = 0),
      fillcolor = 'rgba(0, 0, 0, .05)',
      hoverinfo = 'none'
    )
  
}



#' @importFrom plotly plot_geo layout toRGB
.globe1 <- \() {
  plot_geo() |>
    layout(
      showlegend = FALSE,
      geo = list( # https://plotly.com/r/reference/layout/geo/
        resolution = 50, # 50 high resolution, 110 low resolution
        framewidth = .7, framecolor = toRGB('grey80'), # outer frame of the earth
        showland = TRUE, landcolor = toRGB('linen'),
        showocean = TRUE, oceancolor = toRGB('aliceblue'), coastlinecolor = toRGB('peachpuff'), coastlinewidth = .5,
        showlakes = TRUE, lakecolor = toRGB('lightblue'),
        showrivers = TRUE, rivercolor = toRGB('lightblue'), riverwidth = .5,
        showcountries = TRUE, countrycolor = toRGB('peachpuff'), countrywidth = .7, 
        # showsubunits = TRUE, subunitcolor = toRGB('blue'), # state borders; not working, not sure why
        lonaxis = list(showgrid = TRUE, gridcolor = toRGB('gray80'), gridwidth = .5),
        lataxis = list(showgrid = TRUE, gridcolor = toRGB('gray80'), gridwidth = .5),
        projection = list(
          type = 'orthographic',
          rotation = list(
            # roll = 0 # default 0, roll of rotational axis of Earth
            lon = -100, lat = 40#, # let USA face user
            # 'mean' of longitude is *not* easy to define!!
            # mean of latitude is easy
          )
        )
      )
    )
}


# minimum code to get a globe
.globe0 <- \() {
  plot_geo() |>
    layout(
      geo = list(
        projection = list(
          type = 'orthographic'
        )
      )
    )
}


