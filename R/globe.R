

#' @importFrom plotly toRGB
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


