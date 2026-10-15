

#' @title The Earth with Various Layers
#' 
#' @description
#' The Earth with various layers.
#' 
#' 
#' @param time a \link[base]{POSIXct} object, see the function [subsolarPoint()]
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' The function [globe_twilight()] adds civil, nautical and astronomical twilight, on top of [globe_sunmoon()].
#' 
#' @examples
#' globe_twilight()
#' 
#' @name globe
#' @importFrom plotly add_text add_polygons
#' @export
globe_twilight <- \(time = Sys.time(), ...) {
  
  globe_sunmoon(time = time) |>
    add_polygons(
      line = list(width = 0), fillcolor = 'rgba(0, 0, 0, .06)',
      data = twilightPoints(time = time, sza = (90+6)*pi/180),
      x = ~ lon, y = ~ lat, hoverinfo = 'none'
    ) |>
    add_polygons(
      line = list(width = 0), fillcolor = 'rgba(0, 0, 0, .07)',
      data = twilightPoints(time = time, sza = (90+12)*pi/180),
      x = ~ lon, y = ~ lat, hoverinfo = 'none'
    ) |>
    add_polygons(
      line = list(width = 0), fillcolor = 'rgba(0, 0, 0, .08)',
      data = twilightPoints(time = time, sza = (90+18)*pi/180),
      x = ~ lon, y = ~ lat, hoverinfo = 'none'
    )
  
}



#' @rdname globe
#' @details
#' The function [globe_sunmoon()] adds the subsolar point, sublunar point and the terminator, on top of [globe()].
#' 
#' @examples
#' globe_sunmoon()
#' 
#' @importFrom plotly add_text add_polygons
#' @export
globe_sunmoon <- \(time = Sys.time(), ...) {
  
  globe() |>
    add_text(
      data = subsolarPoint(time = time),
      x = ~ lon, y = ~ lat, text = '\u2600\ufe0f',
      textfont = list(size = 20),
      hovertemplate = '<b>Sun:</b> (%{lon:.2f}, %{lat:.2f})<extra></extra>',
        # Adding <extra></extra> at the end of `hovertemplate` explicitly hides the secondary trace name box
      hoverlabel = list(
        bgcolor = 'white', font = 'darkorange',
        bordercolor = 'darkorange'
      )
    ) |>
    add_text(
      data = sublunarPoint(time = time),
      x = ~ lon, y = ~ lat, text = moonEmoji(time = time),
      textfont = list(size = 18),
      hovertemplate = '<b>Moon:</b> (%{lon:.2f}, %{lat:.2f})<extra></extra>',
      hoverlabel = list(
        bgcolor = 'white', font = 'cornflowerblue',
        bordercolor = 'cornflowerblue'
      )
    ) |>    
    add_polygons(
      data = terminatorPoints(time = time),
      x = ~ lon, y = ~ lat,
      line = list(width = 0),
      fillcolor = 'rgba(0, 0, 0, .05)',
      hoverinfo = 'none'
    )
  
}


#' @rdname globe
#' @details
#' The function [globe()] is the default earth, with `land`, `ocean`, `lakes`, `rivers`, `countries` and USA facing the user.
#' 
#' @examples
#' globe()
#' 
#' @importFrom plotly plot_geo layout toRGB
#' @export
globe <- \() {
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


#' @rdname globe
#' @details
#' The function [globe0()] contains the minimum code to plot the earth (calling only the functions \link[plotly]{plot_geo} and \link[plotly]{layout}).
#' 
#' @examples
#' globe0()
#' 
#' @importFrom plotly plot_geo layout
#' @export
globe0 <- \() {
  plot_geo() |>
    layout(
      geo = list(
        projection = list(
          type = 'orthographic'
        )
      )
    )
}


