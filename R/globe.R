

#' @title The Earth with Various Layers
#' 
#' @description
#' The Earth with various layers.
#' 
#' @param map an \CRANpkg{htmlwidgets} created by the function \link[plotly]{plot_geo}
#' 
#' @param time a \link[base]{POSIXct} object, see the function [subSolarPoint()], etc.
#' 
#' @param type \link[base]{character} \link[base]{vector}, see the function [twilightPolygon()]
#' 
#' @param sza \link[base]{numeric} \link[base]{vector}, see the function [twilightPolygon()]
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' The function [add_twilight()] adds civil, nautical and astronomical twilight, on top of [add_sunmoon()].
#' 
#' @seealso
#' [twilightPolygon()]
#' 
#' @name globe
#' @importFrom plotly add_polygons
#' @export
add_twilight <- \(
  map = add_sunmoon(time = time),
  time = Sys.time(), 
  type = c('civil', 'nautical', 'astronomical'),
  sza = NULL,
  ...
) {
  
  force(time)
  
  map |>
    add_polygons(
      data = twilightPolygons(time = time, type = type, sza = sza),
      x = ~ lon, y = ~ lat, 
      line = list(width = 0), fillcolor = ~ fillcolor,
      hoverinfo = 'none'
    )
  
}


hoverlabel_ <- \(x) {
  list(
    bgcolor = 'white', font = x, bordercolor = x
  )
}



#' @rdname globe
#' @details
#' The function [add_sunmoon()] adds, on top of [add_TropicPolar()], 
#' \itemize{
#' \item{the subsolar point, the ecliptic plane, and the obliquity of the ecliptic;}
#' \item{the sublunar point, the current Moon phase, the lunar orbital plane, and the inclination of the lunar orbit to the equator;}
#' \item{the lunar nodes and the lunar orbital inclination;}
#' \item{the terminator.}
#' }
#' 
#' @seealso 
#' [subSolarPoint()], [subLunarPoint()], [terminatorPolygon()]
#' 
#' @importFrom plotly add_text add_paths add_polygons
#' @export
add_sunmoon <- \(
  map = add_TropicPolar(),
  time = Sys.time(), 
  ...
) {
  
  force(time)
  
  subsolar <- subSolarPoint(time = time)
  sublunar <- subLunarPoint(time = time)
  equator_incl <- rbind.data.frame(
    eclipticEquator_lon(time = time),
    lunarEquator_lon(time = time)
  )

  map |>
    #add_text(
    #  data = rbind.data.frame(subsolar, sublunar),
    #  x = ~ lon, y = ~ lat, text = ~ text,
    #  name = ~ label,
    #  hovertemplate = '(%{lon:.3f}\u00b0, %{lat:.3f}\u00b0)',
    #  hoverlabel = ~ hoverlabel_(color) # \CRANpkg{plotly} messes up!!!
    #) |>
    add_text( # Sub-Solor Point
    #add_markers( # nah..
      data = subsolar,
      x = ~ lon, y = ~ lat, text = ~ text,
      #name = ~ sprintf(fmt = '<b>%s</b>', label),
      name = ~ label,
      #marker = ~ list(color = color), # trace-name color, but triggers screen output: "A marker object has been specified, but markers is not in the mode.  Adding markers to the mode..."
      hovertemplate = '(%{lon:.3f}\u00b0, %{lat:.3f}\u00b0)',
      hoverlabel = ~ hoverlabel_(color)
    ) |>
    add_text( # Sub-Lunar Point
      data = sublunar,
      x = ~ lon, y = ~ lat, text = ~ text,
      name = ~ label,
      hovertemplate = '(%{lon:.3f}\u00b0, %{lat:.3f}\u00b0)',
      hoverlabel = ~ hoverlabel_(color)
    ) |>
    add_paths( # ecliptic
      data = eclipticPolygon(time = time),
      x = ~ lon, y = ~ lat,
      line = ~ list(color = color, dash = 'dot', width = .5),
      hoverinfo = 'none'
    ) |>
    add_paths( # lunar orbit
      data = lunarOrbitPolygon(time = time),
      x = ~ lon, y = ~ lat,
      line = ~ list(color = color, dash = 'dot', width = .5),
      hoverinfo = 'none'
    ) |>
    add_markers( # ecliptic/lunar vs. Equator
      data = equator_incl,
      x = ~ lon, y = 0, text = ~ text,
      customdata = ~ incl,
      marker = ~ list(color = color),
      hovertemplate = '%{text}\nLon: %{lon:.3f}\u00b0\n%{customdata}<extra></extra>',
      hoverlabel = ~ hoverlabel_(color)
    ) |>
    add_markers( # ecliptic vs. lunar
      data = lunarNodePoint(time = time),
      x = ~ lon, y = ~ lat, text = ~ text,
      customdata = ~ incl,
      marker = ~ list(color = color),
      hovertemplate = '%{text}\n(%{lon:.3f}\u00b0, %{lat:.3f}\u00b0)\n%{customdata}<extra></extra>',
      hoverlabel = ~ hoverlabel_(color)
    ) |>
    add_polygons(
      data = terminatorPolygon(time = time),
      x = ~ lon, y = ~ lat,
      line = list(width = 0), fillcolor = ~ fillcolor,
      hoverinfo = 'none'
    ) |>
    layout(
      title = time |>
        format(usetz = TRUE)
    )
  
}


#' @rdname globe
#' @details
#' The function [add_TropicPolar()] adds the equatorial, the tropics, and the polar circles, on top of [add_meridian()].
#' 
#' @importFrom plotly add_paths toRGB
#' @export
add_TropicPolar <- \(
  map = add_meridian(),
  time = Sys.time(), 
  ...
) {
  
  lon <- seq(from = -180, to = 180, length.out = 501L) # not great circle!!
  path_line <- list(color = toRGB('gray85'), width = .5, dash = 'dot')
  
  obliquity <- celestialDegree(time)$obliquity
  
  map |>
    add_paths( # Arctic Circle
      x = lon, y = 90 - obliquity, 
      line = path_line, hoverinfo = 'none'
    ) |>
    add_paths( # Antarctic Circle
      x = lon, y = -90 + obliquity, 
      line = path_line, hoverinfo = 'none'
    ) |>
    add_paths( # Northern Tropic / Tropic of Cancer
      x = lon, y = obliquity, 
      line = path_line, hoverinfo = 'none'
    ) |>
    add_paths( # Southern Tropic / Tropic of Capricorn
      x = lon, y = - obliquity, 
      line = path_line, hoverinfo = 'none'
    ) |>
    add_paths( # Equator
      x = lon, y = 0, 
      line = path_line, hoverinfo = 'none'
    )
}




#' @rdname globe
#' @details
#' The function [add_meridian()] adds the meridian and antimeridian on top of [globe()].
#' 
#' @importFrom plotly plot_geo add_paths toRGB layout
#' @export
add_meridian <- \(
  map = globe()
) {
  
  lat <- seq(from = -90, to = 90, length.out = 3L)
  path_line <- list(color = toRGB('gray85'), width = .5, dash = 'dot')
  
  map |>
    add_paths( # Greenwich meridian
      x = 0, y = lat, 
      line = path_line, hoverinfo = 'none'
    ) |>
    add_paths( # antimeridian
      x = 180, y = lat, 
      line = path_line, hoverinfo = 'none'
    )
  
}




#' @rdname globe
#' @details
#' The function [globe()] is the default earth, that adds `land`, `ocean`, `lakes`, `rivers`, `countries`, longitude/latitude axes, and USA facing the user, on top of [globe_minimal()].
#' 
#' @importFrom plotly plot_geo layout
#' @export
globe <- \(
  map = globe_minimal()
) {
  map |>
    layout(
      showlegend = FALSE,
      geo = list( # https://plotly.com/r/reference/layout/geo/
        resolution = 50, # 50 high resolution, 110 low resolution
        framewidth = .7, framecolor = 'white', # outer frame of the earth. 'white' is the prettiest!! 'grey' is okay. But must use plotly::toRGB('grey80')
        showland = TRUE, landcolor = 'linen',
        showocean = TRUE, oceancolor = 'aliceblue', coastlinecolor = 'peachpuff', coastlinewidth = .5,
        showlakes = TRUE, lakecolor = 'lightblue',
        showrivers = TRUE, rivercolor = 'lightblue', riverwidth = .5,
        showcountries = TRUE, countrycolor = 'peachpuff', countrywidth = .7, 
        # showsubunits = TRUE, subunitcolor = 'blue', # state borders; not working, not sure why
        lonaxis = list(showgrid = TRUE, gridcolor = 'gray80', gridwidth = .5),
        lataxis = list(showgrid = TRUE, gridcolor = 'gray80', gridwidth = .5),
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
#' The function [globe_minimal()] contains the minimum code to create the earth, calling only 
#' \itemize{
#' \item{the function \link[plotly]{plot_geo} with an `'orthographic'` projection;}
#' \item{the function \link[htmlwidgets]{onRender} to obtain user coordinates from the browser.}
#' }
#' 
#' 
#' @references
#' \url{https://plotly.com/r/reference/layout/geo/}
#' 
#' @importFrom htmlwidgets onRender
#' @importFrom plotly plot_geo layout
#' @export
globe_minimal <- \() {
  plot_geo() |>
    layout(
      geo = list(
        projection = list(
          type = 'orthographic'
          #type = 'equirectangular'
        )
      )
    ) |>
    onRender(
      jsCode = system.file('js', 'userLoc.js', package = 'geo') |> # by Gemini!
        readLines() |>
        paste(collapse = '\n')
    )
}


