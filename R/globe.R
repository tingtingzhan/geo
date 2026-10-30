

#' @title The Earth with Various Layers
#' 
#' @description
#' The Earth with various layers.
#' 
#' 
#' @param time a \link[base]{POSIXct} object, see the function [subSolarPoint()]
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' The function [globe_twilight()] adds civil, nautical and astronomical twilight, on top of [globe_sunmoon()].
#' 
#' @note
#' Twilight polygons (for the function [globe_twilight()]) are challenging for \link[plotly]{animation_opts}.
#' 
#' @examples
#' globe_twilight()
#' 
#' @name globe
#' @importFrom plotly add_text add_polygons
#' @export
globe_twilight <- \(time = Sys.time(), ...) {
  
  civil <- twilightPolygon(time = time, sza = (90+6)*pi/180) |>
    st_coordinates()
  nautical <- twilightPolygon(time = time, sza = (90+12)*pi/180) |>
    st_coordinates()
  astronomical <- twilightPolygon(time = time, sza = (90+18)*pi/180) |>
    st_coordinates()
  
  globe_sunmoon(time = time) |>
    add_polygons(
      line = list(width = 0), fillcolor = 'rgba(0, 0, 0, .051)',
      x = civil[,1L], y = civil[,2L], hoverinfo = 'none'
    ) |>
    add_polygons(
      line = list(width = 0), fillcolor = 'rgba(0, 0, 0, .052)',
      x = nautical[,1L], y = nautical[,2L], hoverinfo = 'none'
    ) |>
    add_polygons(
      line = list(width = 0), fillcolor = 'rgba(0, 0, 0, .053)',
      x = astronomical[,1L], y = astronomical[,2L], hoverinfo = 'none'
    )
  
}



hovertemplate_ <- \(x) {
  x |>
    sprintf(fmt = '<b>%s</b> (%%{lon:.3f}, %%{lat:.3f})<extra></extra>')
  # Adding <extra></extra> at the end of `hovertemplate` explicitly hides the secondary trace name box
}

hoverlabel_ <- \(x) {
  list(
    bgcolor = 'white', font = x, bordercolor = x
  )
}



#' @rdname globe
#' @details
#' The function [globe_sunmoon()] adds the subsolar point, the sublunar point and the terminator, on top of [globe_TropicPolar()].
#' 
#' @examples
#' globe_sunmoon()
#' 
#' @importFrom plotly add_text add_polygons
#' @export
globe_sunmoon <- \(time = Sys.time(), ...) {
  
  subsolar <- subSolarPoint(time = time)
  sublunar <- subLunarPoint(time = time)
  terminator <- terminatorPolygon(time = time)[[1L]]
  
  globe_TropicPolar() |>
    add_text(
      x = subsolar[1L], y = subsolar[2L], 
      text = '\u2600\ufe0f',
      textfont = list(size = 20),
      hovertemplate = hovertemplate_('Sun'),
      hoverlabel = hoverlabel_('darkorange')
    ) |>
    add_text(
      x = sublunar[1L], y = sublunar[2L], 
      text = moonEmoji(time = time),
      textfont = list(size = 18),
      hovertemplate = hovertemplate_('Moon'),
      hoverlabel = hoverlabel_('cornflowerblue')
    ) |>    
    add_polygons(
      line = list(width = 0), fillcolor = 'rgba(0, 0, 0, .05)',
      x = terminator[,1L], y = terminator[,2L], hoverinfo = 'none'
    )
  
}


if (FALSE) {
  debug(globe_sunmoon_path); globe_sunmoon_path()
}

#' @importFrom plotly animation_opts
globe_sunmoon_path <- \(
  time = Sys.time(), 
  n = 11L,
  ...
) {
  
  # +/- 12hours
  intv <- time + c(-1, 1) * 60*60*24/2
  tm <- seq(from = intv[1L], to = intv[2L], length.out = n)
  tm_char <- format(tm) # ?base::format.POSIXct
  
  subsolar <- subSolarPoint(time = tm)
  sublunar <- subLunarPoint(time = tm)
  # ignore terminator, for now
  
  # ask Google/Gemini 
  # `R plot_ly animation_opts`
  # `R plotly plot_geo frame`
  
  globe_TropicPolar() |>
    add_text(
      x = subsolar[,1L], y = subsolar[,2L],
      text = '\u2600\ufe0f',
      frame = tm_char,
      hovertemplate = hovertemplate_('Sun'),
      hoverlabel = hoverlabel_('darkorange')
    ) |>
    add_text(
      x = sublunar[,1L], y = sublunar[,2L],
      text = moonEmoji(time = time),
      frame = tm_char,
      hovertemplate = hovertemplate_('Moon'),
      hoverlabel = hoverlabel_('cornflowerblue')
    ) |>
    animation_opts(
      frame = 1000,
      transition = 500,
      easing = 'linear',
      redraw = FALSE
    )
  
  # why sun and moon do no move...
  
}



#' @rdname globe
#' @details
#' The function [globe_TropicPolar()] adds the equatorial, the tropics, and the polar circles, on top of [globe_meridian()].
#' 
#' @examples
#' globe_TropicPolar()
#' 
#' @importFrom plotly add_paths
#' @export
globe_TropicPolar <- \() {
  
  lon <- seq(from = -180, to = 180, length.out = 501L)
  path_line <- list(color = toRGB('gray85'), width = .4, dash = 'dot')
  
  globe_meridian() |>
    add_paths(
      x = lon, y = 66.56, 
      line = path_line, hoverinfo = 'none'
    ) |>
    add_paths(
      x = lon, y = -66.56, 
      line = path_line, hoverinfo = 'none'
    ) |>
    add_paths(
      x = lon, y = 23.44, 
      line = path_line, hoverinfo = 'none'
    ) |>
    add_paths(
      x = lon, y = -23.44, 
      line = path_line, hoverinfo = 'none'
    ) |>
    add_paths(
      x = lon, y = 0, 
      line = path_line, hoverinfo = 'none'
    )
}




#' @rdname globe
#' @details
#' The function [globe_meridian()] adds the meridian and antimeridian on top of [globe()].
#' 
#' @examples
#' globe_meridian()
#' 
#' @importFrom plotly plot_geo add_paths toRGB layout
#' @export
globe_meridian <- \() {
  
  lat <- seq(from = -90, to = 90, length.out = 3L)
  path_line <- list(color = toRGB('gray85'), width = .4, dash = 'dot')
  
  globe() |>
    add_paths(
      x = 0, y = lat, 
      line = path_line, hoverinfo = 'none'
    ) |>
    add_paths(
      x = 180, y = lat, 
      line = path_line, hoverinfo = 'none'
    )
}




#' @rdname globe
#' @details
#' The function [globe()] is the default earth, with `land`, `ocean`, `lakes`, `rivers`, `countries`, longitude/latitude axes, and USA facing the user.
#' 
#' @examples
#' globe()
#' 
#' @importFrom plotly plot_geo add_paths toRGB layout
#' @export
globe <- \() {
  plot_geo() |>
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
#' The function [globe_minimal()] contains the minimum code to create the earth (calling only the function \link[plotly]{plot_geo} with an `'orthographic'` projection).
#' 
#' @examples
#' globe_minimal()
#' 
#' @references
#' \url{https://plotly.com/r/reference/layout/geo/}
#' 
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
    )
}


