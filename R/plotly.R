

#' @title Draw `'IATA'` using \CRANpkg{plotly}
#' 
#' @description
#' ..
#' 
#' @param object `'IATA'`
#' 
#' @param ... ..
#' 
#' @references 
#' \url{https://plotly.com/r/lines-on-maps/}
#' 
#' @importFrom plotly plot_geo add_markers add_segments add_lines layout toRGB
#' @importFrom scales hue_pal
#' @export
turn_IATA <- function(object, ...) {
  
  coord <- lapply(object, FUN = function(i) airports_ip2location[i, , drop = FALSE]@coords)
  
  p0 <- plot_geo()

  col <- toRGB(hue_pal()(n = length(coord)))
  
  p1 <- p0
  for (i in seq_along(coord)) {
    lon <- coord[[i]][,1L]
    lat <- coord[[i]][,2L]
    nm <- rownames(coord[[i]])
    n <- dim(coord[[i]])[1L]
    if (n <= 1L) stop('wont allow')

    p1 <- add_segments(
      p = p1,
      x = lon[seq_len(n-1L)], xend = lon[2:n],
      y = lat[seq_len(n-1L)], yend = lat[2:n],
      line = list(color = col[i], width = 2),
      hoverinfo = 'none'
      # text = rep('abc', n-1L), hoverinfo = 'text', 
      # https://github.com/plotly/plotly.R/issues/1832#issuecomment-675721763
      # TL;DR: plotly cannot do this, as of Sep 2024
    )
    
    p1 <- add_markers(
      p = p1,
      x = lon, y = lat, text = nm,
      marker = list(color = col[i]),
      hoverinfo = 'text', 
      hoverlabel = list(
        font = list(
          color = 'white'
        ), 
        #bordercolor = 'white' # default 'black' 
        bordercolor = col[i]
      )
    )
    # `add_markers` after `add_segments` !!
    # it seems `hoverinfo` overwrites!!
    
  }
  
  # https://plotly.com/r/reference/layout/geo/
  geo <- list(
    resolution = 50, # 50 high resolution, 110 low resolution
    showland = TRUE, landcolor = toRGB('grey97'),
    showlakes = TRUE, lakecolor = toRGB('lightblue'),
    showrivers = TRUE, rivercolor = toRGB('lightblue'), riverwidth = .5,
    showcountries = TRUE, countrycolor = toRGB('grey50'), countrywidth = .5, 
    # showsubunits = TRUE, subunitcolor = toRGB('blue'), # state borders; not working, not sure why
    showocean = TRUE, oceancolor = toRGB('white'), coastlinecolor = toRGB('grey50'), coastlinewidth = .5,
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
  
  return(layout(
    p = p1,
    #title = NULL,
    showlegend = FALSE,
    geo = geo
  ))
  
}


