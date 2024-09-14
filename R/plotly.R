

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
#' @importFrom utils head tail
#' @export
turn_IATA <- function(object, ...) {
  
  coord <- lapply(object, FUN = function(i) airports[i, , drop = FALSE]@coords)
  
  p0 <- plot_geo()

  col <- hue_pal()(n = length(coord))
  
  p1 <- p0
  for (i in seq_along(coord)) {
    lon <- coord[[i]][,1L]
    lat <- coord[[i]][,2L]
    nm <- rownames(coord[[i]])

    p1 <- add_segments(
      p = p1,
      x = head(lon, n = -1L), xend = tail(lon, n = -1L),
      y = head(lat, n = -1L), yend = tail(lat, n = -1L),
      # text = head(nm, n = -1L), # must be same length as `x` and `y` ..
      hoverinfo = 'none',
      line = list(color = toRGB(col[i])),
      size = I(2))
    
    p1 <- add_markers(
      p = p1,
      x = lon, y = lat, text = nm,
      marker = list(color = toRGB(col[i])),
      hoverinfo = 'text', 
      hoverlabel = list(
        font = list(
          color = 'white'
        ), 
        bordercolor = 'white' # otherwise determined by `marker` color
      ),
      alpha = 0.5)
    # `add_markers` after `add_segments` !!
    # it seems `hoverinfo` overwrites!!
    
  }
  
  geo <- list(
    showland = TRUE,
    showlakes = TRUE,
    showcountries = TRUE,
    showocean = TRUE,
    countrywidth = 0.5,
    landcolor = toRGB("grey90"),
    lakecolor = toRGB("white"),
    oceancolor = toRGB("white"),
    projection = list(
      type = 'orthographic',
      rotation = list(
        lon = -100,
        lat = 40,
        roll = 0
      )
    ),
    lonaxis = list(
      showgrid = TRUE,
      gridcolor = toRGB("gray40"),
      gridwidth = 0.5
    ),
    lataxis = list(
      showgrid = TRUE,
      gridcolor = toRGB("gray40"),
      gridwidth = 0.5
    )
    # resolution = 5e3 # default is okay
    # countrycolor = toRGB("grey80") # country border, use default
    # coastlinewidth = 2 # use default
  )
  
  return(layout(
    p = p1,
    #title = NULL,
    showlegend = FALSE,
    geo = geo
  ))
  
}


