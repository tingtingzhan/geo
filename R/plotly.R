

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
#' @importFrom plotly plot_geo add_segments add_lines layout toRGB
#' @importFrom utils head tail
#' @export
turn_IATA <- function(object, ...) {
  
  coord <- lapply(object, FUN = function(i) airports[i, , drop = FALSE]@coords)
  
  p0 <- plot_geo()

  p1 <- p0
  for (i in seq_along(coord)) {
    lon <- coord[[i]][,1L]
    lat <- coord[[i]][,2L]
    p1 <- add_segments(
      p = p1,
      x = head(lon, n = -1L), xend = tail(lon, n = -1L),
      y = head(lat, n = -1L), yend = tail(lat, n = -1L),
      size = I(2))
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
    # resolution = 500 # default is okay
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