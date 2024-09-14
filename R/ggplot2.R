

#' @importFrom ggplot2 autolayer aes geom_sf
#' @importFrom grid arrow unit
#' @export
autolayer.IATA <- function(object, ...) {
  
  sf_data <- lapply(object, FUN = function(obj) gc_(airports[obj, , drop = FALSE]))
  
  # https://stackoverflow.com/questions/67412079/is-there-a-way-to-add-arrows-to-a-simple-features-line-in-ggplot-geom-sf
  ar <- arrow(angle = 20, ends = 'last', type = 'open', length = unit(.03, units = 'npc'))
  
  # ?ggplot2::geom_sf includes ?ggplot2::layer_sf and ?ggplot2::coord_sf
  # ?ggplot2::coord_sf will print a message when 'Coordinate system already present'
  
  n <- length(object)
  if (n == 1L) return(geom_sf(data = sf_data[[1L]], arrow = ar))
  .mapply(FUN = function(data, nm) geom_sf(
    data = data, mapping = aes(colour = nm), arrow = ar, show.legend = FALSE
  ), dots = list(data = sf_data, nm = as.character.default(seq_len(n))), MoreArgs = NULL)
  
}


#' @importFrom ggplot2 autoplot ggplot geom_map xlim ylim
#' @export
autoplot.IATA <- function(object, ...) {
  ggplot() + 
    geom_map(
      mapping = aes(map_id = unique.default(worldmap$region)), 
      map = worldmap, 
      fill = 'white', 
      linewidth = .2, # country border thickness
      colour = 'grey65', # country border colour
    ) + 
    autolayer.IATA(object, ...) + 
    xlim(c(-160, 175)) + ylim(c(-75, 80)) # have to leave some space for x- and y-labels to show! 
}
