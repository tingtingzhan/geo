

#' @title Moving Cross Track Distance
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters of the function \link[geosphere]{dist2gc}
#' 
#' @examples
#' iata('NRT-HNL-YVR')[[1L]] |>
#'  moving_dist2gc()
#' 
#' iata('CTU-PEK-ICN-JFK, EWR-IAH-LIM') |>
#'  lapply(FUN = moving_dist2gc)
#' 
#' @name moving_dist2gc
#' @export
moving_dist2gc <- \(x, ...) UseMethod(generic = 'moving_dist2gc')



#' @rdname moving_dist2gc
#' @export
moving_dist2gc.listof <- \(x, ...) {
  x |>
    lapply(FUN = moving_dist2gc, ...) # [moving_dist2gc.sf()]
}



#' @rdname moving_dist2gc
#' @importFrom sf st_geometry st_coordinates
#' @export
moving_dist2gc.sf <- \(x, ...) {
  cls <- class(st_geometry(x))[[1L]]
  if (cls != 'sfc_POINT') return(invisible())
  x |>
    st_coordinates() |>
    moving_dist2gc.matrix(...)
}



#' @rdname moving_dist2gc
#' @importFrom geosphere dist2gc
#' @export
moving_dist2gc.matrix <- \(x, ...) {
  # `x` is ncol-2L 'matrix' for coordinates
  nr <- nrow(x)
  if (nr < 3L) return(invisible())
  dist2gc(
    p1 = x[seq_len(nr-2L), , drop = FALSE], # gc start
    p2 = x[3:nr, , drop = FALSE], # gc end
    p3 = x[2:(nr-1L), , drop = FALSE], # point away from gc
    ...
  )
}
  
