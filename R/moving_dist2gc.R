

#' @title Moving Cross Track Distance
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters of the function \link[geosphere]{dist2gc}
#' 
#' @name moving_dist2gc
#' @export
moving_dist2gc <- function(x, ...) UseMethod(generic = 'moving_dist2gc')


#' @rdname moving_dist2gc
#' @export
moving_dist2gc.iata <- \(x, ...) {
  airports_ip2location[x, , drop = FALSE] |>
    moving_dist2gc.sf(...)
}

#' @rdname moving_dist2gc
#' @export
moving_dist2gc.iatalist <- \(x, ...) {
  lapply(x, FUN = moving_dist2gc.iata, ...)
}



#' @rdname moving_dist2gc
#' @importFrom sf st_coordinates
#' @export
moving_dist2gc.sf <- \(x, ...) {
  if (nrow(x) < 3L) return(invisible())
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
  dist2gc(
    p1 = x[seq_len(nr-2L), , drop = FALSE], # gc start
    p2 = x[3:nr, , drop = FALSE], # gc end
    p3 = x[2:(nr-1L), , drop = FALSE], # point away from gc
    ...
  )
}
  
