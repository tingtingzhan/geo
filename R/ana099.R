

#' @title ANA Round Trip from US
#' 
#' @param data \link[base]{data.frame}, downloaded from \url{https://ana.099.im}
#' 
#' @param US \link[base]{character} scalar or \link[base]{vector}, 
#' US airport for departure/arrival
#' 
#' @param min.,max. \link[base]{integer} scalar, minimum and maximum days 
#' between two departure dates.
#' 
#' @param ... ..
#' 
#' @details
#' Must download latest `.csv` file from \url{https://ana.099.im}.
#' 
#' Upper-right corner of table, download symbol.
#' 
#' 
#' @examples
#' if (FALSE) {
#' (fl = list.files(path = '~/Downloads', pattern = paste0('^', Sys.Date(), '.*_export.csv'), 
#'   full.names = TRUE))
#' dim(ana <- read.csv(file = fl, header = TRUE))
#' # unique(c(ana$departure, ana$arrival))
#' ana.099.im(ana, US = c('JFK', 'IAD'))
#' ana.099.im(ana, US = c('SFO', 'LAX'))
#' ana.099.im(ana, US = c('IAH'))
#' ana.099.im(ana, US = c('SEA', 'YVR'))
#' ana.099.im(ana, US = c('MEX'))
#' }
#' @importFrom plotly plot_ly
#' @export
ana.099.im <- function(data, US, min. = 21L, max. = 45L, ...) {
  
  data$date <- as.Date(data$date)
  data <- sort_by.data.frame(data, ~ date)
  
  rid0 <- (data$departure %in% US)
  if (!any(rid0)) stop('!any(rid0)')
  rid1 <- (data$arrival %in% US)
  if (!any(rid1)) stop('!any(rid1)')
  
  d0 <- data[rid0,]
  d1 <- data[rid1,]
  
  # row `0`: departure
  # col `1`: arrival
  len <- outer(X = d0$date, Y = d1$date, FUN = function(dt0, dt1) dt1 - dt0)
  
  id0 <- which(len >= min. & len <= max., arr.ind = TRUE)
  if (!length(id0)) return(invisible())
  
  id <- sort_by(as.data.frame.matrix(id0), ~ row + col)
  
  foo <- function(data, dup_rm = FALSE) {
    out <- with(data, sprintf(fmt = '%s %s %s \u2708\ufe0f %s', format.Date(date), flight_no, departure, arrival))
    if (dup_rm) out[duplicated(out)] <- ''
    return(out)
  }
  
  # sankey diagram
  
  foo2 <- function(data, dup_rm = FALSE) {
    with(data, sprintf(fmt = '%s %s \n%s - %s', format.Date(date), flight_no, departure, arrival))
  }
  
  sk1 <- foo2(d0[id$row,])
  sk2 <- foo2(d1[id$col,])
  if (length(intersect(sk1, sk2))) stop('do not allow!!')
  sk_node <- c(
    sort.int(unique.default(sk1)),
    sort.int(unique.default(sk2))
  )
  
  n_ <- dim(id0)[1L]
  sk_id <- match(c(sk1, sk2), table = sk_node)
  
  sk <- plot_ly(
    type = 'sankey',
    orientation = 'h',
    node = list(
      label = sk_node#,
      # label_position = 'outer' # does not work
    ),
    link = list(
      source = sk_id[1:n_] - 1L,
      target = sk_id[(n_+1):(2*n_)] - 1L,
      value = rep(1, times = 2*n_),
      label = sprintf(fmt = '%d days apart', d1[id$col,'date'] - d0[id$row,'date'])#,
      # color = {have write hue pallate by days-apart, manually?}
      # color = 'red' # does not work?
    )
  )
  print(sk)
  
  ret <- data.frame(
    departure = foo(d0[id$row,], dup_rm = TRUE),
    arrival = foo(d1[id$col,], dup_rm = FALSE)
  )
  return(invisible(ret)) # only for debugging
  
}



