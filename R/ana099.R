

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
#' @details
#' Must download latest `.csv` file from \url{https://ana.099.im}.
#' 
#' Upper-right corner of table, download symbol.
#' 
#' 
#' @examples
#' if (FALSE) {
#' dim(ana <- read.csv(file = list.files(path = '~/Downloads', pattern = paste0('^', Sys.Date(), '.*_export.csv'), full.names = TRUE), header = TRUE))
#' unique(c(ana$departure, ana$arrival))
#' ana.099.im(ana, US = c('JFK', 'IAD'))
#' ana.099.im(ana, US = c('SFO', 'LAX'))
#' ana.099.im(ana, US = c('IAH'))
#' ana.099.im(ana, US = c('SEA'))
#' ana.099.im(ana, US = c('YVR'))
#' }
#' @export
ana.099.im <- function(data, US, min. = 21L, max. = 45L, ...) {
  
  data$date <- as.Date(data$date)
  data <- sort_by.data.frame(data, ~ date)
  
  id0 <- (data$departure %in% US)
  if (!any(id0)) stop('!any(id0)')
  id1 <- (data$arrival %in% US)
  if (!any(id1)) stop('!any(id1)')
  
  d0 <- data[id0,]
  d1 <- data[id1,]
  
  # row `0`: departure
  # col `1`: arrival
  len <- outer(X = d0$date, Y = d1$date, FUN = function(dt0, dt1) dt1 - dt0)
  len_id0 <- which(len >= min. & len <= max., arr.ind = TRUE)
  
  if (!length(len_id0)) return(invisible())
  
  len_id <- sort_by(as.data.frame.matrix(len_id0), ~ row + col)
  
  foo <- function(data, dup_rm = FALSE) {
    out <- with(data, sprintf(fmt = '%s %s %s \u2708\ufe0f %s', format.Date(date), flight_no, departure, arrival))
    if (dup_rm) out[duplicated(out)] <- ''
    return(out)
  }
  
  ret <- data.frame(
    departure = foo(d0[len_id[[1L]],], dup_rm = TRUE),
    arrival = foo(d1[len_id[[2L]],], dup_rm = FALSE)
  )
  return(ret)
  
}