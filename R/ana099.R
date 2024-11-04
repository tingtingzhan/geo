

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
#' Must download latest `.csv` file from \url{https://ana.099.im},
#' upper-right corner of table, download symbol.
#' 
#' 
#' @examples
#' \dontrun{
#' # Must download latest `.csv` file from https://ana.099.im
#' tm = Sys.time(); attr(tm, 'tzone') = 'GMT'
#' (fl = list.files(path = '~/Downloads', pattern = paste0('^', as.Date(tm), '.*_export.csv'), 
#'   full.names = TRUE))
#' dim(ana <- read.csv(file = sort(fl, decreasing = TRUE)[1L], header = TRUE))
#' # unique(c(ana$departure, ana$arrival))
#' ana.099.im(ana, US = c('JFK', 'IAD'))
#' ana.099.im(ana, US = c('SFO', 'LAX'), min. = 20, max. = Inf)
#' ana.099.im(ana, US = c('SEA', 'YVR'))
#' ana.099.im(ana, US = c('IAH'))
#' ana.099.im(ana, US = c('MEX'), min. = 35L, max. = 40L)
#' }
#' @importFrom plotly plot_ly toRGB
#' @importFrom scales pal_hue
#' @importFrom stats quantile
#' @export
ana.099.im <- function(data, US, min. = 21L, max. = 45L, ...) {
  
  data$date <- as.Date(data$date)
  data <- sort_by.data.frame(data, ~ date)
  
  rid0 <- (data$departure %in% US) # `$departure` airport in user's choice
  if (!any(rid0)) {
    message('No departure at ', paste(US, collapse = '/'))
    return(invisible())
  }
  rid1 <- (data$arrival %in% US) # `$arrival` airport in user's choice
  if (!any(rid1)) {
    message('No arrival at ', paste(US, collapse = '/'))
    return(invisible())
  }
  
  d0 <- data[rid0,] # eligible departure airport
  d1 <- data[rid1,] # eligible arrival airport
  
  len <- outer(X = d0$date, Y = d1$date, FUN = function(dt0, dt1) dt1 - dt0)
  # row: departure
  # col: arrival
  
  id_ <- which(len >= min. & len <= max., arr.ind = TRUE) # arrival minus departure (local date) satisfies user's choice
  if (!length(id_)) {
    message('No round trip between ', min., ' to ', max., ' days.')
    return(invisible())
  }
  id <- sort_by.data.frame(as.data.frame.matrix(id_), ~ row + col)
  d0_ <- d0[id$row,] # eligible departure flight
  d1_ <- d1[id$col,] # eligible arrival flight
  
  # sankey diagram
  
  node_flight <- function(data) with(data, sprintf(fmt = '%s %s \n%s - %s', format.Date(date, format = '\'%y-%m-%d'), flight_no, departure, arrival))
  sk1 <- node_flight(d0_)
  sk2 <- node_flight(d1_)
  
  sk_node <- c(
    sort.int(unique.default(sk1)),
    sort.int(unique.default(sk2))
  ) # sort eligible departure/arrival flight by date
  
  apart <- d1_$date - d0_$date
  apart_ <- unclass(apart)
  n_apart <- length(unique.default(apart_))
  apart_c <- cut.default(apart_, breaks = unique.default(quantile(apart_, probs = seq.int(0, 1, by = if (n_apart < 4) {
    .5
  } else .25))), include.lowest = TRUE)
    
  sk <- plot_ly(
    type = 'sankey',
    orientation = 'h',
    node = list(
      color = toRGB('gray90'),
      # border = toRGB('gray90'), # does not work :)
      label = sk_node#,
      # label_position = 'outer' # does not work
    ),
    link = list(
      source = match(sk1, table = sk_node) - 1L,
      target = match(sk2, table = sk_node) - 1L,
      value = rep(1, times = length(sk1)),
      label = sprintf(fmt = '%d days apart', apart),
      color = pal_hue()(n = length(levels(apart_c)))[unclass(apart_c)]
    )
  )
  # if (Sys.getenv('RSTUDIO')) rstudioapi::executeCommand('viewerClearAll')
  print(sk) # htmlwidgets:::print.htmlwidget
  
  prt_ <- function(data) {
    with(data, sprintf(fmt = '%s %s %s \u2708 %s', format.Date(date), flight_no, departure, arrival))
  }
  ret <- data.frame(
    departure = prt_(d0_),
    arrival = prt_(d1_)
  )
  return(invisible(ret)) # only for debugging
  
}



