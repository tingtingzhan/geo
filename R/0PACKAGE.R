

#' @references
#' Source of internal datasets are, 
#' \describe{
#' \item{`airports_ip2location`}{\url{https://github.com/ip2location/ip2location-iata-icao}}
#' \item{`airports_datahub`}{\url{https://github.com/datasets/airport-codes}}
#' }
#' 
#' @importFrom methods new setClass setMethod callNextMethod
#' @importClassesFrom sp SpatialPointsDataFrame
'_PACKAGE'

# function name clash
# ggplot2::last_plot vs. plotly::last_plot
# plotly::filter vs. stats::filter
