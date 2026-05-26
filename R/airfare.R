

#' @title \linkS4class{airfare}
#' 
#' @slot carrier \link[base]{character} scalar
#' 
#' @slot depart \link[base]{character} scalar
#' 
#' @slot arrive \link[base]{character} scalar
#' 
#' @slot currency \link[base]{character} scalar
#' 
#' @slot mileage \link[base]{double} scalar
#' 
#' @slot code \link[base]{character} scalar
#' 
#' @slot basefare,tax,YQ,YR,upgrade \link[base]{double} scalar
#' 
#' @name airfare
#' @aliases airfare-class
#' @export
setClass(Class = 'airfare', slots = c(
  carrier = 'character',
  depart = 'character',
  arrive = 'character',
  currency = 'character',
  mileage = 'numeric',
  code = 'character',
  basefare = 'numeric',
  tax = 'numeric',
  YQ = 'numeric', YR = 'numeric',
  upgrade = 'numeric'
))



#' @importFrom geosphere distGeo
setMethod(f = initialize, signature = 'airfare', definition = \(.Object, ...) {
  
  x <- callNextMethod(.Object, ...)
  
  x. <- c(x@depart, x@arrive) |>
    paste(collapse = '-') |>
    as.iata()
  x.[[1L]] |> plot.iata() |> print()
  
  coords <- airports_ip2location@coords[x.[[1L]],]
  if (length(x@mileage)) warning('Do not manully put in @mileage !!')
  x@mileage <- distGeo(p1 = coords[1L,], p2 = coords[2L,]) / 1609.34
  x@currency <- match.arg(tolower(x@currency), choices = c('usd', 'gbp'))
  return(x)
  
})



setMethod(f = show, signature = 'airfare', definition = \(object) {

  object@carrier |> 
    switch(
      ASold =, AS = 'Alaska Airlines\U1f1fa\U1f1f8',
      AA = 'American Airlines\U1f1fa\U1f1f8', 
      BAold =, BA = 'British Airways\U1f1ec\U1f1e7',
      CX = 'Cathay Pacific\U1f1ed\U1f1f0',
      JAL = 'Japan Airlines'
    ) |>
    sprintf(fmt = '%s\n') |> 
    cat()
  
  sprintf(fmt = '%s \u2708 %s\n', object@depart, object@arrive) |>
    cat()
  object@mileage |> 
    sprintf(fmt = '%.1f Miles\n') |> 
    cat()
  
  object@code |> 
    sprintf(fmt = 'Booking Code: %s\n') |> 
    cat()
  
  money <- switch(object@currency, usd = '\U1f4b5$', gbp = '\U1f4b7\u00a3')
  sprintf(fmt = 'Base Fare: %s%.2f\n', money, object@basefare) |> cat()
  sprintf(fmt = 'Carrier Imposed (YQ): %s%.2f\n', money, object@YQ) |> cat()
  sprintf(fmt = 'Carrier Imposed (YR): %s%.2f\n', money, object@YR) |> cat()
  sprintf(fmt = 'Tax: %s%.2f\n', money, object@tax) |> cat()
  sprintf(fmt = 'Upgrade Fee: %s%.2f\n', money, object@upgrade) |> cat()

})

