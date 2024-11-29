

#' @title \linkS4class{airfare}
#' 
#' @slot carrier \link[base]{character} scalar
#' 
#' @slot depart \link[base]{character} scalar
#' 
#' @slot arrive \link[base]{character} scalar
#' 
#' @slot mileage \link[base]{double} scalar
#' 
#' @slot code \link[base]{character} scalar
#' 
#' @slot basefare,tax,carrier_imposed,upgrade \link[base]{double} scalar
#' 
#' @name airfare
#' @aliases airfare-class
#' @export
setClass(Class = 'airfare', slots = c(
  carrier = 'character',
  depart = 'character',
  arrive = 'character',
  mileage = 'numeric',
  code = 'character',
  basefare = 'numeric',
  tax = 'numeric',
  carrier_imposed = 'numeric',
  upgrade = 'numeric'
))



#' @importFrom geosphere distGeo
setMethod(f = initialize, signature = 'airfare', definition = function(.Object, ...) {
  x <- callNextMethod(.Object, ...)
  id <- match(c(x@depart, x@arrive), table = airports_ip2location@data$iata, nomatch = NA_integer_)
  if (anyNA(id)) stop('unknown departure and/or arrival airport')
  coords <- airports_ip2location@coords[id,]
  print(turn_coords(list(coords)))
  if (length(x@mileage)) warning('Do not manully put in @mileage !!')
  x@mileage <- distGeo(p1 = coords[1L,], p2 = coords[2L,]) / 1609.34
  return(x)
})



setMethod(f = show, signature = 'airfare', definition = function(object) {

  cat(sprintf(fmt = '%s\n', switch(
    EXPR = object@carrier, 
    AS = 'Alaska Airlines\U1f1fa\U1f1f8',
    AA = 'American Airlines\U1f1fa\U1f1f8', 
    BA = 'British Airways\U1f1ec\U1f1e7',
    CX = 'Cathay Pacific\U1f1ed\U1f1f0',
    JAL = 'Japan Airlines')))
  
  cat(sprintf(fmt = '%s \u2708 %s\n', object@depart, object@arrive))
  cat(sprintf(fmt = '%.1f Miles\n', object@mileage))
  
  cat(sprintf(fmt = 'Booking Code: %s\n', bg_br_yellow(object@code)))
  
  cat(sprintf(fmt = 'Base Fare: $%.2f\n', object@basefare))
  cat(sprintf(fmt = 'Carrier Imposed Fees: $%.2f\n', object@carrier_imposed))
  cat(sprintf(fmt = 'Tax: $%.2f\n', object@tax))
  cat(sprintf(fmt = 'Upgrade Fee: $%.2f\n', object@upgrade))

})

