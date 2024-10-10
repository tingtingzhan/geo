
#' @importFrom methods setClass
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



#' @importFrom methods setMethod callNextMethod initialize
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



#' @importFrom methods setMethod show
setMethod(f = show, signature = 'airfare', definition = function(object) {

  cat('\n')
  
  cat(sprintf(fmt = '%s\n', switch(
    EXPR = object@carrier, 
    AS = 'Alaska Airlines\U0001f1fa\U0001f1f8',
    AA = 'American Airlines\U0001f1fa\U0001f1f8', 
    BA = 'British Airways\U0001f1ec\U0001f1e7')))
  
  cat(sprintf(fmt = '%s \u2708\ufe0f %s\n', object@depart, object@arrive))
  cat(sprintf(fmt = '%.1f Miles\n', object@mileage))
  
  cat(sprintf(fmt = 'Booking Code: \033[0;103m%s\033[0m\n', object@code))
  
  cat(sprintf(fmt = 'Base Fare: $%.2f\n', object@basefare))
  cat(sprintf(fmt = 'Carrier Imposed Fees: $%.2f\n', object@carrier_imposed))
  cat(sprintf(fmt = 'Tax: $%.2f\n', object@tax))
  cat(sprintf(fmt = 'Upgrade Fee: $%.2f\n', object@upgrade))

  cat('\n')
  show(earnAS(object))
  cat('\n')
  show(earnAA(object))
  cat('\n')
  show(earnBA(object))
  
  cat('\n')
  
})












setClass(Class = 'loyalty', slots = c(
  program = 'character',
  aim = 'numeric',
  reward = 'numeric', status = 'numeric'
))

#' @importFrom methods setMethod callNextMethod initialize
#' @importFrom geosphere distGeo
setMethod(f = initialize, signature = 'loyalty', definition = function(.Object, ...) {
  x <- callNextMethod(.Object, ...)
  x@aim <- switch(EXPR = x@program, AA = {
    1e3 * c(Gold = 40, Platinum = 75, Pro = 125, Executive = 200)
  }, AS = {
    1e3 * c(MVP = 20, Gold = 40, '75K' = 75, '100K' = 100)
  }, BA = {
    c(Bronze = 300, Silver = 600, Gold = 1500)
  })
  return(x)
})



#' @importFrom methods setMethod show
#' @importFrom cli style_hyperlink
setMethod(f = show, signature = 'loyalty', definition = function(object) {
  
  cat(unclass(switch(
    EXPR = object@program,
    AS = style_hyperlink(text = 'Alaska Airlines\U0001f1fa\U0001f1f8', url = 'https://www.alaskaair.com/content/mileage-plan/how-to-earn-miles/airline-partners'),
    AA = style_hyperlink(text = 'American Airlines\U0001f1fa\U0001f1f8', url = 'https://www.aa.com/i18n/travel-info/partner-airlines/american-airlines.jsp'),
    BA = style_hyperlink(text = 'British Airways\U0001f1ec\U0001f1e7', url = 'https://www.britishairways.com/content/executive-club/avios/collecting-avios/flights')
  )), '\n')

  all_tier <- c('Member', names(object@aim))
  n_tier <- length(all_tier)  
  
  if (length(object@reward) == 1L) {
    reward <- rep(object@reward, times = n_tier)
  } else {
    if (length(object@reward) != n_tier) stop('wrong')
    reward <- object@reward
  }
  names(reward) <- all_tier
  
  #cat('Progress to Next Tier\n')
  if (length(object@status) == 1L) {
    status <- rep(object@status, times = n_tier)
  } else {
    if (length(object@status) != n_tier) stop('wrong')
    status <- object@status
  }
  prog <- sprintf(fmt = '%.1f%%', 1e2 * status[-n_tier] / object@aim)
  ret <- rbind('Rewards Earned' = reward, 'Progress to Next Tier' = c(prog, '-'))
  names(dimnames(ret)) <- c(#unclass(switch(
  #  EXPR = object@program,
  #  AS = 'Alaska Airlines (AS)',
  #  AA = 'American Airlines (AA)',
  #  BA = 'British Airways (BA)',
  #)), 
    '',
    'Current Tier')
  print(ret, quote = FALSE, right = TRUE)
  
})
