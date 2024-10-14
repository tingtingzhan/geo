
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
  
  cat(sprintf(fmt = '%s \u2708 %s\n', object@depart, object@arrive))
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
  
  aim_ <- switch(EXPR = x@program, AA = {
    c(Gold = 40e3, Platinum = 75e3, Pro = 125e3, Executive = 200e3)
  }, AS = {
    c(MVP = 20e3, Gold = 40e3, '75K' = 75e3, '100K' = 100e3)
  }, BA = {
    c(Bronze = 300, Silver = 600, Gold = 1500)
  })
  x@aim <- c(Member = 0, aim_)
  
  all_tier <- names(x@aim)
  n_tier <- length(all_tier)  
  
  if (length(x@reward) == 1L) x@reward <- rep(x@reward, times = n_tier)
  if (length(x@reward) != n_tier) stop('wrong')
  names(x@reward) <- all_tier
  
  if (length(x@status) == 1L) x@status <- rep(x@status, times = n_tier)
  if (length(x@status) != n_tier) stop('wrong')
  names(x@status) <- all_tier
  
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

  n_tier <- length(object@reward)
  prog_from_zero <- object@status[-n_tier] / object@aim[-1L]
  prog_from_current <- object@status[-n_tier] / diff(object@aim)
  ret <- rbind(
    'Rewards Earned' = object@reward, 
    'To Next Tier; from zero' = c(sprintf(fmt = '%.1f%%', 1e2 * prog_from_zero), '-'),
    'To Next Tier; from current' = c(sprintf(fmt = '%.1f%%', 1e2 * prog_from_current), '-')
  )
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



#' @importFrom ggplot2 autoplot ggplot theme_void
#' @importFrom grid unit
#' @export
autoplot.loyalty <- function(object, ...) {
  ggplot() + autolayer.loyalty(object, ...) + 
    theme_void()
}



#' @importFrom ggplot2 autolayer aes geom_rect scale_fill_manual coord_polar labs
#' @export
autolayer.loyalty <- function(object, ...) {
  n_tier <- length(object@aim)
  ymax <- object@aim[-1L]
  ymin <- object@aim[-n_tier]
  switch(object@program, AA =, AS =, BA = {
    # https://www.oneworld.com/travel-benefits
    # https://www.oneworld.com/members/alaska-airlines#tiers
    # https://www.oneworld.com/members/american-airlines#tiers
    alliance <- 'One World'
    tier_ <- structure(1:4, levels = c('Member', 'Ruby', 'Sapphire', 'Emerald'), class = 'factor')
    col_ <- c('grey90', '#ae001a', '#262896', '#004721')
    tier <- tier_[seq_along(ymax)]
    col <- col_[seq_along(ymax)]
  }, stop('next alliance'))
  
  list(
    geom_rect(mapping = aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = 0, fill = tier), alpha = .3, stat = 'identity', colour = 'white'),
    geom_rect(mapping = aes(ymax = ymin + object@status[-n_tier], ymin = ymin, xmax = 1, xmin = 0, fill = tier), stat = 'identity', colour = 'white'),
    scale_fill_manual(values = col, name = alliance),
    coord_polar(theta = 'y'),
    labs(
      title = switch(
        EXPR = object@program,
        AS = 'Alaska Airlines',
        AA = 'American Airlines',
        BA = 'British Airways',
      ))
  )
}


