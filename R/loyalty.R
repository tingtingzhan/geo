

#' @title \linkS4class{loyalty}
#' 
#' @slot program \link[base]{character} scalar, airline program to which the flight is credited (**not** carrier of the flight)
#' 
#' @slot goal \link[base]{numeric} \link[base]{vector}
#' 
#' @slot reward \link[base]{numeric} \link[base]{vector}
#' 
#' @slot status \link[base]{numeric} \link[base]{vector}
#' 
#' @examples
#' (JSM24 = new(Class = 'airfare', carrier = 'AA', depart = 'PHL', arrive = 'PDX',
#'   code = 'J', basefare = 276.28, tax = 35.82, upgrade = 241.88))
#' earnAA(JSM24)
#' earnAS(JSM24)
#' earnBA(JSM24)
#' @name loyalty
#' @aliases loyalty-class
#' @export
setClass(Class = 'loyalty', slots = c(
  program = 'character',
  goal = 'numeric',
  reward = 'numeric', status = 'numeric'
))



#' @importFrom methods setMethod callNextMethod initialize
#' @importFrom geosphere distGeo
setMethod(f = initialize, signature = 'loyalty', definition = function(.Object, ...) {
  
  x <- callNextMethod(.Object, ...)
  
  x@goal <- c(Member = 0, switch(EXPR = x@program, AA = {
    # https://www.aa.com/web/i18n/aadvantage-program/discover/member-statuses.html
    c(Gold = 40e3, Platinum = 75e3, Pro = 125e3, Executive = 200e3)
  }, AS = {
    # https://www.alaskaair.com/content/mileage-plan/membership-benefits
    c(MVP = 20e3, Gold = 40e3, '75K' = 75e3, '100K' = 100e3)
  }, BA = {
    # https://www.britishairways.com/content/executive-club/about-the-club/tiers-and-benefits
    c(Bronze = 300, Silver = 600, Gold = 1500)
  }))
  
  n <- length(x@goal)  
  
  if (length(x@reward) == 1L) x@reward <- rep(x@reward, times = n)
  if (length(x@reward) != n) stop('wrong')

  if (length(x@status) == 1L) x@status <- rep(x@status, times = n)
  if (length(x@status) != n) stop('wrong')

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
  
  n <- length(object@goal)
  prog_zero <- object@status[-n] / object@goal[-1L]
  prog_current <- object@status[-n] / diff(object@goal)
  ret <- rbind(
    'Rewards Earned' = object@reward, 
    'To Next Tier; from zero' = c(sprintf(fmt = '%.1f%%', 1e2 * prog_zero), '-'),
    'To Next Tier; from current' = c('-', sprintf(fmt = '%.1f%%', 1e2 * prog_current[-1L]), '-')
  )
  colnames(ret) <- names(object@goal)
  names(dimnames(ret)) <- c(#unclass(switch(
    #  EXPR = object@program,
    #  AS = 'Alaska Airlines (AS)',
    #  AA = 'American Airlines (AA)',
    #  BA = 'British Airways (BA)',
    #)), 
    '',
    'Current Tier')
  print(ret, quote = FALSE, right = TRUE)
  
  print(autoplot.loyalty(object))
  
})



#' @importFrom ggplot2 autoplot ggplot theme_void
#' @importFrom grid unit
#' @export
autoplot.loyalty <- function(object, ...) {
  ggplot() + autolayer.loyalty(object, ...) + theme_void()
}



#' @importFrom ggplot2 autolayer aes geom_rect scale_fill_manual coord_polar labs
#' @export
autolayer.loyalty <- function(object, ...) {
  n <- length(object@goal)
  ymax <- object@goal[-1L]
  ymin <- object@goal[-n]
  switch(object@program, AA =, AS =, BA = {
    # https://www.oneworld.com/travel-benefits
    # https://www.oneworld.com/members/alaska-airlines#tiers
    # https://www.oneworld.com/members/american-airlines#tiers
    # https://www.oneworld.com/members/british-airways#tiers
    alliance <- 'One World'
    tier <- structure(1:4, levels = c('Member', 'Ruby', 'Sapphire', 'Emerald'), class = 'factor')
    col <- c('grey90', '#ae001a', '#262896', '#004721')
  }, stop('next alliance'))
  
  if (length(tier) < n-1L) stop('should not happen')
  sq <- seq_len(n-1L)
  
  list(
    geom_rect(mapping = aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = 0, fill = tier[sq]), alpha = .3, stat = 'identity', colour = 'white'),
    geom_rect(mapping = aes(ymax = ymin + object@status[-n], ymin = ymin, xmax = 1, xmin = 0, fill = tier[sq]), stat = 'identity', colour = 'white'),
    scale_fill_manual(values = col[sq], name = alliance),
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


