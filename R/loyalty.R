

#' @title \linkS4class{loyalty} Progress
#' 
#' @slot program \link[base]{character} scalar, 
#' airline program to which the flight is credited 
#' (**not** carrier of the flight)
#' 
#' @slot goal \link[base]{numeric} \link[base]{vector},
#' milestone of each membership tier 
#' 
#' @slot reward \link[base]{numeric} \link[base]{vector},
#' reward miles earned for each membership tier
#' 
#' @slot status \link[base]{numeric} \link[base]{vector}
#' loyalty points earned for each membership tier
#' 
#' @slot creditcard \link[base]{character} scalar or \link[base]{vector},
#' co-branded credit card(s)
#' 
#' @examples
#' (JSM24 = new(Class = 'airfare', carrier = 'AA', depart = 'PHL', arrive = 'PDX',
#'   code = 'J', currency = 'usd', basefare = 276.28, tax = 35.82, upgrade = 241.88))
#' JSM24 |> earnAA()
#' JSM24 |> earnAS()
#' JSM24 |> earnBA()
#' # JSM24 |> earnBAold()
#' # JSM24 |> earnASold()
#' @name loyalty
#' @keywords internal
#' @aliases loyalty-class
#' @export
setClass(Class = 'loyalty', slots = c(
  program = 'character',
  goal = 'numeric',
  reward = 'numeric', status = 'numeric',
  creditcard = 'character'
))



#' @importFrom geosphere distGeo
setMethod(f = initialize, signature = 'loyalty', definition = \(.Object, ...) {
  
  x <- callNextMethod(.Object, ...)
  
  x@goal <- c(Member = 0, switch(EXPR = x@program, AA = {
    # https://www.aa.com/web/i18n/aadvantage-program/discover/member-statuses.html
    # https://www.citi.com/credit-cards/citi-aadvantage-executive-credit-card
    if ('citi' %in% x@creditcard) {
      c(Gold = 40e3, Platinum = (75-10)*1e3, 'Platinum Pro' = (125-20)*1e3, Executive = (200-20)*1e3)
    } else c(Gold = 40e3, Platinum = 75e3, 'Platinum Pro' = 125e3, Executive = 200e3)
  }, AS = {
    # https://www.alaskaair.com/atmosrewards
    c(Silver = 20e3, Gold = 40e3, Platinum = 80e3, Titanium = 135e3)
  }, ASold = {
    # https://www.alaskaair.com/content/mileage-plan/membership-benefits
    c(MVP = 20e3, Gold = 40e3, '75K' = 75e3, '100K' = 100e3)
  }, BA = {
    # https://www.britishairways.com/content/executive-club/faqs/introducing-the-british-airways-club
    c(Bronze = 3.5e3, Silver = 7.5e3, Gold = 20e3)
  }, BAold = {
    # https://www.britishairways.com/content/executive-club/about-the-club/tiers-and-benefits
    c(Bronze = 300, Silver = 600, Gold = 1500)
  }, CX = {
    # https://www.cathaypacific.com/cx/en_US/membership/status-points.html
    c(Silver = 300, Gold = 600, Diamond = 1200)
  }, JAL = {
    # https://www.oneworld.com/members/japan-airlines#tiers # confusing
    # https://www.jal.co.jp/en/oneworld/jmb_benefit.html # clear; use FLY ON
    c(Crystal = 30e3, Sapphire = 50e3, Premier = 80e3, Diamond = 100e3, Metal = 150e3)
  }))
  
  n <- length(x@goal)  
  
  if (length(x@reward) == 1L) x@reward <- rep(x@reward, times = n)
  if (length(x@reward) != n) stop('wrong')

  if (length(x@status) == 1L) x@status <- rep(x@status, times = n)
  if (length(x@status) != n) stop('wrong')

  return(x)
})



setMethod(f = show, signature = 'loyalty', definition = \(object) {
  
  if (FALSE) {
    switch(
      EXPR = object@program,
      ASold =, AS = style_hyperlink(text = 'Alaska Airlines\U1f1fa\U1f1f8', url = 'https://www.alaskaair.com/atmosrewards/content/partners/airlines'),
      AA = style_hyperlink(text = 'American Airlines\U1f1fa\U1f1f8', url = 'https://www.aa.com/i18n/travel-info/partner-airlines/american-airlines.jsp'),
      BAold =, BA = style_hyperlink(text = 'British Airways\U1f1ec\U1f1e7', url = 'https://www.britishairways.com/content/executive-club/avios/collecting-avios/flights')
    ) |> 
      unclass() |>
      cat('\n')
  }
  
  n <- length(object@goal)
  prog_zero <- object@status[-n] / object@goal[-1L]
  prog_current <- object@status[-n] / diff(object@goal)
  ret <- rbind(
    'Rewards Earned' = object@reward, 
    'To Next Tier; from zero' = c(sprintf(fmt = '%.1f%%', 1e2 * prog_zero), '-'),
    'To Next Tier; from current' = c('-', sprintf(fmt = '%.1f%%', 1e2 * prog_current[-1L]), '-')
  )
  colnames(ret) <- names(object@goal)
  names(dimnames(ret)) <- c('', 'Current Tier')
  # print(ret, quote = FALSE, right = TRUE) # really my figure is why much prettier!!!
  
  print(autoplot.loyalty(object))
  
})



#' @importFrom ggplot2 autoplot ggplot theme_void
#' @importFrom grid unit
#' @export
autoplot.loyalty <- function(object, ...) {
  ggplot() + 
    autolayer.loyalty(object, ...) + 
    theme_void()
}


# https://allancameron.github.io/geomtextpath/
# this is absolutely gorgeous!!!!!

#' @importFrom ggplot2 autolayer aes geom_rect ylim coord_polar
#' @importFrom geomtextpath geom_textpath
#' @export
autolayer.loyalty <- function(object, ...) {
  
  n <- length(object@goal)
  
  max_ <- object@goal[-1L]
  min_ <- object@goal[-n]
  ctr_ <- (min_+max_)/2
  
  switch(object@program, AA =, AS =, ASold =, BA =, BAold =, CX =, JAL = {
    # https://www.oneworld.com/travel-benefits
    # https://www.oneworld.com/members/alaska-airlines#tiers
    # https://www.oneworld.com/members/american-airlines#tiers
    # https://www.oneworld.com/members/british-airways#tiers
    # https://www.oneworld.com/members/cathay-pacific#tiers
    alliance <- 'oneworld'
    tier <- c('Member', 'Ruby', 'Sapphire', 'Emerald')
    col <- c('grey60', '#ae001a', '#262896', '#004721')
    airline_ <- switch(
      EXPR = object@program,
      ASold =, AS = 'Alaska Airlines',
      AA = 'American Airlines',
      BAold =, BA = 'British Airways',
      CX = 'Cathay Pacific',
      JAL = 'Japan Airlines',
      stop('next airline')
    )
  }, stop('next alliance'))
  
  if (length(tier) < n-1L) stop('should not happen')
  tier_ <- tier[seq_len(n-1L)]
  col_ <- col[seq_len(n-1L)]
  
  if (anyNA(object@status)) return(invisible())
  
  list(
    
    geom_rect(mapping = aes(xmin = min_, xmax = max_, ymin = .5, ymax = 1), fill = col_, alpha = .2, color = 'white'),
    
    geom_rect(mapping = aes(xmin = min_, xmax = min_ + object@status[-n], ymin = .5, ymax = 1), fill = col_, alpha = .5, color = 'white'),
    
    geom_textpath(mapping = aes(x = ctr_, y = 1.1, label = airline_), color = col_),
    
    geom_textpath(mapping = aes(x = ctr_, y = .9, label = names(min_)), color = col_),
    
    geom_textpath(mapping = aes(x = ctr_, y = .6, label = tier_), color = col_, fontface = 3),
    
    geom_textpath(mapping = aes(x = ctr_, y = .4, label = alliance), color = col_, fontface = 3),
    
    ylim(0, 1.2),
    
    coord_polar(theta = 'x')
      
  )
  
}


