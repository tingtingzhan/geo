

#' @title Credit a Flight to an Airline in oneworld Alliance
#' 
#' @param x an \linkS4class{airfare}
#' 
#' @param creditcard slot of class \linkS4class{loyalty}
#' 
#' @keywords internal
#' @name earnOneWorld
#' @export
earnAA <- function(x, creditcard = 'citi') {
  
  switch(EXPR = x@carrier, AA = {
    # https://www.aa.com/web/i18n/aadvantage-program/earn-miles/american-airlines-flights.html
    fare <- round(sum(x@basefare, x@YQ, x@YR))
    reward <- fare * 5 * (1 + c(Member = 0, Gold = .4, Platinum = .6, Pro = .8, Executive = 1.2))
  })
  
  return(new(Class = 'loyalty', program = 'AA', reward = reward, status = reward, creditcard = creditcard))
  
}


#' @rdname earnOneWorld
#' @export
earnAS <- function(x) {
  
  #switch(x@carrier, AA = { # does carrier matter?
    # https://www.alaskaair.com/atmosrewards
    # https://www.uscreditcardguide.com/alaska-announces-atmos-rewards-program/
    reward <- max(
      round(x@mileage),
      round(sum(x@basefare, x@YQ, x@YR)) * 5,
      500
    )

  #})
  
  return(new(Class = 'loyalty', program = 'AS', reward = reward, status = reward))
  
}







#' @rdname earnOneWorld
#' @export
earnASold <- function(x) {
  
  switch(x@carrier, AA = {
    # https://www.alaskaair.com/content/mileage-plan/how-to-earn-miles/airline-partners/american-airlines?lid=airline-partners:partners-american
    reward <- round(round(x@mileage) * switch(
      EXPR = x@code, 
      O =, Q =, B = .25,
      N =, S = .5,
      G =, V = .75,
      H =, K =, L =, M =, Y = 1, 
      P = 1,
      W = 1.1, 
      C = NA_real_, # C (Upgrade)	Earning is based on original booking
      D =, R =, I = 1.5,
      J = 2,
      X = NA_real_, # X (Upgrade)	Earning is based on original booking
      A = 1.5,
      F = 2,
      NA_real_))
  })
  
  return(new(Class = 'loyalty', program = 'ASold', reward = reward, status = reward))
  
}









#' @rdname earnOneWorld
#' @export
earnBA <- function(x) {
  
  # https://www.britishairways.com/content/executive-club/faqs/introducing-the-british-airways-club
  # We'll award 1 Tier Point for 1 pound (£) spent on:
  # The base fare on any commercial British Airways, American Airlines and Iberia marketed flights
  # The Carrier Imposed Charges (YQ and YR) on any commercial British Airways, American Airlines and Iberia marketed flights
  # Cabin upgrades made online or at the airport on any British Airways marketed and operated flights
  # Additional baggage pre-purchased online, through our Contact Centres or at the airport on any British Airways or Iberia marketed and operated flights
  # Pre-paid seating charges on any British Airways or Iberia marketed and operated flights  
  
  # .79 is exchange rate between GBP and USD
  status <- switch(x@carrier, AA = {
    if (x@currency != 'usd') stop('really?')
    round(sum(x@basefare, sum(x@YQ), sum(x@YR)) * .79)
    
  }, BA = {
    NA_real_
    #switch(
    #      EXPR = x@code, 
    #      Q =, O =, G = 20,
    #      K =, L =, M =, N =, S =, V = 35, 
    #      Y =, B =, H = 70, 
    #      R =, I = , J =, C =, D = 140,
    #      NA_real_)
    
  })
  
  # how is this changed after 2025?
  reward <- round(round(x@basefare * .79) * c(Blue = 6, Bronze = 7, Silver = 8, Gold = 9))
  
  return(new(Class = 'loyalty', program = 'BA', reward = reward, status = status))
  
}



earnBAold <- function(x) {
  
  # https://www.headforpoints.com/2024/02/01/how-many-british-airways-tier-points-do-i-earn-per-flight-3
  haul <- .bincode(x@mileage, breaks = c(0, 2e3, 6e3, Inf))
  # PHL-IAH, 1324.6 mile, lowest
  # PHL-PHX, 2075 mile, has a jump
  # PHL-ANC, 3379.0 mile, same
  # PHL-EZE, 5248.4 mile, same
  # PHL-NRT, 6755.3 mile, a jump
  
  # https://www.britishairways.com/travel/flight-calculator/execclub/_gf/en_gb
  # only use 'Tier Points'
  status <- switch(x@carrier, AA = {
    m <- array(c(
      5, 10, 20, 0, 0, 40, 40, 60, 60, # <2k miles
      20, 35, 70, 90, 90, 140, 140, 210, 210, # <6k miles
      20, 40, 80, 100, 100, 160, 160, 240, 240 # ?? upper limit?
    ), dim = c(9L, 2L))
    m[switch(
      EXPR = x@code, 
      B =, G =, N =, O =, Q = 1L, # 'Economy Lowest',
      K =, M =, L =, V =, S = 2L, # 'Economy Low', 
      Y =, H = 3L, # 'Economy Flexible', 
      P = 4L, # 'Premium Economy Lowest', 
      W = 5L, # 'Premium Economy Flexible',
      R =, I = 6L, # 'Business Lowest', 
      J =, C =, D = 7L, # 'Business Flexible',
      A = 8L, # 'First Lowest', 
      F = 9L # 'First Flexible'
    ), haul]
    
  }, BA = {
    NA_real_
    #switch(
    #      EXPR = x@code, 
    #      Q =, O =, G = 20,
    #      K =, L =, M =, N =, S =, V = 35, 
    #      Y =, B =, H = 70, 
    #      R =, I = , J =, C =, D = 140,
    #      NA_real_)
    
  })

  # this is when fare money is not available to BA
  #reward <- round(round(x@mileage) * switch(
  #  EXPR = x@code, 
  #  # https://www.britishairways.com/travel/flight-calculator/execclub/_gf/en_gb
  #  B =, G =, N =, O =, Q = .25,
  #  K =, M =, L =, V =, S = .5,
  #  Y =, H =, P = 1,
  #  W =, R =, I = 1.5,
  #  J =, D =, C = 2.5,
  #  NA_real_))
  
  # .79 is exchange rate between GBP and USD
  # https://www.britishairways.com/content/executive-club/avios/collecting-avios/flights
  # 'If you pay in another currency, the amount you’ve spent is converted to GBP using the IATA 5-day exchange rate that applied on the day of your purchase. We’ll use this converted amount to calculate your Avios.'
  reward <- round(round(x@basefare * .79) * c(Blue = 6, Bronze = 7, Silver = 8, Gold = 9))
  
  return(new(Class = 'loyalty', program = 'BAold', reward = reward, status = status))
  
}



#' @rdname earnOneWorld
#' @export
earnCX <- function(x) {
  
  switch(EXPR = x@carrier, AA = {
    # https://www.asiamiles.com/en/earn-miles/airlines/detail.html/american-airlines
    reward <- round(round(x@mileage) * switch(
      EXPR = x@code, 
      #O =, B = ???,
      G =, N =, S =, Q = .25,
      H =, K =, L =, M =, V = .75,
      Y = 1, 
      P = 1,
      W = 1.1, 
      J =, C =, D =, R =, I = {
        id <- match(c(x@depart, x@arrive), table = airports_ip2location@data$iata, nomatch = NA_integer_)
        country_ <- airports_ip2location@data[id, 'country_code']
        if (all(country_ %in% 'US')) 1.5 else 1.25
      },
      A =, F = 1.5,
      NA_real_))
  })
  
  # https://www.cathaypacific.com/acc/en/frequent-flyers/earning-club-points-and-asia-miles.html
  # CX does not publish the actual rule of accumulating status point
  
  return(new(Class = 'loyalty', program = 'CX', reward = reward, status = NA_real_))
  
}


#' @rdname earnOneWorld
#' @export
earnJAL <- function(x) {
  
  switch(EXPR = x@carrier, AA = {
    # https://www.jal.co.jp/en/jmb/earn/travel/flight/aa.html
    reward <- round(round(x@mileage) * switch(
      EXPR = x@code, 
      #B = ???,
      O =, Q = .3,
      L =, V =, S =, N =, G = .5,
      H =, K =, M = .7,
      Y = 1, 
      W =, P = 1,
      J =, C =, D =, R =, I = 1.25,
      A =, F = 1.5,
      NA_real_))
  })
  
  # https://www.jal.co.jp/jp/en/jalmile/flyon/guide.html
  # not clear how FLY ON points are calculated..
  
  return(new(Class = 'loyalty', program = 'JAL', reward = reward, status = NA_real_))
  
}