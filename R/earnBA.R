

#' @importFrom methods new
earnBA <- function(airfare) {
  
  # https://www.headforpoints.com/2024/02/01/how-many-british-airways-tier-points-do-i-earn-per-flight-3
  haul <- .bincode(airfare@mileage, breaks = c(0, 2e3, 6e3, Inf))
  # PHL-IAH, 1324.6 mile, lowest
  # PHL-PHX, 2075 mile, has a jump
  # PHL-ANC, 3379.0 mile, same
  # PHL-EZE, 5248.4 mile, same
  # PHL-NRT, 6755.3 mile, a jump
  
  # https://www.britishairways.com/travel/flight-calculator/execclub/_gf/en_gb
  # only use 'Tier Points'
  status <- switch(airfare@carrier, AA = {
    m <- array(c(
      5, 10, 20, 0, 0, 40, 40, 60, 60, # <2k miles
      20, 35, 70, 90, 90, 140, 140, 210, 210, # <6k miles
      20, 40, 80, 100, 100, 160, 160, 240, 240 # ?? upper limit?
    ), dim = c(9L, 2L))
    m[switch(
      EXPR = airfare@code, 
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
    #      EXPR = airfare@code, 
    #      Q =, O =, G = 20,
    #      K =, L =, M =, N =, S =, V = 35, 
    #      Y =, B =, H = 70, 
    #      R =, I = , J =, C =, D = 140,
    #      NA_real_)
    
  })

  # this is when fare money is not available to BA
  #reward <- round(round(airfare@mileage) * switch(
  #  EXPR = airfare@code, 
  #  # https://www.britishairways.com/travel/flight-calculator/execclub/_gf/en_gb
  #  B =, G =, N =, O =, Q = .25,
  #  K =, M =, L =, V =, S = .5,
  #  Y =, H =, P = 1,
  #  W =, R =, I = 1.5,
  #  J =, D =, C = 2.5,
  #  NA_real_))
  
  # .79 is exchange rate between GBP and USD
  # https://www.britishairways.com/content/executive-club/avios/collecting-avios/flights
  reward <- round(round(airfare@basefare * .79) * c(
    Blue = 6, 
    Bronze = 7, 
    Silver = 8, 
    Gold = 9
  ))
  
  return(new(Class = 'loyalty', program = 'BA', reward = reward, status = status))
  
}