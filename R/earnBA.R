

#' @importFrom methods new
earnBA <- function(airfare, status = c('blue', 'bronze', 'silver', 'gold')) {
  
  status <- match.arg(status)
  
  switch(airfare@carrier, AA = {
    tierpt <- switch(
      EXPR = .bincode(airfare@mileage, breaks = c(0, 1e3, 2e3, 6e3, Inf), ), 
      # https://www.headforpoints.com/2024/02/01/how-many-british-airways-tier-points-do-i-earn-per-flight-3
      '1' = {
        NA_real_
      }, '2' = {
        NA_real_
      }, '3' = {
        switch(
          EXPR = airfare@code, 
          # https://www.britishairways.com/travel/flight-calculator/execclub/_gf/en_gb
          #B =, G =, N =, O =, Q = ,
          #K =, M =, L =, V =, S = ,
          #Y =, H =, P = ,
          #W =, 
          R =, I = , J =, D =, C = 140,
          NA_real_)
      }, '4' = {
        NA_real_
      }, NA_real_)
    
    # this is when fare money is not available to BA
    #avios <- round(round(airfare@mileage) * switch(
    #  EXPR = airfare@code, 
    #  # https://www.britishairways.com/travel/flight-calculator/execclub/_gf/en_gb
    #  B =, G =, N =, O =, Q = .25,
    #  K =, M =, L =, V =, S = .5,
    #  Y =, H =, P = 1,
    #  W =, R =, I = 1.5,
    #  J =, D =, C = 2.5,
    #  NA_real_))
    
    avios <- round(round(airfare@basefare * .79) * switch(status, blue = 6, bronze = 7, silver = 8, gold = 9))
    
  })
  
  new(Class = 'loyalty', 
      program = 'BA', avios = avios, tierpt = tierpt,
      aim = c(Bronze = 300, Silver = 600, Gold = 1500), aim_id = 2L) 
  
}