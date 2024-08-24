

earnAS <- function(airfare) {
  
  switch(airfare@carrier, AA = {
    
    # https://www.alaskaair.com/content/mileage-plan/how-to-earn-miles/airline-partners/american-airlines?lid=airline-partners:partners-american
    mileageplan <- round(round(airfare@mileage) * switch(
      EXPR = airfare@code, 
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
  
  return(new(
    Class = 'loyalty', 
    program = 'AS', 
    mileageplan = mileageplan, 
    EQM = mileageplan,
    aim = 1e3 * c(MVP = 20, Gold = 40, '75K' = 75, '100K' = 100), 
    aim_id = 2L
  ))
  
}