


#' @title Credit a Flight to American Airlines
#' 
#' @param airfare ..
#' 
#' @importFrom methods new
#' @export
earnAA <- function(airfare) {
  
  switch(EXPR = airfare@carrier, AA = {
    # AA to AA
    # https://www.aa.com/web/i18n/aadvantage-program/earn-miles/american-airlines-flights.html
    fare <- round(sum(airfare@basefare, airfare@carrier_imposed))
    reward <- fare * 5 * (1 + c(
      Member = 0,
      Gold = .4,
      Platinum = .6,
      Pro = .8, 
      Executive = 1.2
    ))
  })
  
  return(new(Class = 'loyalty', program = 'AA', reward = reward, status = reward))

}