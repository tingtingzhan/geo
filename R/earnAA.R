
earnAA <- function(airfare) {
  
  switch(EXPR = airfare@carrier, AA = {
    
    # For travel on American-marketed flights, 
    # miles are earned based on ticket price 
    # (includes base fare plus carrier-imposed fees; 
    # excludes government-imposed taxes and fees).
    # https://www.aa.com/web/i18n/aadvantage-program/earn-miles/american-airlines-flights.html
    
    fare <- round(sum(airfare@basefare, airfare@carrier_imposed))
    
    aadvantage <- fare * 5 * 
      (1 + switch(
        EXPR = match.arg(airfare@AA_status, choices = c('member', 'gold', 'platinum', 'pro', 'executive')),
        member = 0,
        gold = .4,
        platinum = .6,
        pro = .8, 
        executive = 1.2))
    
  })
  
  return(new(
    Class = 'loyalty', 
    program = 'AA', 
    aadvantage = aadvantage, 
    loyaltypt = aadvantage,
    aim = 1e3 * c(Gold = 40, Platinum = 75, Pro = 125, Executive = 200), 
    aim_id = 2L
  ))

}