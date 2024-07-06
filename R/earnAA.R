
earnAA <- function(airfare, status = c('member', 'gold', 'platinum', 'pro', 'executive')) {
  
  status <- match.arg(status)
  
  # For travel on American-marketed flights, 
  # miles are earned based on ticket price 
  # (includes base fare plus carrier-imposed fees; 
  # excludes government-imposed taxes and fees).
  
  fare <- round(sum(airfare@basefare, airfare@carrier_imposed))
  
  aadvantage <- fare * 5 * 
    (1 + switch(
      EXPR = status,
      member = 0,
      gold = .4,
      platinum = .6,
      pro = .8, 
      executive = 1.2))
  
  new(Class = 'loyalty', 
      program = 'AA', aadvantage = aadvantage, loyaltypt = aadvantage,
      aim = 1e3 * c(Gold = 40, Platinum = 75, Pro = 125, Executive = 200), aim_id = 2L)
  
}