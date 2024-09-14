
#' @importFrom methods setClass
setClass(Class = 'airfare', slots = c(
  carrier = 'character',
  depart = 'character',
  arrive = 'character',
  mileage = 'numeric',
  code = 'character',
  AA_status = 'character', # traveler's status with AA
  AS_status = 'character', # traveler's status with AS
  BA_status = 'character', # traveler's status with BA
  basefare = 'numeric',
  tax = 'numeric',
  carrier_imposed = 'numeric',
  upgrade = 'numeric'
), prototype = prototype(
  AA_status = 'member',
  AS_status = 'member',
  BA_status = 'blue'
))

# normalization
airfare <- function(object) {
  if (!length(object@mileage)) {
    trip <- IATA(paste(object@depart, object@arrive, sep = '-'))
    ap <- airports[trip[[1L]], , drop = FALSE]
    object@mileage <- c(distGeo_(ap@coords))
  }
  return(object)
}

#' @importFrom methods show
setMethod(f = show, signature = signature(object = 'airfare'), definition = function(object) {

  object <- airfare(object)
  
  cat('\n')
  
  cat(sprintf(fmt = '%s\n', switch(
    EXPR = object@carrier, 
    AS = 'Alaska Airlines\U0001f1fa\U0001f1f8',
    AA = 'American Airlines\U0001f1fa\U0001f1f8', 
    BA = 'British Airways\U0001f1ec\U0001f1e7')))
  
  cat(sprintf(fmt = '%s \u2708\ufe0f %s\n', object@depart, object@arrive))
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
  aim_id = 'integer',
  mileageplan = 'numeric', EQM = 'numeric', # Alaska (AS)
  aadvantage = 'numeric', loyaltypt = 'numeric', # American (AA)
  avios = 'numeric', tierpt = 'numeric' # British (BA)
))

#' @importFrom methods show
#' @importFrom cli.tzh styleURL
setMethod(f = show, signature = signature(object = 'loyalty'), definition = function(object) {
  
  cat(sprintf(fmt = '%s earns\n', switch(
    EXPR = object@program,
    AS = styleURL(text_ = 'Alaska Airlines\U0001f1fa\U0001f1f8', url_ = 'www.alaskaair.com/content/mileage-plan/how-to-earn-miles/airline-partners'),
    AA = styleURL(text_ = 'American Airlines\U0001f1fa\U0001f1f8', url_ = 'www.aa.com/i18n/travel-info/partner-airlines/american-airlines.jsp'),
    BA = styleURL(text_ = 'British Airways\U0001f1ec\U0001f1e7', url_ = 'www.britishairways.com/content/executive-club/avios/collecting-avios/flights')
  )))

  cat(sprintf(fmt = '\033[33m%d\033[0m Mileage Plan\n', object@mileageplan))
  
  cat(sprintf(fmt = '\033[33m%d\033[0m AAdvantage Miles\n', object@aadvantage))
  
  cat(sprintf(fmt = '\033[33m%d\033[0m Avios\n', object@avios))
  
  pt0 <- attributes(object)[c('EQM', 'loyaltypt', 'tierpt')]
  pt1 <- pt0[lengths(pt0, use.names = FALSE) > 0L]
  if (length(pt1) > 1L) stop('will not happen')
  pt <- sprintf(fmt = '%.1f%% %s', pt1[[1L]] / object@aim * 1e2, names(object@aim))
  if (length(object@aim_id)) {
    #pt[object@aim_id] <- paste0('\033[0;103;1;31m', pt[object@aim_id], '\033[22;34m')
    pt[object@aim_id] <- paste0('\033[1;31m', pt[object@aim_id], '\033[22;34m')
    if (length(object@tierpt)) pt[object@aim_id] <- paste0('\033[0;103m', pt[object@aim_id])
  }
  pt_txt <- paste0(pt, collapse = ', ')
  cat(sprintf(fmt = '\033[32m%d\033[0m EQMs \033[34m%s\033[0m\n', object@EQM, pt_txt))
  cat(sprintf(fmt = '\033[32m%d\033[0m Loyalty Points \033[34m%s\033[0m\n', object@loyaltypt, pt_txt))
  cat(sprintf(fmt = '\033[32m%d\033[0m Tier Points \033[34m%s\033[0m\n', object@tierpt, pt_txt))
  
})