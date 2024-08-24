

#' @title Generate Strong Password
#' 
#' @param size \link[base]{integer} scalar, number of characters allowed.  Default `40L`
#' 
#' @param digit \link[base]{logical} scalar, whether digits `0-9` are allowed. Default `TRUE`
#' 
#' @param lower \link[base]{logical} scalar, whether lower case characters `a-z` are allowed. Default `TRUE`
#' 
#' @param upper \link[base]{logical} scalar, whether upper case characters `A-Z` are allowed. Default `TRUE`
#' 
#' @param symbol \link[base]{logical} scalar, whether commonly used symbols `~!@#$%^&*<>?:;` are allowed. Default `TRUE`
#' 
#' @export
gen_psw <- function(size = 40L, digit = TRUE, lower = TRUE, upper = TRUE, symbol = TRUE) {
  
  x <- character()
  x_digit <- as.character(0:9)
  x_symbol <- c('~', '!', '@', '#', '$', '%', '^', '&', '*', '<', '>', '?', ':', ';')
  if (digit) x <- c(x, x_digit)
  if (lower) x <- c(x, letters)
  if (upper) x <- c(x, LETTERS)
  if (symbol) x <- c(x, x_symbol)
  
  repeat {
    y <- sample(x, size = size, replace = TRUE)
    if (digit & !any(y %in% x_digit)) next
    if (lower & !any(y %in% letters)) next
    if (upper & !any(y %in% LETTERS)) next
    if (symbol & !any(y %in% x_symbol)) next
    break
  }
  
  return(paste(y, collapse = ''))
  
}