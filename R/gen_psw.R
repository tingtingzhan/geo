

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
#' @param symbol \link[base]{character} scalar or \link[base]{vector}, symbols allowed.  
#' Use `character()`, `''` or `NULL` to indicate no symbol allowed
#' 
#' @returns
#' Function [gen_psw] returns a \link[base]{character} scalar.
#' 
#' @examples
#' gen_psw(50L)
#' 
#' @export
gen_psw <- function(size = 40L, digit = TRUE, lower = TRUE, upper = TRUE, symbol = '~!@#$%^&*') {
  
  if (!is.integer(size) || length(size) != 1L || is.na(size) || size <= 1L) stop('`size` must be >=2L integer')
  
  x <- character()
  x_digit <- as.character(0:9)
  x_symbol <- setdiff(unlist(strsplit(symbol, split = '')), y = c(' '))
  if (digit) x <- c(x, x_digit)
  if (lower) x <- c(x, letters)
  if (upper) x <- c(x, LETTERS)
  x <- c(x, x_symbol) # !length(x_symbol) okay
  
  repeat {
    y <- sample(x, size = size, replace = TRUE)
    if (digit & !any(y %in% x_digit)) next
    if (lower & !any(y %in% letters)) next
    if (upper & !any(y %in% LETTERS)) next
    if (length(x_symbol) & !any(y %in% x_symbol)) next
    sq <- seq_len(length.out = size - 1L)
    if (any(y[sq] == y[sq + 1L])) next # do not allow same adjacent character
    break
  }
  
  return(paste(y, collapse = ''))
  
}