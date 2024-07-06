

#' @title Style URL
#' 
#' @param url_ \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param text_ (optional) \link[base]{character} scalar or \link[base]{vector}
#' 
#' @references
#' \url{https://github.com/rstudio/rstudio/issues/1941}
#' 
#' @examples
#' cat(styleURL(c('www.google.com', 'www.apple.com')), sep = '\n')
#' 
#' @importFrom cli style_hyperlink
#' @export
styleURL <- function(url_, text_) {
  
  n <- length(url_)
  if (!n) return(invisible())
  
  if (!is.character(url_) || anyNA(url_) || !all(nzchar(url_))) stop('illegal `url_`')
  id <- !grepl('^https://|^http://', x = url_)
  if (any(id)) url_[id] <- paste0('https://', url_[id])
  
  if (missing(text_) || !length(text_)) text_ <- gsub('^https://|^http://', replacement = '', x = url_)
  if (!is.character(text_) || length(text_) != n || anyNA(text_) || !all(nzchar(text_))) stop('illegal reference name')
  
  tmp <- style_hyperlink(text = text_, url = url_)
  return(unclass(tmp))

}