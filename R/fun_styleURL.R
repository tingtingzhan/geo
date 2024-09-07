

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
  
  url_ok <- grepl(pattern = '\033]8;', x = url_)
  if (all(url_ok)) return(url_)
  
  url__ <- url_[!url_ok]
  id <- !grepl('^https://|^http://', x = url__)
  if (any(id)) url__[id] <- paste0('https://', url__[id])
  
  if (missing(text_) || !length(text_)) {
    text_ <- gsub('^https://|^http://', replacement = '', x = url__)
  }
  if (!is.character(text_) || length(text_) != n || anyNA(text_) || !all(nzchar(text_))) stop('illegal reference name')
  
  url_[!url_ok] <- style_hyperlink(text = text_, url = url__)
  #return(unclass(url_))
  return(url_)

}