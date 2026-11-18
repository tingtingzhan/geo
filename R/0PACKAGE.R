

#' @note
#' This package is tested on Unix-like systems only!
#' 
'_PACKAGE'



.onLoad <- \(libname, pkgname) {
  
  if (.Platform$OS.type != 'unix') {
    pkgname |>
      sQuote() |>
      sprintf(fmt = 'Package %s is tested on Unix-like systems only!') |>
      stop()
  }
  
}