

#' @title Transform All Geometry Columns
#' 
#' @param x `'sf'`
#' 
#' @param crs,... parameters of the function \link[sf]{st_transform}
#' 
#' @importFrom sf st_geometry st_geometry<- st_transform
#' @export
st_transform_all <- \(x, crs = 4326, ...) {
  
  cls <- x |>
    vapply(FUN = inherits, what = 'sfc', FUN.VALUE = NA)
  
  geo_orig <- attr(x, which = 'sf_column', exact = TRUE)
  
  if (sum(cls) == 1L) {
    
    x <- x |>
      st_transform(x = _, crs = crs, ...)
    
  } else {
    
    for (i in names(which(cls))) {
      st_geometry(x) <- i
      x <- x |>
        st_transform(x = _, crs = crs, ...)
    }
    
    st_geometry(x) <- geo_orig
    
  }
  
  return(x)
  
}