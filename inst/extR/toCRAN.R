
library(adv.tzh) # devtools::install_github('tingtingzhan/adv.tzh')

if (FALSE) {
  dt = c(
    cli = '2024-06-21', 
    geosphere = '2022-11-13', 
    ggplot2 = '2024-04-22', 
    plotly = '2024-01-13', 
    leaflet = '2024-03-25', 
    scales = '2023-11-27', 
    # methods, # vanilla R
    sf = '2024-09-06'
  )
  stopifnot(identical(unname(as.Date(dt)), do.call(c, lapply(names(dt), packageDate))))
}


removeLocalPackage('iata')
checkDocument('.')
updateDESCRIPTION('.')
checkRelease('.')
