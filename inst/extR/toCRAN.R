
library(adv.tzh) # devtools::install_github('tingtingzhan/adv.tzh')

file.copy(from = file.path('../.', 'tzh', 'R', 
                           c('lower_n_vanilla.R')), 
          to = file.path('../.', 'IATA', 'R'), overwrite = TRUE)

file.copy(from = file.path('../.', 'cooking', 'R', 
                           c('fun_styleURL.R')), 
          to = file.path('../.', 'IATA', 'R'), overwrite = TRUE)


removeLocalPackage('iata')
checkDocument('.')
updateDESCRIPTION('.')
checkRelease('.')
