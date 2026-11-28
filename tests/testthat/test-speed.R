
if (FALSE) {
  # as.Date('2026-11-27')
  library(microbenchmark)
  x = array(rnorm(2e3L), dim = c(2L, 1e3L))
  f1 = matrixStats::colMins
  f2 = Rfast::colMins # much faster
  stopifnot(identical(f1(x), f2(x, value = TRUE)))
  microbenchmark(f1(x), f2(x, value = TRUE))
}

