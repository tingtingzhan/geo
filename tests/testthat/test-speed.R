
# as.Date('2026-11-27')
x = array(rnorm(2e3L), dim = c(2L, 1e3L))
f1 = matrixStats::colMins
f2 = Rfast::colMins # much faster

expect_identical(
  object = f1(x), 
  expected = f2(x, value = TRUE)
)

library(microbenchmark)
expect_no_error(object = {
  microbenchmark(f1(x), f2(x, value = TRUE))  
})

# but \CRANpkg{RcppParallel} kills \CRANpkg{shiny}, dont know why


