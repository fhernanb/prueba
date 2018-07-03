
test_that("counts are as expected", {
  set.seed(1974)
  x <- rpois(n=50, lambda=4.5)
  rta <- round(loglik(x, dist='dpois', param=list(lambda=4)), 1)
  expect_equal(rta, -106.4)

})

