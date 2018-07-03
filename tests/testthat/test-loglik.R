
test_that("counts are as expected", {
  set.seed(1974)
  x <- rpois(n=50, lambda=4.5)

  expect_equal(loglik(x, dist='dpois', param=list(lambda=4)), -106.4437)

})

