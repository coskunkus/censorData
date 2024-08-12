library(testthat) # load testthat package
library(censorData) # load our package
test_that("functions returns a list", {
  expect_type(pcdata(10,rep(1,10),"qweibull",c(1,2)), "double")
})
# ## Test whether the code throw an error.
test_that("functions returns errors", {
  expect_error(pcdata(9,rep(1,10),"qweibull",c(1,2)),
               "dimension of r must be equal to m")
})
# ## Test whether the output is a vector with the expected size
test_that("functions returns a  vector with the expected size", {
  expect_vector(pcdata(10,rep(1,10),"qweibull",c(1,2)),
                ptype = double(), size = 10)
})

