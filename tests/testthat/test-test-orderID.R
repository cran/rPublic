library(testthat)
test_that("rp_getOrderId creates a valid id", {
  id <- rp_getOrderId()
  expect_type(id, "character")
})
