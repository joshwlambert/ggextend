test_that("sim_data works as expected", {
  dat <- sim_data(t = 10, b = 1, d = 1)
  expect_s3_class(dat, class = "data.frame")
  expect_identical(dim(dat), c(11L, 2L))
  expect_named(dat, c("time", "system"))
})