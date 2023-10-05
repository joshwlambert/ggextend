test_that("<bdts> plot method works as expected", {
  dat <- sim_data(t = 100, b = 10, d = 5)

  expect_silent(plot(dat))

  f <- function() plot(dat)
  vdiffr::expect_doppelganger(
    title = "epidist.plot",
    fig = f
  )
})
