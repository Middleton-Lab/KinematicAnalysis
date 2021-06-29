test_that("angle calculation works", {
  expect_equal(vector_angle(c(0, 0, 1), c(0, 1, 0)), pi / 2)
  expect_equal(vector_angle(c(0, 0, 1), c(0, 0, 1)), 0)
})
