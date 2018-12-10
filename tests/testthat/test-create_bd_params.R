context("test-create_bd_params")

context("create_bd_params")

test_that("use", {

  spec_rate <- 0.1
  ext_rate <- 0.2

  params <- create_bd_params(
    spec_rate = spec_rate,
    ext_rate = ext_rate
  )
  expect_equal(spec_rate, params$spec_rate)
  expect_equal(ext_rate, params$ext_rate)

})

test_that("abuse", {

  expect_error(
    create_bd_params(
      ext_rate = -12.34,
      spec_rate = 1.0
    ),
    "'ext_rate' must be positive"
  )
  expect_error(
    create_bd_params(
      ext_rate = 1.0,
      spec_rate = -12.34
    ),
    "'spec_rate' must be positive"
  )
})
