context("is_pbd_params")

test_that("use", {
  expect_true(is_pbd_params(x = create_pbd_params(0.1, 0.2, 0.3, 0.4, 0.5)))
  expect_false(is_pbd_params("nonsense"))
  expect_false(is_pbd_params(NULL))
  expect_false(is_pbd_params(NA))
})
