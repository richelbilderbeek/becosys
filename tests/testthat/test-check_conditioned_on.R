test_that("use", {
  expect_silent(check_conditioned_on("nothing"))
  expect_silent(check_conditioned_on("non_extinction"))
  expect_error(check_conditioned_on(""))
  expect_error(check_conditioned_on("nonsense"))
  expect_error(check_conditioned_on(NULL))
  expect_error(check_conditioned_on(NA))
  expect_error(check_conditioned_on(Inf))
  expect_error(check_conditioned_on(c("nothing", "nothing")))

})
