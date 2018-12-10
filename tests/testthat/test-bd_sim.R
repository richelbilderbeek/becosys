context("bd_sim")

test_that("use", {

  spec_rate <- 0.1 # speciation rate
  ext_rate <- 0.05 # extinction rate
  crown_age <- 1

  set.seed(42)
  out <- bd_sim(
    bd_params = create_bd_params(spec_rate = spec_rate, ext_rate = ext_rate),
    crown_age = crown_age,
    conditioned_on = "non_extinction"
  )
})

test_that("abuse", {

  bd_params <- create_bd_params(0.1, 0.2)
  crown_age <- 15

  expect_error(
    bd_sim(
      bd_params = "nonsense",
      crown_age = crown_age
    ),
    "'bd_params' must be a valid BD parameter set"
  )
  expect_error(
    bd_sim(
      bd_params = bd_params,
      crown_age = -12.34
    ),
    "'crown_age' must be positive"
  )
  expect_error(
    bd_sim(
      bd_params = bd_params,
      stem_age = -12.34
    ),
    "'stem_age' must be positive"
  )
  expect_error(
    bd_sim(
      bd_params = bd_params,
      crown_age = NA,
      stem_age = NA
    ),
    "'crown_age' or 'stem_age' must be set"
  )
  expect_error(
    bd_sim(
      bd_params = bd_params,
      crown_age = 1.2,
      stem_age = 2.3
    ),
    "'crown_age' or 'stem_age' must be set exclusively"
  )
  expect_error(
    bd_sim(
      bd_params = bd_params,
      crown_age = 1.2,
      conditioned_on = "nonsense"
    ),
    "'conditioned_on' must be either 'nothing' or 'non_extinction'"
  )

})
